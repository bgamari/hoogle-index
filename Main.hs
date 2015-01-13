{-# LANGUAGE TupleSections #-}

import Control.Monad (void, forM, when, forever)
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Applicative
import Data.Version
import Data.Maybe (listToMaybe)
import Data.Ord (comparing)
import Data.List (sortBy)
import System.Directory
import System.Exit
import System.Process
import System.FilePath
import System.IO

import qualified Data.Set as S

import Data.Either (partitionEithers)
import Control.Error

import Options.Applicative

import qualified Data.ByteString.Char8 as BS
import System.IO.Temp

import Distribution.Verbosity (Verbosity, normal, verbose)
import Distribution.Simple.Compiler (Compiler (compilerId), compilerFlavor)
import Distribution.Simple.GHC
import Distribution.Simple.Program (defaultProgramConfiguration)
import Distribution.Simple.Compiler (PackageDB (..))
import Distribution.Simple.PackageIndex ( PackageIndex, lookupPackageName
                                        , reverseTopologicalOrder
                                        , searchByNameSubstring
                                        )
import Distribution.System (buildPlatform)
import Distribution.InstalledPackageInfo (InstalledPackageInfo_ (..), InstalledPackageInfo)
import Distribution.Package (PackageId, PackageName (..), PackageIdentifier (..))
import qualified Distribution.Simple.InstallDirs as IDirs

import qualified Paths_hoogle_index
import Data.Version

import Hoogle (defaultDatabaseLocation)

-- | Various configuration
data Config = Config { verbosity               :: Verbosity
                     , installTextBase         :: Bool
                     , useLocalDocs            :: Bool
                     , ignoreExistingTextBases :: Bool
                     , otherPackageDbs         :: [PackageDB]
                     }

opts =
    Config <$> flag normal verbose
               ( short 'v' <> long "verbose"
              <> help "Enable verbose output"
               )
           <*> switch
               ( short 'i' <> long "install"
              <> help "Install generated textbases for future use"
               )
           <*> switch
               ( short 'l' <> long "local"
              <> help "Use local Haddock documentation when available"
               )
           <*> switch
               ( long "ignore-existing"
              <> help "Always regenerate textbases even if one already exists"
               )
           <*> many (option (SpecificPackageDB <$> str)
               ( short 'f' <> long "package-db"
              <> help "Add an addition package database (e.g. a Cabal sandbox)"
               ))

-- | An unpacked Cabal project
newtype PackageTree = PkgTree FilePath
                    deriving (Show)

-- | Call a process
callProcessE :: Config -> FilePath -> [String] -> EitherT String IO ()
callProcessE ver = callProcessIn ver "."

-- | Call a process from the given directory
callProcessIn :: Config
              -> FilePath    -- ^ directory
              -> FilePath    -- ^ executable
              -> [String]    -- ^ arguments
              -> EitherT String IO ()
callProcessIn cfg dir exec args = do
    let ver = verbosity cfg
        stream
          | verbose < ver = CreatePipe
          | otherwise     = Inherit
    let cp = (proc exec args) { cwd = Just dir
                              , std_err = stream
                              , std_out = stream
                              }
    (_, hOut, hErr, ph) <- tryIO' $ createProcess cp

    let handleStream (Just h) = liftIO $ void $ forkIO $ forever $ hGetLine h
        handleStream Nothing  = return ()
    handleStream hOut
    handleStream hErr
    code <- liftIO $ waitForProcess ph
    case code of
      ExitSuccess -> return ()
      ExitFailure e -> left $ "Process "++exec++" failed with error "++show e

-- | Unpack a package
unpack :: Config -> PackageId -> EitherT String IO PackageTree
unpack cfg (PackageIdentifier (PackageName pkg) ver) = do
    tmpDir <- liftIO getTemporaryDirectory
    dir <- liftIO $ createTempDirectory tmpDir "hoogle-index.pkg"
    callProcessIn cfg dir "cabal" ["unpack", pkg++"=="++showVersion ver]
    return $ PkgTree $ dir </> pkg++"-"++showVersion ver

-- | Remove an unpacked tree
removeTree :: PackageTree -> IO ()
removeTree (PkgTree dir) =
    removeDirectoryRecursive $ takeDirectory dir

-- | A Haddock textbase
newtype TextBase = TextBase BS.ByteString

-- | Write a Haddock textbase to a file
writeTextBase :: TextBase -> FilePath -> EitherT String IO ()
writeTextBase (TextBase content) path = liftIO $ BS.writeFile path content

-- | A file containing a Haddock textbase
type TextBaseFile = FilePath

-- | Find a pre-installed textbase corresponding to a package (if one exists)
findTextBase :: InstalledPackageInfo -> IO (Maybe TextBaseFile)
findTextBase ipkg = do
    listToMaybe . catMaybes <$> mapM checkDir (haddockHTMLs ipkg)
  where
    PackageName name = pkgName $ sourcePackageId ipkg
    checkDir root = do
        let path = root </> name++".txt"
        putStrLn $ "Looking for "++path
        exists <- doesFileExist path
        if exists
          then return $ Just path
          else return Nothing

-- | Build a textbase from source
buildTextBase :: Config -> PackageName -> PackageTree -> EitherT String IO TextBase
buildTextBase cfg (PackageName pkg) (PkgTree dir) = do
    callProcessIn cfg dir "cabal" ["configure"]
    callProcessIn cfg dir "cabal" ["haddock", "--hoogle"]
    let path = dir </> "dist" </> "doc" </> "html" </> pkg </> (pkg++".txt")
    TextBase <$> liftIO (BS.readFile path)

placeTextBase :: Config -> InstalledPackageInfo -> TextBase -> EitherT String IO ()
placeTextBase cfg ipkg tb
  | Just docRoot <- listToMaybe $ haddockHTMLs ipkg = tryIO' $ do
      let TextBase content = tb
          tbPath = docRoot </> name++".txt"
      when (verbosity cfg > verbose)
          $ liftIO $ putStrLn $ "Installing textbase to "++tbPath
      createDirectoryIfMissing True docRoot
      BS.writeFile tbPath content
  | otherwise =
      liftIO $ putStrLn $ "Can't install textbase due to missing documentation directory"
  where
    pkg = sourcePackageId ipkg
    PackageName name = pkgName pkg

getTextBase :: Config -> InstalledPackageInfo -> EitherT String IO TextBase
getTextBase cfg ipkg = do
    existing <- if ignoreExistingTextBases cfg
                  then return Nothing
                  else liftIO $ findTextBase ipkg
    case existing of
      Just path -> TextBase <$> liftIO (BS.readFile path)
      Nothing -> do
        pkgTree <- unpack cfg pkg
        tb <- buildTextBase cfg (pkgName pkg) pkgTree
        liftIO $ removeTree pkgTree

        when (installTextBase cfg) $ do
            -- It's not the end of the world if this fails
            result <- liftIO $ runEitherT $ placeTextBase cfg ipkg tb
            case result of
              Left e  -> liftIO $ putStrLn $ name++": Failed to install textbase: "++e
              Right _ -> return ()
        return tb
  where
    pkg = sourcePackageId ipkg
    PackageName name = pkgName pkg

-- | A Hoogle database
newtype Database = DB FilePath
                 deriving (Show)

-- | Delete a database file
removeDB :: Database -> IO ()
removeDB (DB path) = removeFile path

-- | Convert a textbase to a Hoogle database
convert :: Config -> TextBaseFile
        -> Maybe FilePath             -- ^ documentation root
        -> [Database]
        -> EitherT String IO Database
convert cfg tbf docRoot merge = do
    let docRoot' = maybe [] (\d->["--haddock", "--doc="++d]) docRoot
    (tb,h) <- liftIO $ openTempFile "/tmp" "db.hoo"
    liftIO $ hClose h
    let args = ["convert", tbf, tb] ++ docRoot'
               ++ map (\(DB db)->"--merge="++db) merge

    callProcessE cfg "hoogle" args
    return $ DB tb

-- | Generate a Hoogle database for an installed package
indexPackage :: Config -> InstalledPackageInfo -> EitherT String IO Database
indexPackage cfg ipkg
  | pkgName pkg `S.member` downloadPackages = do
    tmpDir <- liftIO getTemporaryDirectory
    callProcessE cfg "hoogle" ["data", "--datadir="++tmpDir, name]
    return $ DB $ tmpDir </> name++".hoo"

  | pkgName pkg `S.member` ignorePackages =
    left $ "Can't build documentation for "++name

  | otherwise = do
    tb <- getTextBase cfg ipkg
    docRoot <- case haddockHTMLs ipkg of
                   docRoot:_ | useLocalDocs cfg -> return $ Just docRoot
                   []        | useLocalDocs cfg -> do
                     liftIO $ putStrLn $ "No local documentation for "++show pkg
                     return Nothing
                   _ -> return Nothing
    (tbf,h) <- liftIO $ openTempFile "/tmp" "textbase.txt"
    liftIO $ hClose h
    writeTextBase tb tbf
    db <- convert cfg tbf docRoot []
    tryIO' $ removeFile tbf
    return db
  where
    downloadPackages = S.fromList $ map PackageName
                       ["base"]
    ignorePackages = S.fromList $ map PackageName
                     [ "rts", "ghc-prim", "integer-gmp", "bin-package-db"
                     , "ghc", "haskell98", "haskell2010"]
    pkg = sourcePackageId ipkg
    PackageName name = pkgName pkg

-- | Combine Hoogle databases
combineDBs :: Config -> [Database] -> EitherT String IO Database
combineDBs cfg dbs = do
    tmpDir <- tryIO' $ getTemporaryDirectory
    (out, h) <- tryIO' $ openTempFile tmpDir "combined.hoo"
    tryIO' $ hClose h
    let args = ["combine", "--outfile="++out] ++ map (\(DB db)->db) dbs
    callProcessE cfg "hoogle" args
    return (DB out)

-- | Install a database in Hoogle's database directory
installDB :: Database -> EitherT String IO ()
installDB (DB db) = do
    dbDir <- liftIO defaultDatabaseLocation
    tryIO' $ createDirectoryIfMissing True dbDir
    let dest = dbDir </> "databases" </> "default.hoo"
    liftIO $ copyFile db dest
    liftIO $ putStrLn $ "Installed Hoogle index to "++dest

main :: IO ()
main = do
    cfg <- execParser
        $ info (helper <*> opts)
               ( fullDesc
              <> progDesc "Generate Hoogle indexes for locally install packages"
              <> header "hoogle-index - Painless local Hoogle indexing"
              <> footer ("hoogle-index version "++showVersion Paths_hoogle_index.version)
               )

    (compiler, _, progCfg) <- configure (verbosity cfg)
                              Nothing Nothing
                              defaultProgramConfiguration
    let pkgDbs = [GlobalPackageDB, UserPackageDB] ++ otherPackageDbs cfg
    pkgIdx <- getInstalledPackages (verbosity cfg) pkgDbs progCfg
    let pkgs = reverseTopologicalOrder pkgIdx
        maybeIndex :: InstalledPackageInfo -> IO (Either (PackageId, String) Database)
        maybeIndex pkg = do
            putStrLn ""
            result <- runEitherT $ indexPackage cfg pkg
            case result of
              Left e  -> do
                let pkgId = sourcePackageId pkg
                    PackageName name = pkgName pkgId
                putStrLn $ "Error while indexing "++name++": "++e
                return $ Left (pkgId, e)
              Right r -> return $ Right r
    (failed, idxs) <- fmap partitionEithers $ mapM maybeIndex pkgs

    when (not $ null failed) $ do
      putStrLn "Failed to build the following indexes:"
      let failedMsg (pkgId, reason) = "  "++show (pkgName pkgId)++"\t"++reason
      putStrLn $ unlines $ map failedMsg failed

    res <- runEitherT $ do
        combined <- fmapLT ("Error while combining databases: "++)
                    $ combineDBs cfg idxs
        installDB combined
    either putStrLn return res

    mapM_ removeDB idxs

tryIO' :: IO a -> EitherT String IO a
tryIO' = fmapLT show . tryIO
