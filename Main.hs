{-# LANGUAGE TupleSections #-}

import Control.Monad (forM, when)
import Control.Monad.IO.Class
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
           <*> many (option (fmap SpecificPackageDB . pure)
               ( short 'f' <> long "package-db"
              <> help "Add an addition package database (e.g. a Cabal sandbox)"
               ))

-- | An unpacked Cabal project
newtype PackageTree = PkgTree FilePath
                    deriving (Show)

-- | Call a package from the given directory
callProcessIn :: FilePath -> FilePath -> [String] -> EitherT String IO ()
callProcessIn dir exec args = do
    let cp = (proc exec args) { cwd = Just dir }
    (_, _, _, ph) <- liftIO $ createProcess cp
    code <- liftIO $ waitForProcess ph
    case code of
      ExitSuccess -> return ()
      ExitFailure e -> left $ "Process "++exec++" failed with error "++show e

-- | Unpack a package
unpack :: PackageId -> EitherT String IO PackageTree
unpack (PackageIdentifier (PackageName pkg) ver) = do
    tmpDir <- liftIO getTemporaryDirectory
    dir <- liftIO $ createTempDirectory tmpDir "hoogle-index.pkg"
    callProcessIn dir "cabal" ["unpack", pkg++"=="++showVersion ver]
    return $ PkgTree $ dir </> pkg++"-"++showVersion ver

-- | Remove an unpacked tree
removeTree :: PackageTree -> IO ()
removeTree (PkgTree dir) =
    removeDirectoryRecursive $ takeDirectory dir

-- | A Haddock textbase
newtype TextBase = TextBase BS.ByteString

-- | Write a Haddock textbase to a file
writeTextBase :: TextBase -> FilePath -> IO ()
writeTextBase (TextBase content) path = BS.writeFile path content

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
buildTextBase :: PackageName -> PackageTree -> EitherT String IO TextBase
buildTextBase (PackageName pkg) (PkgTree dir) = do
    callProcessIn dir "cabal" ["configure"]
    callProcessIn dir "cabal" ["haddock", "--hoogle"]
    let path = dir </> "dist" </> "doc" </> "html" </> pkg </> (pkg++".txt")
    TextBase <$> liftIO (BS.readFile path)

getTextBase :: Config -> InstalledPackageInfo -> EitherT String IO TextBase
getTextBase cfg ipkg = do
    existing <- if ignoreExistingTextBases cfg
                  then return Nothing
                  else liftIO $ findTextBase ipkg
    case existing of
      Just path -> TextBase <$> liftIO (BS.readFile path)
      Nothing -> do
        pkgTree <- unpack pkg
        tb <- buildTextBase (pkgName pkg) pkgTree
        liftIO $ removeTree pkgTree

        -- install textbase if necessary
        case listToMaybe $ haddockHTMLs ipkg of
          Just docRoot | installTextBase cfg -> do
            let TextBase content = tb
                PackageName name = pkgName $ sourcePackageId ipkg
                tbPath = docRoot </> name++".txt"
            liftIO $ putStrLn $ "Installing textbase to "++tbPath
            liftIO $ BS.writeFile tbPath content
          Nothing | installTextBase cfg -> do
            liftIO $ putStrLn $ "Can't install textbase due to missing documentation directory"
          _ -> return ()
        return tb
  where
    pkg = sourcePackageId ipkg

-- | A Hoogle database
newtype Database = DB FilePath
                 deriving (Show)

-- | Delete a database file
removeDB :: Database -> IO ()
removeDB (DB path) = removeFile path

-- | Convert a textbase to a Hoogle database
convert :: TextBaseFile -> Maybe FilePath -> [Database] -> EitherT String IO Database
convert tbf docRoot merge = do
    let docRoot' = maybe [] (\d->["--haddock", "--doc="++d]) docRoot
    (tb,h) <- liftIO $ openTempFile "/tmp" "db.hoo"
    liftIO $ hClose h
    let args = ["convert", tbf, tb] ++ docRoot'
               ++ map (\(DB db)->"--merge="++db) merge

    fmapLT show $ tryIO $ callProcess "hoogle" args
    return $ DB tb

-- | Generate a Hoogle database for an installed package
indexPackage :: Config -> InstalledPackageInfo -> EitherT String IO Database
indexPackage cfg ipkg = do
    let pkg = sourcePackageId ipkg
    tb <- getTextBase cfg ipkg
    docRoot <- case haddockHTMLs ipkg of
                   docRoot:_ | useLocalDocs cfg -> return $ Just docRoot
                   []        | useLocalDocs cfg -> do
                     liftIO $ putStrLn $ "No local documentation for "++show pkg
                     return Nothing
                   _ -> return Nothing
    (tbf,h) <- liftIO $ openTempFile "/tmp" "textbase.txt"
    liftIO $ hClose h
    liftIO $ writeTextBase tb tbf
    db <- convert tbf docRoot []
    liftIO $ removeFile tbf
    return db

-- | Combine Hoogle databases
combineDBs :: [Database] -> IO Database
combineDBs dbs = do
    tmpDir <- getTemporaryDirectory
    (out, h) <- openTempFile tmpDir "combined.hoo"
    hClose h
    callProcess "hoogle" (["combine", "--outfile="++out] ++ map (\(DB db)->db) dbs)
    return (DB out)

-- | Install a database in Hoogle's database directory
installDB :: Compiler -> PackageIndex -> Database -> EitherT String IO ()
installDB compiler pkgIdx (DB db) = do
    hooglePkg <- case sortBy (comparing fst) $ lookupPackageName pkgIdx (PackageName "hoogle") of
                      (_, (pkg:_)):_ -> return pkg
                      _              -> left "Hoogle not installed"
    template <- liftIO $ IDirs.defaultInstallDirs (compilerFlavor compiler) True False
    let installDirs = IDirs.absoluteInstallDirs (sourcePackageId hooglePkg)
                                                (compilerId compiler)
                                                IDirs.NoCopyDest
                                                buildPlatform template

    let dbDir = IDirs.datadir installDirs </> "databases"
        dest = dbDir </> "default.hoo"
    liftIO $ copyFile db dest
    liftIO $ putStrLn $ "Installed Hoogle index to "++dest

main :: IO ()
main = do
    config <- execParser
        $ info (helper <*> opts)
               ( fullDesc
              <> progDesc "Generate Hoogle indexes for locally install packages"
              <> header "hoogle-index - Painless local Hoogle indexing"
               )

    (compiler, _, progCfg) <- configure (verbosity config)
                              Nothing Nothing
                              defaultProgramConfiguration
    let pkgDbs = [GlobalPackageDB, UserPackageDB] ++ otherPackageDbs config
    pkgIdx <- getInstalledPackages (verbosity config) pkgDbs progCfg
    let pkgs = reverseTopologicalOrder pkgIdx
        maybeIndex :: InstalledPackageInfo -> IO (Either (PackageId, String) Database)
        maybeIndex pkg = do
            putStrLn ""
            result <- runEitherT $ indexPackage config pkg
            case result of
              Left e  -> do
                let pkgId = sourcePackageId pkg
                putStrLn $ "Error while indexing "++show (pkgName pkgId)++": "++e
                return $ Left (pkgId, e)
              Right r -> return $ Right r
    (failed, idxs) <- fmap partitionEithers $ mapM maybeIndex pkgs

    when (not $ null failed) $ do
      putStrLn "Failed to build the following indexes:"
      let failedMsg (pkgId, reason) = "  "++show (pkgName pkgId)++"\t"++reason
      putStrLn $ unlines $ map failedMsg failed

    combined <- combineDBs idxs
    mapM_ removeDB idxs
    res <- runEitherT $ installDB compiler pkgIdx combined
    either print (const $ return ()) res
