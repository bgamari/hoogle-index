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

import Data.Either (partitionEithers)
import Control.Error

import Distribution.Verbosity (Verbosity, normal)
import Distribution.Simple.Compiler (Compiler (compilerId), compilerFlavor)
import Distribution.Simple.GHC
import Distribution.Simple.Program (defaultProgramConfiguration)
import Distribution.Simple.Compiler (PackageDB (..))
import Distribution.Simple.PackageIndex ( PackageIndex, lookupPackageName
                                        , reverseTopologicalOrder)
import Distribution.System (buildPlatform)
import Distribution.InstalledPackageInfo (InstalledPackageInfo_ (..), InstalledPackageInfo)
import Distribution.Package (PackageId, PackageName (..), PackageIdentifier (..))
import qualified Distribution.Simple.InstallDirs as IDirs

-- | Various configuration
data Config = Config { outputDir :: FilePath
                     , verbosity :: Verbosity
                     }

config = Config { outputDir = "./hoogle-index"
                , verbosity = normal
                }

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
unpack :: PackageId -> IO PackageTree
unpack (PackageIdentifier (PackageName pkg) ver) = do
    callProcess "cabal" ["unpack", pkg++"=="++showVersion ver]
    return $ PkgTree $ pkg++"-"++showVersion ver

-- | Remove an unpacked tree
removeTree :: PackageTree -> IO ()
removeTree (PkgTree dir) = removeDirectoryRecursive dir

-- | A Haddock textbase
type TextBase = FilePath

-- | Find a pre-installed textbase corresponding to a package (if one exists)
findTextBase :: InstalledPackageInfo -> IO (Maybe TextBase)
findTextBase ipkg = do
    listToMaybe . catMaybes <$> mapM checkDir (haddockHTMLs ipkg)
  where
    PackageName name = pkgName $ sourcePackageId ipkg
    checkDir root = do
        let path = root </> name++".txt"
        exists <- doesFileExist path
        return $ if exists then Just path else Nothing

-- | Build a textbase from source 
buildTextBase :: PackageName -> PackageTree -> EitherT String IO TextBase
buildTextBase (PackageName pkg) (PkgTree dir) = do
    callProcessIn dir "cabal" ["configure"]
    callProcessIn dir "runghc" ["Setup", "haddock", "--hoogle"]
    let path = dir </> "dist" </> "doc" </> "html" </> pkg </> (pkg++".txt")
    liftIO $ doesFileExist path
    return $ path

-- | A Hoogle database
newtype Database = DB FilePath
                 deriving (Show)

-- | Convert a textbase to a Hoogle database        
convert :: TextBase -> Maybe FilePath -> [Database] -> EitherT String IO Database
convert tb docRoot merge = do
    let docRoot' = maybe [] (\d->["--doc="++d]) docRoot
    let args = ["convert", tb, "--haddock"]++docRoot'++map (\(DB db)->"--merge="++db) merge
    fmapLT show $ tryIO $ callProcess "hoogle" args
    return $ DB $ replaceExtension tb ".hoo"

-- | Generate a Hoogle database for an installed package
indexPackage :: Config -> InstalledPackageInfo -> EitherT String IO Database
indexPackage cfg ipkg = do
    let pkg = sourcePackageId ipkg
    pkgTree <- fmapLT show $ tryIO $ unpack pkg
    tb <- liftIO (findTextBase ipkg)
       >>= maybe (buildTextBase (pkgName pkg) pkgTree) return
    let PackageName name = pkgName pkg
    let dest = outputDir cfg </> name++".txt" :: TextBase
    liftIO $ copyFile tb dest

    let docRoot = listToMaybe $ haddockHTMLs ipkg
    db <- convert dest docRoot []

    liftIO $ removeTree pkgTree
    return db

-- | Combine Hoogle databases
combineDBs :: [Database] -> IO Database
combineDBs dbs = do
    let out = "all.hoo"
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
    createDirectoryIfMissing True (outputDir config)
    (compiler, _, progCfg) <- configure (verbosity config)
                              Nothing Nothing
                              defaultProgramConfiguration
    pkgIdx <- getInstalledPackages (verbosity config)
              [GlobalPackageDB, UserPackageDB] progCfg
    let pkgs = reverseTopologicalOrder pkgIdx
        maybeIndex :: InstalledPackageInfo -> IO (Either (PackageId, String) Database)
        maybeIndex pkg = runEitherT $ fmapLT (sourcePackageId pkg,)
                         $ indexPackage config pkg
    (failed, idxs) <- fmap partitionEithers $ mapM maybeIndex pkgs

    when (not $ null failed) $ do
      putStrLn "Failed to build the following indexes:"
      let failedMsg (pkgId, reason) = "  "++show (pkgName pkgId)++"\t"++reason
      putStrLn $ unlines $ map failedMsg failed

    combined <- combineDBs idxs
    res <- runEitherT $ installDB compiler pkgIdx combined
    either print (const $ return ()) res
