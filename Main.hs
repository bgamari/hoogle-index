{-# LANGUAGE TupleSections #-}

import Control.Monad (forM, when)
import Control.Monad.IO.Class
import Control.Applicative
import Data.Version
import Data.Ord (comparing)
import Data.List (sortBy)
import System.Directory
import System.Exit
import System.Process
import System.FilePath

import Data.Either (partitionEithers)
import Control.Error

import Distribution.Verbosity (normal)
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

verbosity = normal

newtype PackageTree = PkgTree FilePath
                    deriving (Show)

callProcessIn :: FilePath -> FilePath -> [String] -> EitherT String IO ()
callProcessIn dir exec args = do
    let cp = (proc exec args) { cwd = Just dir }
    (_, _, _, ph) <- liftIO $ createProcess cp
    code <- liftIO $ waitForProcess ph
    case code of
      ExitSuccess -> return ()
      ExitFailure e -> left $ "Process "++exec++" failed with error "++show e

unpack :: PackageId -> IO PackageTree
unpack (PackageIdentifier (PackageName pkg) ver) = do
    callProcess "cabal" ["unpack", pkg++"=="++showVersion ver]
    return $ PkgTree $ pkg++"-"++showVersion ver

removeTree :: PackageTree -> IO ()
removeTree (PkgTree dir) = removeDirectoryRecursive dir

type TextBase = FilePath

buildTextBase :: PackageName -> PackageTree -> EitherT String IO TextBase
buildTextBase (PackageName pkg) (PkgTree dir) = do
    callProcessIn dir "cabal" ["configure"]
    callProcessIn dir "runghc" ["Setup", "haddock", "--hoogle"]
    let path = dir </> "dist" </> "doc" </> "html" </> pkg </> (pkg++".txt")
    liftIO $ doesFileExist path
    return $ path

newtype Database = DB FilePath
                 deriving (Show)

convert :: TextBase -> Maybe FilePath -> [Database] -> EitherT String IO Database
convert tb docRoot merge = do
    let docRoot' = maybe [] (\d->["--doc="++d]) docRoot
    let args = ["convert", tb, "--haddock"]++docRoot'++map (\(DB db)->"--merge="++db) merge
    fmapLT show $ tryIO $ callProcess "hoogle" args
    return $ DB $ replaceExtension tb ".hoo"

indexPackage :: Config -> InstalledPackageInfo -> EitherT String IO Database
indexPackage cfg ipkg = do
    let pkg = sourcePackageId ipkg
    pkgTree <- fmapLT show $ tryIO $ unpack pkg
    tb <- buildTextBase (pkgName pkg) pkgTree
    let PackageName name = pkgName pkg
    let dest = outputDir cfg </> name++".txt" :: TextBase
    liftIO $ copyFile tb dest

    let docRoot = case haddockHTMLs ipkg of
                      root:_ -> Just root
                      []     -> Nothing
    db <- convert dest docRoot []

    liftIO $ removeTree pkgTree
    return db

data Config = Config { outputDir :: FilePath }

config = Config { outputDir = "./hoogle-index" }

combineDBs :: [Database] -> IO Database
combineDBs dbs = do
    let out = "all.hoo"
    callProcess "hoogle" (["combine", "--outfile="++out] ++ map (\(DB db)->db) dbs)
    return (DB out)

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

main = do
    createDirectoryIfMissing True (outputDir config)
    (compiler, _, progCfg) <- configure verbosity Nothing Nothing defaultProgramConfiguration
    pkgIdx <- getInstalledPackages verbosity [GlobalPackageDB, UserPackageDB] progCfg
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
    runEitherT $ installDB compiler pkgIdx combined
