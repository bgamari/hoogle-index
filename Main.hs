{-# LANGUAGE TupleSections #-}

import Control.Monad (forM, when)
import Control.Monad.IO.Class
import Control.Applicative
import Data.Version
import System.Directory
import System.Exit
import System.Process
import System.FilePath

import Data.Either (partitionEithers)
import Control.Error

import Distribution.Verbosity (normal)
import Distribution.Simple.GHC
import Distribution.Simple.Program (defaultProgramConfiguration)
import Distribution.Simple.Compiler (PackageDB (..))
import Distribution.Simple.PackageIndex (reverseTopologicalOrder)
import Distribution.InstalledPackageInfo (InstalledPackageInfo_ (..), InstalledPackageInfo)
import Distribution.Package (PackageId, PackageName (..), PackageIdentifier (..))

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
                      root:_ -> Just $ root </> "index.html"
                      []     -> Nothing
    db <- convert dest docRoot []

    liftIO $ removeTree pkgTree
    return db

data Config = Config { outputDir :: FilePath }

config = Config { outputDir = "./hoogle-index" }

main = do
    createDirectoryIfMissing True (outputDir config)
    (_, _, progCfg) <- configure verbosity Nothing Nothing defaultProgramConfiguration
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

    callProcess "hoogle" (["--outfile=all.hoo", "combine"] ++ map (\(DB db)->db) idxs)

    --installDirs <- defaultInstallDirs buildCompilerFlavor True False
    putStrLn "Place all.hoo in Cabal's datadir"
