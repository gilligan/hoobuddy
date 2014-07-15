module Hoobuddy where

import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import System.Exit
import System.Directory
import Data.List (isSuffixOf)
import System.Process
import System.IO (hGetContents)
import System.FilePath.Posix (dropExtension)
import Data.Functor ((<$>))


data Hoobuddy = Hoobuddy {
                           databases :: String
                         , useBase   :: Bool
                         , custom    :: [String]
                         } deriving (Show)

type StdOut = String
type StdErr = String

-- | Prints dependencies from cabal file
deps :: FilePath -> IO ()
deps  path = do
        pkgs <- getDeps path
        putStrLn $ unlines pkgs

-- | Returns list of dependencies from cabal file
getDeps :: FilePath -> IO [String]
getDeps cabal = do
        contents <- readFile cabal
        let depInfo = parsePackageDescription contents
        case depInfo of
            ParseFailed _ -> exitWith (ExitFailure 1)
            ParseOk _ d     -> return (packageNames $ extractDeps d)
        where
            packageNames :: [Dependency] -> [String]
            packageNames  = map (init . tail . head . tail . words . show . pkg)
            pkg :: Dependency -> PackageName
            pkg (Dependency x _) = x

extractDeps :: GenericPackageDescription -> [Dependency]
extractDeps d = ldeps ++ edeps
  where ldeps = case condLibrary d of
                Nothing -> []
                Just c -> condTreeConstraints c
        edeps = concatMap (condTreeConstraints . snd) $ condExecutables d

-- | Returns a list of available ".hoo" files
getHooDatabases :: FilePath -> IO [String]
getHooDatabases p = do
    files <- getDirectoryContents p
    return $ filter (\x -> ".hoo" `isSuffixOf`  x) files

-- | Calls hoogle to fetch all packages specified
hoogleFetch :: [String] -> IO (Either (ExitCode, StdErr) StdOut)
hoogleFetch [] = return (Right "No data to fetch")
hoogleFetch pkgs =  do
    (_, Just hOut, Just hErr, pHandle) <- createProcess (proc "hoogle" ("data":pkgNames)) {std_out = CreatePipe, std_err = CreatePipe}
    exitCode <- waitForProcess pHandle
    stdOut <- hGetContents hOut
    stdErr <- hGetContents hErr
    return (if exitCode == ExitSuccess then Right stdOut else Left (exitCode, stdErr))
        where
            pkgNames = dropExtension <$> pkgs

