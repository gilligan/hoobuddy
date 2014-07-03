{-# LANGUAGE DeriveGeneric #-}

import System.Environment (getArgs)
import System.Exit
import Data.Aeson
import Data.Yaml
import Data.List
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import GHC.Generics
import System.Directory (getDirectoryContents)
import System.FilePath.Posix
import Hoogle (defaultDatabaseLocation, mergeDatabase)
import Control.Applicative

data Hoobuddy = Hoobuddy {
                           databases :: String
                         , useBase   :: Bool
                         , custom    :: [String]
                         } deriving (Generic, Show)

instance ToJSON Hoobuddy
instance FromJSON Hoobuddy

confPath :: String
confPath = "~/hoobuddy.conf"

dbPath :: FilePath
dbPath = "/home/gilligan/.cabal/share/x86_64-linux-ghc-7.8.2/hoogle-4.2.33/databases"

basePackages :: [String]
basePackages = words "Cabal.hoo array.hoo base.hoo binary.hoo bytestring.hoo containers.hoo deepseq.hoo directory.hoo filepath.hoo haskell2010.hoo haskell98.hoo hoopl.hoo hpc.hoo old-locale.hoo old-time.hoo pretty.hoo process.hoo template-haskell.hoo time.hoo unix.hoo"

platformPackages :: [String]
platformPackages = words "GLURaw.hoo GLUT.hoo HTTP.hoo HUnit.hoo OpenGL.hoo OpenGLRaw.hoo QuickCheck.hoo async.hoo attoparsec.hoo case-insensitive.hoo cgi.hoo fgl.hoo hashable.hoo haskell-src.hoo html.hoo mtl.hoo network.hoo parallel.hoo parsec.hoo primitive.hoo random.hoo regex-base.hoo regex-compat.hoo regex-posix.hoo split.hoo stm.hoo syb.hoo text.hoo transformers.hoo unordered-containers.hoo vector.hoo xhtml.hoo zlib.hoo"

help :: IO ()
help = putStrLn $
    unlines [ "Usage : hoobuddy [deps|build]"
            , "                 [--help]"
            , ""
            , "deps         list configured dependencies"
            , "build        do stuff yet to be defined"
            ]

main :: IO ()
main = do
    conf <- loadConfig
    args <- getArgs
    run conf args where
        run _ ["deps", file]          = deps file
        run conf  ["build", file]     = build file conf
        run _ ["--help"]              = help
        run _ _ = do
            help
            exitWith (ExitFailure 1)

unique :: (Ord a) => [a] -> [a]
unique = map head . group . sort

loadConfig :: IO Hoobuddy
loadConfig = decodeConfig >>= maybe defaultConfig return where
    defaultConfig = do
        location <- defaultDatabaseLocation
        return $ Hoobuddy location True []

encodeConfig :: Hoobuddy -> IO ()
encodeConfig  = encodeFile confPath

decodeConfig :: IO (Maybe Hoobuddy)
decodeConfig = do
    parseResult <- decodeFileEither confPath
    return $ either (const Nothing) Just parseResult

getHooDatabases :: FilePath -> IO [String]
getHooDatabases p = do
    files <- getDirectoryContents p
    return $ filter (\x -> ".hoo" `isSuffixOf`  x) files

hoogleFetch :: String -> IO ()
hoogleFetch [] = return ()
hoogleFetch _ = error "Not implemented yet"


build :: FilePath -> Hoobuddy -> IO ()
build cabalFile _ = do
    pkgs <- map (++ ".hoo") <$> getDeps cabalFile
    dbs <- getHooDatabases dbPath
    let allPkgs = pkgs ++ basePackages ++ platformPackages
    let available = allPkgs `intersect` dbs

    printInfo "Found databases for: " available
    let missing = filter (`notElem` available) allPkgs
    printInfo "Missing databases for: " missing
    sequence_ $ hoogleFetch <$> missing

    putStrLn "Merging databases ..."
    mergeDatabase  (map (dbPath </>) allPkgs) "/tmp/test.hoo"

    return ()
        where
            printInfo :: String -> [String] -> IO ()
            printInfo _ [] = return ()
            printInfo str xs = putStrLn $ str ++ "[" ++ intercalate "," xs ++ "]"


deps :: FilePath -> IO ()
deps  path = do
        pkgs <- getDeps path
        putStrLn $ unlines pkgs


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
