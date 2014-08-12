{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import GHC.Generics
import System.Environment (getArgs)
import System.Exit
import Data.Aeson hiding (encode)
import Data.Yaml
import Data.List
import System.Directory (doesFileExist, findExecutable, getCurrentDirectory, getHomeDirectory)
import System.FilePath.Posix
import Hoogle (defaultDatabaseLocation, mergeDatabase)
import Control.Applicative
import Control.Monad (filterM, liftM, unless)
import Data.Maybe (isJust)

import Hoobuddy
import Data.ByteString.Char8 (unpack)


-- TODOs:
-- hoo-4 : use reader monad for config ?

deriving instance Generic Hoobuddy
instance ToJSON Hoobuddy
instance FromJSON Hoobuddy


hoogleMissingError :: String
hoogleMissingError =
    unlines [ "Error: hoogle is not installed or not in path"
            , "Please install hoogle and run `hoogle data`"]

defaultPkgs :: [String]
defaultPkgs = words "Cabal.hoo array.hoo base.hoo binary.hoo bytestring.hoo containers.hoo deepseq.hoo directory.hoo filepath.hoo haskell2010.hoo haskell98.hoo hoopl.hoo hpc.hoo old-locale.hoo old-time.hoo pretty.hoo process.hoo template-haskell.hoo time.hoo unix.hoo GLURaw.hoo GLUT.hoo HTTP.hoo HUnit.hoo OpenGL.hoo OpenGLRaw.hoo QuickCheck.hoo async.hoo attoparsec.hoo case-insensitive.hoo cgi.hoo fgl.hoo hashable.hoo haskell-src.hoo html.hoo mtl.hoo network.hoo parallel.hoo parsec.hoo primitive.hoo random.hoo regex-base.hoo regex-compat.hoo regex-posix.hoo split.hoo stm.hoo syb.hoo text.hoo transformers.hoo unordered-containers.hoo vector.hoo xhtml.hoo zlib.hoo"

help :: IO ()
help = putStrLn $
    unlines [ "Usage : hoobuddy [deps|build] <cabal-file>"
            , "                 [--help]"
            , "                 [--default]"
            , ""
            , "deps         list configured dependencies"
            , "build        do stuff yet to be defined"
            , ""
            , "--default    prints the default configuration"
            , "--help       prints this help"
            ]

main :: IO ()
main = do
    exitIfHoogleMissing
    conf <- loadConfig
    args <- getArgs
    run conf args where
        run _ ["deps", file]          = deps file
        run conf  ["build", file]     = build file conf
        run _ ["--help"]              = help
        run _ ["--default"]           = defaultConfig >>= \x -> putStrLn $ unpack $ encode x
        run _ _ = do
            help
            exitWith (ExitFailure 1)

-- | Exits with error code if hoogle isn't installed
exitIfHoogleMissing :: IO ()
exitIfHoogleMissing = do
    hoogleInstalled <- liftM isJust (findExecutable "hoogle")
    unless hoogleInstalled (putStrLn hoogleMissingError >> exitWith (ExitFailure 1))


-- | Loads configuration from file or uses defaults
loadConfig :: IO Hoobuddy
loadConfig = decodeConfig >>= maybe defaultConfig return

unique :: (Ord a) => [a] -> [a]
unique = map head . group . sort

defaultConfig :: IO Hoobuddy
defaultConfig = do
    location <- defaultDatabaseLocation
    return $ Hoobuddy (location </> "databases") True []

-- | Decodes configuration from JSON
decodeConfig :: IO (Maybe Hoobuddy)
decodeConfig = do
    homeDir <- getHomeDirectory
    parseResult <- decodeFileEither $ homeDir </> ".hoobuddy.conf"
    return $ either (const Nothing) Just parseResult


build :: FilePath -> Hoobuddy -> IO ()
build cabalFile conf = do
    pkgs <- map (++ ".hoo") <$> getDeps cabalFile
    dbs <- getHooDatabases (databases conf)

    let allPkgs = (++) pkgs (if useBase conf then defaultPkgs else custom conf)
    let available = allPkgs `intersect` dbs
    let missing = filter (`notElem` available) allPkgs

    printInfo "Fetching databases for: " missing
    hoogleFetch missing >>= \x -> case x of
        Right _             -> return ()
        Left (code, stderr) -> putStrLn ("hoogle exited with error:\n" ++ stderr) >> exitWith code

    putStrLn "Merging databases ..."
    currDir <- getCurrentDirectory
    existingDbs <- filterM doesFileExist (fmap (databases conf </>) allPkgs)
    mergeDatabase  existingDbs (currDir </> "default.hoo")

-- | Pretty printer for info output
printInfo :: String -> [String] -> IO ()
printInfo _ [] = return ()
printInfo str xs = putStrLn $ str ++ "[" ++ intercalate "," xs ++ "]"


