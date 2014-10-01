{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import GHC.Generics
import System.Environment (getArgs)
import Data.Aeson hiding (encode)
import Data.Yaml
import Data.List
import System.Directory (doesFileExist, findExecutable, getCurrentDirectory, getHomeDirectory)
import System.FilePath.Posix
import Hoogle (defaultDatabaseLocation, mergeDatabase)
import Control.Applicative
import Control.Monad.Error
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
    unlines [ "Usage : hoobuddy [deps|fetch] <cabal-file>"
            , "                 [--help]"
            , "                 [--default]"
            , ""
            , "deps         list configured dependencies"
            , "fetch        fetch and merge documentation databases"
            , ""
            , "--default    prints the default configuration"
            , "--help       prints this help"
            ]

type HoobuddyAction = ErrorT String IO

main :: IO ()
main = do
    conf <- loadConfig
    args <- getArgs
    ret <- runErrorT $ runHoobuddy conf args
    either putStrLn (\_ -> putStrLn "") ret

runHoobuddy :: Hoobuddy -> [String] -> HoobuddyAction ()
runHoobuddy cfg args = do
    hoogleInstalled <- liftM isJust (liftIO $ findExecutable "hoogle")
    unless hoogleInstalled (throwError hoogleMissingError)
    run cfg args
        where
            run _     ["deps", file]      = liftIO $ deps file
            run conf  ["fetch", file]     = build file conf
            run _     ["--help"]          = liftIO help
            run _     ["--default"]       = liftIO $ defaultConfig >>= \x -> putStrLn $ unpack $ encode x
            run _ _                       = liftIO help


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


build :: FilePath -> Hoobuddy -> HoobuddyAction ()
build cabalFile conf = do
    pkgs <- map (++ ".hoo") <$> liftIO (getDeps cabalFile)
    dbs <- liftIO $ getHooDatabases (databases conf)

    let allPkgs = (++) pkgs (if useBase conf then defaultPkgs else custom conf)
    let available = allPkgs `intersect` dbs
    let missing = filter (`notElem` available) allPkgs

    liftIO $ if null missing
     then putStrLn "No data needs to be fetched"
     else printInfo "Fetching databases for: " missing
    fetchOp <- liftIO $ hoogleFetch missing
    either (\_ -> throwError "Error executing hoogle") (\_ -> return ()) fetchOp

    liftIO $ putStrLn "Merging databases ..."
    currDir <- liftIO getCurrentDirectory
    existingDbs <- liftIO $ filterM doesFileExist (fmap (databases conf </>) allPkgs)
    liftIO $ mergeDatabase existingDbs (currDir </> "default.hoo")
    liftIO $ putStrLn "Success: default.hoo"

-- | Pretty printer for info output
printInfo :: String -> [String] -> IO ()
printInfo str xs = putStrLn $ str ++ "[" ++ intercalate "," xs ++ "]"


