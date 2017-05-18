{-# LANGUAGE MultiWayIf, OverloadedStrings #-}

-- TODO: Better error handling
-- TODO: Make the program not check a podcast which has been checked the last hour or something.
-- TODO: Add option to specify path to config file.

module Main where

import Control.Exception (bracket)
import Control.Monad (forM)
import qualified Data.Attoparsec.Text as A
import qualified Data.ByteString as B
import Data.Ini (Ini, parseValue, readIniFile)
import Data.List (repeat, sortOn)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Semigroup ((<>))
import Data.Serialize (decode, encode)
import qualified Data.Text as T
import Data.Text (Text)
import Network.Http.Client
import Options.Applicative (Parser, argument, auto, command, execParser, fullDesc, header, help, helper, info, long, metavar, option, progDesc, short, str, subparser, value)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getFileSize, listDirectory)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (IOMode(..), hSetBinaryMode, withFile)
import System.Posix (fileSize, getFileStatus, isDirectory, isRegularFile)

import Logging
import Podcast
import TUI

-- | Config data type
data Config = Config
    { cfgDBPath         :: FilePath -- ^ The full path of the database.
    , cfgPodcastPath    :: FilePath -- ^ The full path of the directory the podcasts are put in.
    , cfgAllocatedSpace :: Int      -- ^ The amount of space the podcasts are allowed to use, in MB.
    } deriving (Show)

data Options = Options Actions
    deriving (Show)

data Actions
    = Add Url
    | List
    | Download
    deriving (Show)

argumentParser :: Parser Options
argumentParser = Options <$> actionParser

actionParser :: Parser Actions
actionParser =
    subparser
        ( command "add" (info
            (Add <$> argument (T.pack <$> str) (metavar "URL"))
            (progDesc "Add a podcast URL to the database."))
        <> command "list" (info
            (pure List)
            (progDesc "List all podcasts."))
        <> command "download" (info
            (pure Download)
            (progDesc "Download all podcasts."))
        )

saveDb :: FilePath -> [Podcast] -> IO ()
saveDb fp db = withFile fp WriteMode $ \fh -> do
    hSetBinaryMode fh True
    (B.hPut fh . encode) db

-- | Like parseValue but returns a default value if something goes wrong.
parseValueDefault :: Text -> Text -> a -> A.Parser a -> Ini -> a
parseValueDefault section key fallback parser ini = either (const fallback) id (parseValue section key parser ini)

-- | Parses a config file, uses defaults instead of failing.
readConfigFile :: Ini -> Config
readConfigFile ini =
    Config (T.unpack $ parseValueDefault "filepaths" "db-path"             "podcasts.db" A.takeText ini)
           (T.unpack $ parseValueDefault "filepaths" "podcast-path"        "podcasts/"   A.takeText ini)
           (go (       parseValueDefault "podcasts"  "max-allocated-space"          1024 A.decimal  ini))
    where
        -- Default max size in byte.
        go = (*) (1024 * 1024 * 1024) -- To Megabyte to Kilobyte to byte

-- | Computes the total filesize of a path and its subdirectories.
getRecursiveFileSize :: FilePath -> IO Integer
getRecursiveFileSize fp = do
    st <- getFileStatus fp
    if | isDirectory   st -> listDirectory fp >>= \dirs -> sum <$> mapM (getRecursiveFileSize . (fp </>)) dirs
       | isRegularFile st -> (return . fromIntegral . fileSize) st
       | otherwise        -> return 0

-- | Get the size of an episode on the disk.
getEpisodeFileSize :: Episode -> IO Integer
getEpisodeFileSize e = maybe (return 0) (getFileSize . T.unpack) (localFilename e)

-- | Downloads new episodes while there still is space to put them in.
downloadAll :: (IsString msg, ToLogStr msg) => SugarLogger msg -> Config -> [Podcast] -> IO [Podcast]
downloadAll logger cfg ps = do
    currSize <- getRecursiveFileSize (cfgPodcastPath cfg)
    if currSize >= fromIntegral (cfgAllocatedSpace cfg)
       then (logger $ fromString $ "We have used up our podcast storage quota. Allowed: " <> show (cfgAllocatedSpace cfg) <> " Used: " <> show currSize) >> return ps
       else forM ps $ \p -> do
           p' <- fetchPodcast logger p
           case p' of
             Left err  -> logger (fromString err) >> return p
             Right p'' -> do
                 logger (fromString $ show p'')
                 -- Download in chronological order.
                 es' <- (moderateDownloader p'' currSize . sortOn pubDate . M.elems . episodes) p''
                 return p'' { episodes = M.fromList es' }
    where
        -- | Download until we've reached the quota.
        moderateDownloader :: Podcast -> Integer -> [Episode] -> IO [(Url, Episode)]
        moderateDownloader _ _        []     = return []
        moderateDownloader p currSize (e:es) = do
            if currSize >= (fromIntegral (cfgAllocatedSpace cfg))
               then logger "We have used all our podcast quota." >> return (map (\x -> (epUrl x, x)) (e:es))
               else do
                   e' <- downloadE p e
                   size <- getEpisodeFileSize (snd e')
                   (e':) <$> moderateDownloader p (currSize + size) es

        -- Download an episode, the quick and dirty way.
        downloadE p e = do
            logger "Downloading... "
            logger (fromString $ show e)
            e' <- downloadEpisode (cfgPodcastPath cfg) p e
            case e' of
              Left err  -> do
                  logger $ fromString ("Boom!\n" <> show err)
                  return (epUrl e, e)
              Right e'' -> return (epUrl e'', e'')

main :: IO ()
main = tuiMain
{- do
    cfg <- either (\e -> putStrLn e >> exitFailure) return =<< (fmap . fmap) readConfigFile (readIniFile "hpod.ini")

    (Options action) <- execParser $ info (helper <*> argumentParser)
        ( fullDesc
        <> header "hPod"
        <> progDesc "Podcast management. Download your podcasts today."
        )

    let dbPath = cfgDBPath cfg
    exists <- doesFileExist dbPath
    db <- if exists
             then decode <$> withFile dbPath ReadMode (\fh -> do
                 hSetBinaryMode fh True
                 B.hGetContents fh)
             else do
                 withFile dbPath WriteMode (const $ return ())
                 (return . return) []

    dexists <- doesDirectoryExist (cfgPodcastPath cfg)
    if dexists
       then return ()
       else createDirectoryIfMissing True (cfgPodcastPath cfg)

    bracket
        newLogger
        (\(_, loggerCleanup) -> loggerCleanup)
        (\(logger, loggerCleanup) ->
            case db of
              Left err -> putStrLn err >> exitFailure
              Right db' -> case action of
                             Add url  -> saveDb dbPath (newPodcast url : db')
                             List     -> mapM_ print db'
                             Download -> do
                                 db'' <- downloadAll logger cfg db'
                                 saveDb dbPath db''
        )
        -}
