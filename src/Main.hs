{-# LANGUAGE OverloadedStrings #-}

-- TODO: Logging
-- TODO: Better error handling
-- TODO: Etags
-- TODO: Make the program not check a podcast which has been checked the last hour or something.

module Main where

import Control.Monad (forM)
import qualified Data.Attoparsec.Text as A
import qualified Data.ByteString as B
import Data.Ini (Ini, parseValue, readIniFile)
import qualified Data.Map as M
import Data.Semigroup ((<>))
import Data.Serialize (decode, encode)
import qualified Data.Text as T
import Data.Text (Text)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Options.Applicative (Parser, argument, auto, command, execParser, fullDesc, header, help, helper, info, long, metavar, option, progDesc, short, str, subparser, value)
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.IO (IOMode(..), hSetBinaryMode, withFile)

import Podcast

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
           (           parseValueDefault "podcasts"  "max-allocated-space" 1024          A.decimal  ini)

main :: IO ()
main = do
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

    case db of
      Left err -> putStrLn err >> exitFailure
      Right db' -> case action of
                     Add url  -> saveDb dbPath (newPodcast url : db')
                     List     -> mapM_ print db'
                     Download -> do
                       manager <- newManager defaultManagerSettings
                       db'' <- forM db' $ \p -> do
                           p' <- fetchPodcast p manager
                           case p' of
                             Left err  -> print err >> return p
                             Right p'' -> do
                                 print p''
                                 es' <- forM (M.elems $ episodes p'') $ \e -> do
                                     putStr "Downloading... "
                                     print e
                                     e' <- downloadEpisode (cfgPodcastPath cfg) p'' e manager
                                     case e' of
                                       Left err  -> putStrLn "Boom!" >> print err >> return (epUrl e, e)
                                       Right e'' -> return (epUrl e'', e'')

                                 return p'' { episodes = M.fromList es' }
                       saveDb dbPath db''

