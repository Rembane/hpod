{-# LANGUAGE OverloadedStrings #-}

-- TODO: Logging
-- TODO: Better error handling
-- TODO: Etags
-- TODO: Make the program not check a podcast which has been checked the last hour or something.

module Main where

import Control.Monad (forM)
import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.Semigroup ((<>))
import Data.Serialize (decode, encode)
import qualified Data.Text as T
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Options.Applicative (Parser, argument, auto, command, execParser, fullDesc, header, help, helper, info, long, metavar, option, progDesc, short, str, subparser, value)
import System.Directory (doesFileExist)
import System.IO (IOMode(..), hSetBinaryMode, withFile)

import Podcast

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

saveDb :: [Podcast] -> IO ()
saveDb db = withFile "podcasts.db" WriteMode $ \fh -> do
    hSetBinaryMode fh True
    (B.hPut fh . encode) db

main :: IO ()
main = do
    (Options action) <- execParser $ info (helper <*> argumentParser)
        ( fullDesc
        <> header "hPod"
        <> progDesc "Podcast management. Download your podcasts today."
        )

    exists <- doesFileExist "podcasts.db"
    db <- if exists
             then decode <$> withFile "podcasts.db" ReadMode (\fh -> do
                 hSetBinaryMode fh True
                 B.hGetContents fh)
             else do
                 withFile "podcasts.db" WriteMode (const $ return ())
                 (return . return) []

    case db of
      Left err -> putStrLn err
      Right db' -> case action of
                    Add url  -> saveDb ((newPodcast url) : db')
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
                                     e' <- downloadEpisode "podcasts" p'' e manager
                                     case e' of
                                       Left err  -> putStrLn "Boom!" >> print err >> return (epUrl e, e)
                                       Right e'' -> return (epUrl e'', e'')

                                 return p'' { episodes = M.fromList es' }
                       saveDb db''

