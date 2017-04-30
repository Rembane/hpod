{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Podcast
    ( module Podcast.Types
    , downloadEpisode
    , fetchPodcast
    , newPodcast
    ) where

import Control.Error (note, partitionEithers)
import Data.Bifunctor (first)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlphaNum, isSpace)
import Data.Monoid ((<>))
import Data.Function (on)
import Data.Time.Calendar (fromGregorian)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import Data.Time.Clock (UTCTime(..), getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime, iso8601DateFormat)
import qualified Data.Map.Strict as M
import Network.HTTP.Client (Manager, brRead, httpLbs, parseRequest, responseBody, responseStatus, withResponse)
import Network.HTTP.Types.Status (statusCode)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (FilePath, (</>))
import System.IO (IOMode(..), withFile)

import Podcast.Rss
import Podcast.Types

-- | This function just cleans up a string so it passes as a slug.
textToSlug :: Text -> Text
textToSlug = T.toLower
    . T.foldr (\c a -> if T.null a then T.singleton c else let lc = T.last a in if lc == '-' && lc == c then a else T.cons c a) ""
    . T.map (\c -> if isSpace c then '-' else c) . T.filter (\c -> isAlphaNum c || isSpace c)
    . T.strip

-- | Create a new podcast
newPodcast :: Text -> Podcast
newPodcast url = Podcast "" url M.empty (UTCTime (fromGregorian 1970 1 1) 0) -- epoch

-- | Fetch a podcast
fetchPodcast :: Podcast -> Manager -> IO (Either String Podcast)
fetchPodcast p mgr = do
    r <- mapM (`httpLbs` mgr) (parseRequest (T.unpack $ pcUrl p))
    case r of
      Left e         -> return $ Left $ show e
      Right response -> do
          putStrLn $ "The status code was: " ++ show (statusCode $ responseStatus response)
          -- TODO: Log the lefts.
          let (pct, (_, rs)) = partitionEithers <$> (parseRss . BL.toStrict . responseBody) response
          let pct' = note "Got no podcast title from parseRss" pct >>= (first show . decodeUtf8')
          now <- getCurrentTime
          return $ either Left (\t -> Right $ p { pcTitle = t, episodes = foldr (\e m -> M.insertWith (\_ old -> old) (epUrl e) e m) (episodes p) rs, lastChecked = now }) pct'

-- | Download an episode
downloadEpisode :: FilePath -> Podcast -> Episode -> Manager -> IO (Either String Episode)
downloadEpisode basePath p e mgr = do
    case (parseRequest . T.unpack . epUrl) e of
        Left err  -> return $ Left $ show err
        Right req -> do
            let [prefix, suffix] = T.splitOn "." $ epTitle e <> "-" <> T.takeWhileEnd (/='/') (epUrl e)
            let filename = (formatTime defaultTimeLocale (iso8601DateFormat Nothing) (pubDate e)) <> "-" <> T.unpack (textToSlug prefix <> "." <> suffix)
            let relativeEpisodePath = ((T.unpack . textToSlug . pcTitle) p)
            createDirectoryIfMissing True (basePath </> relativeEpisodePath)
            withResponse req mgr $ \br ->
                withFile (basePath </> relativeEpisodePath </> filename) WriteMode $ \fh ->
                    readLoop br fh
            now <- getCurrentTime
            return $ Right $ e { localFilename = Just $ T.pack (relativeEpisodePath </> filename), downloaded = Just now }
    where
        readLoop br fh = do
            bs <- brRead $ responseBody br
            if B.null bs
               then putStrLn "Got empty read. Returning." >> return ()
               else B.hPut fh bs >> readLoop br fh
