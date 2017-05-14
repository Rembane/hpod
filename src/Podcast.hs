{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Podcast
    ( module Podcast.Types
    , downloadEpisode
    , fetchPodcast
    , newPodcast
    ) where

import Control.Error (note, partitionEithers)
import Control.Monad (unless)
import Data.Bifunctor (first)
import qualified Data.ByteString as B
import Data.Char (isAlphaNum, isSpace)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..), getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime, iso8601DateFormat)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Network.Http.Client
import System.Directory (createDirectoryIfMissing)
import System.FilePath (FilePath, (</>))
import qualified System.IO.Streams as Streams

import Logging
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

-- | Download a podcast RSS/Atom file from the internet.
fetchPodcast :: (IsString msg, ToLogStr msg) => SugarLogger msg -> Podcast -> IO (Either String Podcast)
fetchPodcast logger p = get (encodeUtf8 $ pcUrl p) $ \r i -> do
    case getStatusCode r of
      200 -> do
          (pct, es) <- parseRss . B.concat <$> Streams.toList i
          let (ls, rs) = partitionEithers es
          unless (null ls) $ do
              logger "These episodes returned errors:"
              mapM_ (logger . fromString . show) ls
          let pct' = note "Got no podcast title from parseRss" pct >>= (first show . decodeUtf8')
          now <- getCurrentTime
          return $ either Left (\t -> Right $ p { pcTitle = t, episodes = foldr (\e m -> M.insertWith (\_ old -> old) (epUrl e) e m) (episodes p) rs, lastChecked = now }) pct'
      s   -> return $ Left $ show s

-- | Download an episode
downloadEpisode :: FilePath -> Podcast -> Episode -> IO (Either String Episode)
downloadEpisode basePath p e
  | isJust $ downloaded e = return $ Left "Episode already downloaded."
  | otherwise = get (encodeUtf8 $ epUrl e) $ \r i -> do
      case getStatusCode r of
        200 -> do
            let [prefix, suffix] = (T.splitOn "." . T.takeWhileEnd (/='/') . epUrl) e
            let filename = formatTime defaultTimeLocale (iso8601DateFormat Nothing) (pubDate e)
                         <> "-" <> T.unpack (textToSlug (epTitle e) <> "-" <> textToSlug prefix <> "." <> suffix)
            let relativeEpisodePath = ((T.unpack . textToSlug . pcTitle) p)
            createDirectoryIfMissing True (basePath </> relativeEpisodePath)
            Streams.withFileAsOutput (basePath </> relativeEpisodePath </> filename) (\o -> Streams.connect i o)
            now <- getCurrentTime
            return $ Right $ e { localFilename = Just $ T.pack (relativeEpisodePath </> filename), downloaded = Just now }
        s   -> return $ Left $ "Got weird statuscode: " <> (show s)
