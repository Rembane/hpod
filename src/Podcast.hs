{-# LANGUAGE OverloadedStrings #-}
module Podcast
    ( fetchPodcast
    , newPodcast
    ) where

import Control.Error (note, partitionEithers)
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BL
import Data.Time.Calendar (fromGregorian)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import Data.Time.Clock (UTCTime(..), getCurrentTime)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Network.HTTP.Client (Manager, httpLbs, parseRequest, responseBody, responseStatus)
import Network.HTTP.Types.Status (statusCode)

import Podcast.Rss
import Podcast.Types

-- | Create a new podcast
newPodcast :: Text -> Podcast
newPodcast url = Podcast "" url V.empty (UTCTime (fromGregorian 1970 1 1) 0) -- epoch

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
          return $ either Left (\t -> Right $ p { pcTitle = t, episodes = V.fromList rs V.++ episodes p, lastChecked = now }) pct'

-- downloadEpisode :: Episode -> IO Episode
