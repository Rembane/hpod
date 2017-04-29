{-# LANGUAGE OverloadedStrings #-}

-- TODO: Logging
-- TODO: Make sure we don't add duplicates of episodes.
-- TODO: Better error handling

module Main where

import Control.Error (note, partitionEithers)
import Control.Monad.State (State, execState, modify')
import Data.Bifunctor (first)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8')
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..))
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Exception (SomeException)
import Network.HTTP.Client (Manager, defaultManagerSettings, httpLbs, newManager, parseRequest, responseBody, responseStatus)
import Network.HTTP.Types.Status (statusCode)
import System.Environment (getArgs)
import Xeno.SAX (process)

data Episode = Episode
    { epTitle       :: Text
    , epUrl         :: Text
    , pubDate       :: UTCTime
    , downloaded    :: Maybe UTCTime
    , listened      :: Maybe UTCTime
    , localFilename :: Maybe Text
    }
    deriving (Show)

data Podcast = Podcast
    { pcTitle     :: Text
    , pcUrl       :: Text
    , episodes    :: Vector Episode
    , lastChecked :: UTCTime
    }
    deriving (Show)

data States = ParsingPodcastTitle
            | ParsingItem
            | ParsingTitle
            | ParsingPubDate
            | ParsingEnclosure
            | Other

data MyState = MyState
    { currState    :: States
    , partHash     :: Map B.ByteString B.ByteString
    , podcastTitle :: Maybe B.ByteString
    , msEpisodes   :: [Either String Episode]
    }

type ParseState = State MyState

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
          return $ either Left (\t -> Right $ p { pcTitle = t, episodes = V.fromList rs V.++ episodes p }) pct'

-- | Returns podcast title, and a list of episodes and error messages.
parseRss :: B.ByteString -> (Maybe B.ByteString, [Either String Episode])
parseRss s = (podcastTitle finalState, msEpisodes finalState)
    where
        proc = process handleTagStart handleAttribute handleTagEnd handleText handleTagClose handleCData s
        emptyState = MyState Other M.empty Nothing []
        finalState = execState proc emptyState

        handleTagStart :: B.ByteString -> ParseState ()
        handleTagStart tagName = modify' $ \st ->
            case (tagName, currState st) of
              ("item",      Other)       -> st { currState = ParsingItem }
              ("title",     Other)       -> st { currState = ParsingPodcastTitle }
              ("title",     ParsingItem) -> st { currState = ParsingTitle }
              ("pubDate",   ParsingItem) -> st { currState = ParsingPubDate }
              ("enclosure", ParsingItem) -> st { currState = ParsingEnclosure }
              _                          -> st

        handleAttribute :: B.ByteString -> B.ByteString -> ParseState ()
        handleAttribute key value = modify' $ \st ->
            case (key, currState st) of
              -- Episode url
              ("url", ParsingEnclosure) -> st { partHash = M.insert key value (partHash st) }
              _                         -> st

        -- When a start-tag ends this callback is called.
        handleTagEnd :: B.ByteString -> ParseState ()
        handleTagEnd tagName = modify' $ \st ->
            case (tagName, currState st) of
              ("enclosure", ParsingEnclosure) -> st { currState = ParsingItem }
              _                               -> st

        handleText :: B.ByteString -> ParseState ()
        handleText txt = modify' $ \st ->
            case currState st of
              ParsingTitle        -> st { partHash = M.insert "title" txt (partHash st) }
              ParsingPubDate      -> st { partHash = M.insert "pubdate" txt (partHash st) }
              ParsingPodcastTitle -> st { podcastTitle = Just txt }
              _                   -> st

        handleTagClose :: B.ByteString -> ParseState ()
        handleTagClose tagName = modify' $ \st ->
            case (tagName, currState st) of
              ("item", ParsingItem) -> do
                  let ph  = partHash st
                  let lph = (`M.lookup` ph)
                  let mep = first (\msg -> "Couldn't turn this partHash into episode: " <> show ph <> "\n" <> msg) (
                                Episode
                                <$> ((first show . decodeUtf8') =<< note "Couldn't find title" (lph "title"))
                                <*> ((first show . decodeUtf8') =<< note "Couldn't find url" (lph "url"))
                                <*> note "Couldn't either find or parse the datetimeString." (parseDateTimeString . B8.unpack =<< lph "pubdate")
                                <*> pure Nothing
                                <*> pure Nothing
                                <*> pure Nothing)

                  st { msEpisodes = mep : msEpisodes st, partHash = M.empty }
              ("title",   ParsingTitle)        -> st { currState = ParsingItem }
              ("title",   ParsingPodcastTitle) -> st { currState = Other }
              ("pubDate", ParsingPubDate)      -> st { currState = ParsingItem }
              _                                -> st

        handleCData :: B.ByteString -> ParseState ()
        handleCData = const $ return ()

-- | Parse a datetime string and turn it into a UTCTime.
parseDateTimeString :: String -> Maybe UTCTime
parseDateTimeString = parseTimeM False defaultTimeLocale "%a, %d %b %Y %T %z"

main :: IO ()
main = do
    args <- getArgs
    manager <- newManager defaultManagerSettings
    let p = newPodcast $ T.pack $ head args
    print =<< fetchPodcast p manager
