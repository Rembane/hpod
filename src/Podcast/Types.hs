module Podcast.Types
    ( Episode(..)
    , Podcast(..)
    ) where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Vector (Vector)

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
