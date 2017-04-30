{-# LANGUAGE DeriveGeneric #-}

module Podcast.Types
    ( Episode(..)
    , Podcast(..)
    ) where

import Data.Serialize
import Data.Serialize.Text ()
import Data.Text (Text)
import Data.Time.Calendar (Day(..))
import Data.Time.Clock (UTCTime(..))
import Data.Map.Strict (Map)
import GHC.Generics

type Url = Text

instance Serialize UTCTime where
    get = (\(d, t) -> UTCTime (ModifiedJulianDay d) (fromRational t)) <$> get
    put t = put ((toModifiedJulianDay . utctDay) t, (toRational . utctDayTime) t)

data Episode = Episode
    { epTitle       :: Text
    , epUrl         :: Url
    , pubDate       :: UTCTime
    , downloaded    :: Maybe UTCTime
    , listened      :: Maybe UTCTime
    , localFilename :: Maybe Text
    }
    deriving (Generic, Show)

instance Serialize Episode

data Podcast = Podcast
    { pcTitle     :: Text
    , pcUrl       :: Url
    , episodes    :: Map Url Episode
    , lastChecked :: UTCTime
    }
    deriving (Generic, Show)

instance Serialize Podcast
