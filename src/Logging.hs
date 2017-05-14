{-# LANGUAGE OverloadedStrings #-}
module Logging
    ( IsString
    , fromString
    , LogStr
    , ToLogStr
    , toLogStr

    , SugarLogger
    , newLogger
    ) where

import Data.Semigroup ((<>))
import Data.String (IsString, fromString)
import System.Log.FastLogger (LogStr, LogType(..), TimedFastLogger, ToLogStr, defaultBufSize, newTimedFastLogger, toLogStr)
import System.Log.FastLogger.Date (newTimeCache)

-- | A bit nicer logger type
type SugarLogger msg = msg -> IO ()

-- | A log emitter
emitter :: ToLogStr msg => msg -> (msg -> LogStr)
emitter s = (\ft -> (toLogStr ft) <> " " <> (toLogStr s) <> "\n")

newLogger = do
    -- Timelogformat:
    -- http://pubs.opengroup.org/onlinepubs/9699919799/functions/strftime_l.html
    timeCache <- newTimeCache "[%Y-%m-%d %H:%M:%S]"
    (logger, loggerCleanup) <- newTimedFastLogger timeCache (LogStderr defaultBufSize)
    return (logger . emitter, loggerCleanup)
