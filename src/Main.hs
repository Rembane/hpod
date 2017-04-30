{-# LANGUAGE OverloadedStrings #-}

-- TODO: Logging
-- TODO: Better error handling
-- TODO: Etags

module Main where

import qualified Data.Text as T
import Network.HTTP.Client (defaultManagerSettings, newManager)
import System.Environment (getArgs)

import Podcast

main :: IO ()
main = do
    args <- getArgs
    manager <- newManager defaultManagerSettings
    let p = newPodcast $ T.pack $ head args
    print =<< fetchPodcast p manager
