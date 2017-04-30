{-# LANGUAGE OverloadedStrings #-}

-- TODO: Logging
-- TODO: Better error handling
-- TODO: Etags

module Main where

import qualified Data.Map as M
import qualified Data.Text as T
import Network.HTTP.Client (defaultManagerSettings, newManager)
import System.Environment (getArgs)

import Podcast
import Podcast.Types

main :: IO ()
main = do
    args <- getArgs
    manager <- newManager defaultManagerSettings
    let p = newPodcast $ T.pack $ head args

    p' <- fetchPodcast p manager
    case p' of
      Left err -> print err
      Right p'' -> do
          print p'
          print =<< downloadEpisode "podcasts" p'' (head $ M.elems $ episodes p'') manager

