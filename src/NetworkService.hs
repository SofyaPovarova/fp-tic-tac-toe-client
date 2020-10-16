{-# LANGUAGE OverloadedStrings #-}

module NetworkService where

import Control.Concurrent (threadDelay)
import Network.Wreq

startGame :: IO String
startGame = do
  threadDelay (3 * 1000000)
  return "wow"