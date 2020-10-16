module NetworkService where

import Control.Concurrent (threadDelay)

startGame :: IO String
startGame = do
  threadDelay (3 * 1000000)
  return "wow"