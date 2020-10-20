{-# LANGUAGE OverloadedStrings #-}

module NetworkService where

import qualified Data.ByteString.Char8 as BS

import Network.Wreq
import Control.Lens
import Data.Aeson (decode)
import Data.Text (pack)

import Models

baseUrl :: String
baseUrl = "http://localhost:8081"

startGame :: Int -> PlayerRoleParam -> Int -> IO (Either String (SessionId, GameSession))
startGame fieldSize role winLineLength = do
  let opts = defaults & param "fieldSize" .~ [pack $ show fieldSize] 
                      & param "role" .~ [pack $ show role] 
                      & param "winLineLength" .~ [pack $ show winLineLength]
  result <- postWith opts (baseUrl ++ "/startGame") (BS.pack "")
  let body = result ^. responseBody
  let sessionHeader = BS.unpack $ result ^. responseHeader "Session"
  let decodedBody = decode body :: Maybe GameSession
  case decodedBody of
    Just session -> return $ Right $ (sessionHeader, session)
    Nothing -> return $ Left $ "request to /startGame failed with code " ++ (show $ result^.responseStatus.statusCode)
  
  