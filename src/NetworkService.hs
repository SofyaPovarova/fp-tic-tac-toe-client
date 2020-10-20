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
  
  return $ case decodedBody of
    Just session -> Right $ (sessionHeader, session)
    Nothing -> Left $ "request to /startGame failed with code " ++ (show $ result^.responseStatus.statusCode)
  
move :: SessionId -> Int -> Int -> IO (Either String GameSession)
move sessionId x y = do
  let opts = defaults & param "x" .~ [pack $ show x]
                      & param "y" .~ [pack $ show y]
                      & header "Session" .~ [BS.pack sessionId]

  result <- postWith opts (baseUrl ++ "/move") (BS.pack "")

  let body = result ^. responseBody
  let decodedBody = decode body :: Maybe GameSession

  return $ case decodedBody of
    Just session -> Right session
    Nothing -> Left $ "request to /move failed with code " ++ (show $ result^.responseStatus.statusCode)