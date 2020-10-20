{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Models where

import qualified Data.Map as Map

import GHC.Generics
import Data.Aeson

type SessionId = String

data SessionState = SessionState 
  { stateSessionId :: Maybe SessionId
  , stateGameSession :: GameSession
  }

data PlayerRoleParam = PlayerRoleParam Cell | PlayerRoleRandom

instance Show PlayerRoleParam where
  show (PlayerRoleParam cell) = show cell
  show PlayerRoleRandom = "random"
  

data Cell = X | O
  deriving (Eq, Generic)

instance Show Cell where
  show = \case
    X -> "x"
    O -> "o"

instance FromJSON Cell where
  parseJSON = withText "Cell" $
    \case
      "x" -> pure X
      "o" -> pure O
      _ -> fail $ "Unknown Cell value"

data GameResult = CrossesWin | NoughtsWin | Draw
  deriving (Eq, Generic)

instance Show GameResult where
    show = \case
      CrossesWin -> "x"
      NoughtsWin -> "o"
      Draw -> "draw"

instance FromJSON GameResult where
  parseJSON = withText "GameResult" $ 
    \case
      "x" -> pure CrossesWin
      "o" -> pure NoughtsWin
      "draw" -> pure Draw
      _ -> fail $ "Unknown GameResult value"

toGameResult :: Cell -> GameResult
toGameResult =
  \case
    X -> CrossesWin
    O -> NoughtsWin


data Field = Field
  { fieldCells :: Map.Map (Int, Int) Cell
  , fieldSize :: Int
  } deriving (Show)

instance FromJSON Field where
  parseJSON = withObject "Field" $ \o -> do
    fieldCells <- o .: "cells"
    fieldSize <- o .: "size"
    return Field{..}

data GameSession = GameSession
  { gsField :: Field
  , gsPlayerRole :: Cell
  , gsGameResult :: Maybe GameResult
  } deriving (Show, Generic)

instance FromJSON GameSession where
  parseJSON = withObject "GameSession" $ \o -> do
    gsField <- o .: "field"
    gsPlayerRole <- o .: "playerRole"
    gsGameResult <- o .: "gameResult"
    return GameSession{..}
    