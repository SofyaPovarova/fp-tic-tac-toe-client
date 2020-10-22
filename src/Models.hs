{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Models where

import qualified Data.Map as Map

import Lens.Micro.TH
import GHC.Generics
import Data.Aeson

type SessionId = String

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
  { _fieldCells :: Map.Map (Int, Int) Cell
  , _fieldSize :: Int
  } deriving (Show)

makeLenses ''Field

instance FromJSON Field where
  parseJSON = withObject "Field" $ \o -> do
    _fieldCells <- o .: "cells"
    _fieldSize <- o .: "size"
    return Field{..}

data GameState = GameState 
  { _gsField :: Field
  , _gsGameResult :: Maybe GameResult
  } deriving (Show)

makeLenses ''GameState

instance FromJSON GameState where
  parseJSON = withObject "GameState" $ \o -> do
    _gsField <- o .: "field"
    _gsGameResult <- o .: "gameResult"
    return GameState{..}
    
data SessionState = SessionState 
  { stateSessionId :: Maybe SessionId
  , stateGameSession :: GameState
  }