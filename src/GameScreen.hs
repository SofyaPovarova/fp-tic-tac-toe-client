{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module GameScreen 
  ( showGameScreen
  ) where

import qualified GI.Gtk as Gtk

import qualified NetworkService

import Data.Text (pack)
import Control.Concurrent.Async (waitCatch)
import Data.Maybe ( fromJust)
import Lens.Micro
import Data.Foldable (for_)
import Data.Map ((!?))
import Data.Int (Int32)
import Network.HTTP.Client
import Control.Exception

import Models
import Utils
import {-# SOURCE #-} StartGameScreen

showGameScreen :: Gtk.Application -> String -> GameState -> IO ()
showGameScreen app sessionId initialState = do
  setupStyles "res/game-screen.css"

  let gladeFile = "res/game-screen.glade"
  builder <- Gtk.builderNewFromFile (pack gladeFile)

  window <- builderGetObject Gtk.Window builder "window"
  gridContainer <- builderGetObject Gtk.Box builder "game-container"
  statusLabel <- builderGetObject Gtk.Label builder "status-label"
  restartButton <- builderGetObject Gtk.Button builder "restart-button"

  _ <- Gtk.onButtonClicked restartButton $ do
    showStartScreen app
    Gtk.windowClose window

  grid <- renderField sessionId (initialState^.gsField) statusLabel restartButton

  Gtk.containerAdd gridContainer grid
  Gtk.widgetShowAll window
  Gtk.widgetHide restartButton
  Gtk.applicationAddWindow app window

renderField :: SessionId -> Field -> Gtk.Label -> Gtk.Button -> IO Gtk.Grid
renderField sessionId field statusLabel restartButton = do
  let cells = field^.fieldCells
  let gridSize = field^.fieldSize
  let range = [0..gridSize - 1]
  let indices = [(x, y) | x <- range, y <- range]

  grid <- Gtk.gridNew
  
  for_ range $ \i -> do
    Gtk.gridInsertRow grid $ fromIntegral i

  for_ range $ \i -> do
    Gtk.gridInsertColumn grid $ fromIntegral i

  for_ indices $ \point@(i, j) -> do
    let mCell = cells !? point
    cellButton <- makeCellButton mCell

    _ <- Gtk.onButtonClicked cellButton $ do
      let req = NetworkService.move sessionId i j
      asyncRequest req $ \aGameState -> do
        mGameState <- waitCatch aGameState
        doOnMainThread $
          case mGameState of
            Right gameState -> renderGameState grid statusLabel restartButton gameState
            Left e -> do
              Gtk.labelSetLabel statusLabel $ pack $ 
                case (fromException e) of
                  Just (HttpExceptionRequest _ content) -> 
                    case content of 
                      StatusCodeException _ errText -> "Error: " ++ show errText
                      ConnectionFailure e' -> show e'
                      _ -> show e
                  _ -> show e
        return ()

    Gtk.gridAttach
      grid
      cellButton
      (fromIntegral i)
      (fromIntegral j)
      (1 :: Int32)
      (1 :: Int32)

  return grid

renderGameState :: Gtk.Grid -> Gtk.Label -> Gtk.Button -> Either String GameState -> IO ()
renderGameState grid statusLabel restartButton =
  \case
    Right gameState -> do
      renderButtons (gameState^.gsField) grid

      case (gameState^.gsGameResult) of
        Just result -> do
          Gtk.labelSetLabel statusLabel $ pack ("win status: " ++ show result)
          Gtk.widgetSetSensitive grid False
          Gtk.widgetShow restartButton
        Nothing -> do
          Gtk.labelSetLabel statusLabel "Your turn!"
        
    Left err -> 
      Gtk.labelSetLabel statusLabel $ 
        pack ("Request error: " ++ show err)

renderButtons :: Field -> Gtk.Grid -> IO ()
renderButtons field grid = do
  let cells = field^.fieldCells
  let gridSize = field^.fieldSize
  let range = [0..gridSize - 1]
  let indices = [(x, y) | x <- range, y <- range]

  for_ indices $ \point@(i, j) -> do
    cellButtonRaw <- fromJust <$> Gtk.gridGetChildAt grid (fromIntegral i) (fromIntegral j)
    cellButton <- Gtk.unsafeCastTo Gtk.Button cellButtonRaw
    Gtk.buttonSetLabel cellButton $ 
      case cells !? point of
        Just cell -> pack $ show cell
        Nothing -> ""

makeCellButton :: Maybe Cell -> IO Gtk.Button
makeCellButton mCell =
  Gtk.buttonNewWithLabel $ 
    case mCell of
      Just cell -> pack $ show cell
      Nothing -> ""