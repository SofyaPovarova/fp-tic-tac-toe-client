{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module GameScreen where

import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import qualified GI.Gio as Gio
import qualified GI.GLib as GLib
import qualified Data.Text as Text
import qualified Data.Map as Map

import qualified NetworkService

import Data.Text (Text, pack, unpack)
import Control.Concurrent.Async (waitCatch)
import Data.Maybe (fromMaybe, fromJust)
import Data.IORef
import Lens.Micro
import Data.Foldable (for_)
import Data.Map ((!?))
import Data.Int (Int32)

import Models
import Utils

showGameScreen :: String -> GameSession -> IO ()
showGameScreen sessionId initialState = do
  setupStyles "res/game-screen.css"

  let gladeFile = "res/game-screen.glade"
  builder <- Gtk.builderNewFromFile (pack gladeFile)

  window <- builderGetObject Gtk.Window builder "window"

  grid <- renderField sessionId $ initialState^.gsField

  Gtk.containerAdd window grid
  Gtk.widgetShowAll window

renderField :: SessionId -> Field -> IO Gtk.Grid
renderField sessionId field = do
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

    Gtk.onButtonClicked cellButton $ do
      let req = NetworkService.move sessionId i j
      asyncRequest req $ \aGameState -> do
        mGameState <- waitCatch aGameState
        doOnMainThread $ do
          case mGameState of
            Right (Right gameState) -> do
              print gameState
              renderButtons (gameState^.gsField) grid
            Right (Left err) -> putStrLn $ "err: " ++ show err
            Left err -> putStrLn $ "err: " ++ show err
        return ()

    Gtk.gridAttach
      grid
      cellButton
      (fromIntegral i)
      (fromIntegral j)
      (1 :: Int32)
      (1 :: Int32)

  return grid

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
makeCellButton cell =
  Gtk.buttonNewWithLabel $ 
    case cell of
      Just cell -> pack $ show cell
      Nothing -> ""