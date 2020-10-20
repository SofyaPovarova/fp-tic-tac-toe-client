{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GameScreen where

import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import qualified GI.Gio as Gio
import qualified GI.GLib as GLib
import qualified Data.Text as Text

import qualified NetworkService

import Data.Text (Text, pack, unpack)
import Control.Concurrent.Async (waitCatch)
import Data.Maybe (fromMaybe)
import Data.IORef

import Models
import Utils

showGameScreen :: String -> GameSession -> IO ()
showGameScreen sessionId initialState = do
  setupStyles "res/game-screen.css"

  let gladeFile = "res/game-screen.glade"
  builder <- Gtk.builderNewFromFile (pack gladeFile)

  window <- builderGetObject Gtk.Window builder "window"

  print sessionId
  print initialState

  Gtk.widgetShowAll window

