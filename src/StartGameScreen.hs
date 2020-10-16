{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StartGameScreen where

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
import GameScreen

showStartScreen :: IO ()
showStartScreen = do
  setupStyles "res/login-screen.css"

  let gladeFile = "res/login-screen.glade"
  builder <- Gtk.builderNewFromFile (pack gladeFile)

  window <- builderGetObject Gtk.Window builder "window"
  fieldSizePicker <- builderGetObject Gtk.SpinButton builder "field-size-picker"
  roleButtonGroup <- builderGetObject Gtk.ButtonBox builder "role-button-group"
  buttonRoleCrosses <- builderGetObject Gtk.RadioButton builder "button-role-crosses"
  buttonRoleNoughts <- builderGetObject Gtk.RadioButton builder "button-role-noughts"
  buttonRoleRandom <- builderGetObject Gtk.RadioButton builder "button-role-random"
  winLineLengthPicker <- builderGetObject Gtk.SpinButton builder "win-line-length-picker"
  startGameButton <- builderGetObject Gtk.Button builder "start-game-button"

  Gtk.onButtonClicked startGameButton $ do
    Gtk.widgetSetSensitive startGameButton False
    asyncRequest NetworkService.startGame $ \pSession -> do
      mSessionId <- waitCatch pSession
      doOnMainThread $ do
        case mSessionId of
          Right sessionId -> do
            showGameScreen sessionId
            Gtk.windowClose window
          Left err -> putStrLn $ "err: " ++ show err
        Gtk.widgetSetSensitive startGameButton True
      return ()

  Gtk.widgetShowAll window
