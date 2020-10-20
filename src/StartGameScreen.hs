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
import GHC.Float

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

  roleRef <- newIORef $ PlayerRoleParam X

  setChangeRollCallback buttonRoleCrosses (PlayerRoleParam X) roleRef
  setChangeRollCallback buttonRoleNoughts (PlayerRoleParam O) roleRef
  setChangeRollCallback buttonRoleRandom (PlayerRoleRandom) roleRef

  Gtk.onButtonClicked startGameButton $ do
    Gtk.widgetSetSensitive startGameButton False
    fieldSizeValue <- Gtk.spinButtonGetValue fieldSizePicker
    role <- readIORef roleRef
    winLineLengthValue <- Gtk.spinButtonGetValue winLineLengthPicker
    let req = NetworkService.startGame (double2Int fieldSizeValue) role (double2Int winLineLengthValue)
    asyncRequest req $ \aSession -> do
      mSession <- waitCatch aSession
      doOnMainThread $ do
        case mSession of
          Right (Right (sessionId, initialState)) -> do
            showGameScreen sessionId initialState
            Gtk.windowClose window
          Right (Left err) -> putStrLn $ "err: " ++ show err
          Left err -> putStrLn $ "err: " ++ show err
        Gtk.widgetSetSensitive startGameButton True
      return ()

  Gtk.widgetShowAll window

setChangeRollCallback :: Gtk.RadioButton -> PlayerRoleParam -> IORef PlayerRoleParam -> IO ()
setChangeRollCallback button role ref = do
  _ <- Gtk.afterToggleButtonToggled button $ do
    isButtonActive <- Gtk.toggleButtonGetActive button
    if isButtonActive
      then writeIORef ref role
      else return ()
  return ()