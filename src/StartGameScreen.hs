{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StartGameScreen 
  ( showStartScreen
  ) where

import qualified GI.Gtk as Gtk

import qualified NetworkService

import Data.Text (pack)
import Control.Concurrent.Async (waitCatch)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import GHC.Float (double2Int)

import Models
import Utils
import GameScreen

showStartScreen :: Gtk.Application -> IO ()
showStartScreen app = do
  let gladeFile = "res/login-screen.glade"
  builder <- Gtk.builderNewFromFile (pack gladeFile)

  window <- builderGetObject Gtk.Window builder "window"
  fieldSizePicker <- builderGetObject Gtk.SpinButton builder "field-size-picker"
  buttonRoleCrosses <- builderGetObject Gtk.RadioButton builder "button-role-crosses"
  buttonRoleNoughts <- builderGetObject Gtk.RadioButton builder "button-role-noughts"
  buttonRoleRandom <- builderGetObject Gtk.RadioButton builder "button-role-random"
  winLineLengthPicker <- builderGetObject Gtk.SpinButton builder "win-line-length-picker"
  startGameButton <- builderGetObject Gtk.Button builder "start-game-button"
  errorLabel <- builderGetObject Gtk.Label builder "error-label"

  roleRef <- newIORef $ PlayerRoleParam X

  setChangeRoleCallback buttonRoleCrosses (PlayerRoleParam X) roleRef
  setChangeRoleCallback buttonRoleNoughts (PlayerRoleParam O) roleRef
  setChangeRoleCallback buttonRoleRandom (PlayerRoleRandom) roleRef

  _ <- Gtk.onButtonClicked startGameButton $ do
    Gtk.widgetSetSensitive startGameButton False
    fieldSizeValue <- Gtk.spinButtonGetValue fieldSizePicker
    role <- readIORef roleRef
    winLineLengthValue <- Gtk.spinButtonGetValue winLineLengthPicker
    if (winLineLengthValue > fieldSizeValue)
      then do
        Gtk.widgetShow errorLabel
        Gtk.labelSetLabel errorLabel $ "Win line length shouldn't be more than field size!"
      else do
        let req = NetworkService.startGame (double2Int fieldSizeValue) role (double2Int winLineLengthValue)
        asyncRequest req $ \aSession -> do
          mSession <- waitCatch aSession
          doOnMainThread $ do
            case mSession of
              Right (Right (sessionId, initialState)) -> do
                Gtk.widgetHide errorLabel
                showGameScreen app sessionId initialState
                Gtk.windowClose window
              Right (Left err) -> do
                Gtk.widgetShow errorLabel
                Gtk.labelSetLabel errorLabel $ pack $ show err
              Left e -> do
                Gtk.widgetShow errorLabel
                setErrorLabel errorLabel e
    Gtk.widgetSetSensitive startGameButton True
    return ()

  Gtk.widgetShowAll window
  Gtk.widgetHide errorLabel
  Gtk.applicationAddWindow app window

setChangeRoleCallback :: Gtk.RadioButton -> PlayerRoleParam -> IORef PlayerRoleParam -> IO ()
setChangeRoleCallback button role ref = do
  _ <- Gtk.afterToggleButtonToggled button $ do
    isButtonActive <- Gtk.toggleButtonGetActive button
    if isButtonActive
      then writeIORef ref role
      else return ()
  return ()
