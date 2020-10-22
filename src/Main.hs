{-# LANGUAGE OverloadedStrings #-}

import qualified GI.Gtk as Gtk
import qualified GI.Gio as Gio

import Data.Text (Text)

import StartGameScreen

main :: IO ()
main = do
  Just app <- Gtk.applicationNew (Just appId) []
  _ <- Gio.onApplicationActivate app (showStartScreen app)
  _ <- Gio.applicationRun app Nothing
  return ()

appId :: Text
appId = "fp.tic-tac-toe"