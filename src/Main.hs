{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Text (Text, pack, unpack)
import Data.Word
import Data.Maybe (fromJust)
import qualified Data.Text as Text
import Control.Concurrent (threadDelay, forkIO)
import System.Random (randomRIO)
import Text.Read (readMaybe)
import Control.Monad (unless)

import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import qualified GI.Gio as Gio
import qualified GI.GLib as GLib
import GI.GObject
import Data.GI.Base

main :: IO ()
main = do
  _ <- Gtk.init Nothing

  let gladeFile = "res/login-screen.glade"  
  builder <- Gtk.builderNewFromFile (pack gladeFile)
  
  maybeScreen <- Gdk.screenGetDefault
  cssProvider <- Gtk.cssProviderNew
  let styleFile = "res/login-screen.css"
  Gtk.cssProviderLoadFromPath cssProvider (pack styleFile)
  Gtk.styleContextAddProviderForScreen
    (fromJust maybeScreen)
    cssProvider
    (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER :: Word32)

  window <- builderGetObject Gtk.Window builder "window"
  loginField <- builderGetObject Gtk.Entry builder "login-field"
  passwordField <- builderGetObject Gtk.Entry builder "password-field"
  loginButton <- builderGetObject Gtk.Button builder "login-button"
  loginStatus <- builderGetObject Gtk.Label builder "login-status"

  Gtk.onButtonClicked loginButton $ do
    _ <- forkIO $ do
      loginText <- Gtk.entryGetText loginField
      passwordText <- Gtk.entryGetText passwordField
      response <- login (unpack loginText) (unpack passwordText)
      _ <- GLib.idleAdd GLib.PRIORITY_HIGH_IDLE $ do
        _ <- Gtk.labelSetText loginStatus (pack response)
        return False
      return ()
    return ()

  Gtk.widgetShowAll window
  Gtk.main
  
login :: String -> String -> IO String
login username password = do
  threadDelay (3 * 1000000)
  return $ "username: " ++ username ++ ", password: " ++ password

builderGetObject ::
  (GI.GObject.GObject b, Gtk.IsBuilder a) =>
  (Data.GI.Base.ManagedPtr b -> b) ->
  a ->
  Prelude.String ->
  IO b
builderGetObject objectTypeClass builder objectId =
  fromJust <$> Gtk.builderGetObject builder (pack objectId) >>=
    Gtk.unsafeCastTo objectTypeClass

appId :: Text
appId = Text.pack "io.serokell.gui-haskell-app"
