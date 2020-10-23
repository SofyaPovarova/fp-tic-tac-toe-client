{-# LANGUAGE MonoLocalBinds #-}

module Utils where

import qualified GI.Gtk as Gtk
import qualified GI.GLib as GLib

import Data.Text (pack)
import Data.Maybe (fromJust)
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (Async, withAsync)
import Control.Exception (SomeException, fromException)
import Network.HTTP.Client

import GI.GObject
import Data.GI.Base

{-
  usage:
    builder <- Gtk.builderNewFromFile (pack gladeFile)
    builderGetObject Gtk.Window builder "window-id"   
-}
builderGetObject ::
  (GI.GObject.GObject b, Gtk.IsBuilder a) =>
  (Data.GI.Base.ManagedPtr b -> b) ->
  a ->
  Prelude.String ->
  IO b
builderGetObject objectTypeClass builder objectId =
  fromJust <$> Gtk.builderGetObject builder (pack objectId) >>=
    Gtk.unsafeCastTo objectTypeClass

doOnMainThread :: IO () -> IO ()
doOnMainThread block = do
  _ <- GLib.idleAdd GLib.PRIORITY_HIGH_IDLE $ do
    block
    return False
  return ()
    
doAsync :: IO () -> IO ()
doAsync block = forkIO block >> return ()

asyncRequest :: IO a -> (Async a -> IO ()) -> IO ()
asyncRequest req block = doAsync $ withAsync req block

setErrorLabel :: Gtk.Label -> SomeException -> IO ()
setErrorLabel errorLabel e = 
  Gtk.labelSetLabel errorLabel $ pack $ 
    case (fromException e) of
      Just (HttpExceptionRequest _ content) -> 
        case content of 
          StatusCodeException _ errText -> "Error: " ++ show errText
          ConnectionFailure e' -> show e'
          _ -> show e
      _ -> show e