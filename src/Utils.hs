{-# LANGUAGE MonoLocalBinds #-}

module Utils where

import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import qualified GI.GLib as GLib

import Data.Text (pack)
import Data.Word (Word32)
import Data.Maybe (fromJust)
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (Async, withAsync)

import GI.GObject
import Data.GI.Base

setupStyles :: String -> IO ()
setupStyles styleFilePath = do
  maybeScreen <- Gdk.screenGetDefault
  cssProvider <- Gtk.cssProviderNew
  Gtk.cssProviderLoadFromPath cssProvider (pack styleFilePath)
  Gtk.styleContextAddProviderForScreen
    (fromJust maybeScreen)
    cssProvider
    (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER :: Word32)  
 
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
