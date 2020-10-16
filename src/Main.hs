import qualified GI.Gtk as Gtk

import Control.Monad.State (evalStateT)

import Models
import StartGameScreen

main :: IO ()
main = do
  _ <- Gtk.init Nothing
  showStartScreen 
  Gtk.main
 

