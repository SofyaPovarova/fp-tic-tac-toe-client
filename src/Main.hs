import qualified GI.Gtk as Gtk

import StartGameScreen

main :: IO ()
main = do
  _ <- Gtk.init Nothing
  showStartScreen
  Gtk.main
 

