-- |
-- Module: Graphics.UI.BoringWindowSwitcher
-- Description: main module behind boring-window-switcher
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
-- 
module Graphics.UI.BoringWindowSwitcher
       (main) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Graphics.UI.BoringWindowSwitcher.Internal.Control
  ( withControl, selectableWindows, raiseWindow
  )
import Graphics.UI.BoringWindowSwitcher.Internal.Dialog (createDialog)

import qualified Graphics.UI.Gtk as Gtk

-- | Entry point of @boring-window-switcher@ executable.
main :: IO ()
main = do
  void $ Gtk.initGUI
  withControl $ \control -> do
    wins <- selectableWindows control
    dialog <- createDialog wins $ \selected_window -> do
      raiseWindow control selected_window
      Gtk.mainQuit
    void $ Gtk.on dialog Gtk.deleteEvent $ do
      liftIO $ Gtk.mainQuit
      return False
    Gtk.widgetShowAll dialog
    Gtk.mainGUI
