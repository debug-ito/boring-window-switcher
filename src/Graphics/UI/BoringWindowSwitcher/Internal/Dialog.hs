-- |
-- Module: Graphics.UI.BoringWindowSwitcher.Internal.Dialog
-- Description: Dialog UI. 
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- This is an internal module. End-users should not use this.
module Graphics.UI.BoringWindowSwitcher.Internal.Dialog
       ( createDialog
       ) where

import Control.Monad (void, guard, when)
import Data.Maybe (listToMaybe)
import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk (AttrOp((:=)))

import Graphics.UI.BoringWindowSwitcher.Internal.Control
  ( Window, windowName
  )

createDialog :: [Window] -> (Window -> IO ()) -> IO Gtk.Window
createDialog wins on_selected = do
  dialog_window <- Gtk.windowNew
  Gtk.set dialog_window [Gtk.windowTitle := "Boring Window Switcher"]
  wlist <- createWindowList wins $ \selected_window -> do
    on_selected selected_window
    Gtk.widgetDestroy dialog_window
  Gtk.containerAdd dialog_window wlist
  return dialog_window

createWindowList :: [Window] -> (Window -> IO ()) -> IO Gtk.TreeView
createWindowList wins on_selected = impl where
  impl = do
    model <- Gtk.listStoreNew wins
    view <- Gtk.treeViewNewWithModel model
    void $ Gtk.treeViewAppendColumn view =<< makeWinNameColumn model
    void $ Gtk.on view Gtk.rowActivated $ \path _ ->
      maybe (return ()) on_selected $ getRowItem wins path
    when (length wins > 0) $ Gtk.treeViewSetCursor view [(min 1 (length wins - 1))] Nothing
    return view
  makeWinNameColumn win_model = do
    col <- Gtk.treeViewColumnNew
    Gtk.set col [Gtk.treeViewColumnTitle := "Window Name"]
    renderer <- Gtk.cellRendererTextNew
    -- see https://wiki.haskell.org/Gtk2Hs/Tutorials/TreeView
    Gtk.cellLayoutPackStart col renderer False
    Gtk.cellLayoutSetAttributes col renderer win_model $ \win ->
      [Gtk.cellText := windowName win]
    return col
  getRowItem rows indices = do
    index <- listToMaybe indices
    guard (0 <= index && index < length rows)
    return $ rows !! index
  
  
