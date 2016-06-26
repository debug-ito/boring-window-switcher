-- |
-- Module: Graphics.UI.BoringWindowSwitcher.Internal.Dialog
-- Description: Dialog UI. 
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- This is an internal module. End-users should not use this.
module Graphics.UI.BoringWindowSwitcher.Internal.Dialog
       ( createDialog
       ) where

import Control.Monad (void)
import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk (AttrOp((:=)))

import Graphics.UI.BoringWindowSwitcher.Internal.Control (Window)

createDialog :: [Window] -> IO Gtk.Window
createDialog wins = do
  dialog_window <- Gtk.windowNew
  Gtk.set dialog_window [Gtk.windowTitle := "Boring Window Switcher"]
  wlist <- createWindowList wins
  Gtk.containerAdd dialog_window wlist
  return dialog_window

createWindowList :: [Window] -> IO Gtk.TreeView
createWindowList wins = impl where
  impl = do
    model <- Gtk.listStoreNew wins
    view <- Gtk.treeViewNewWithModel model
    void $ Gtk.treeViewAppendColumn view =<< makeWinNameColumn
    return view
  makeWinNameColumn = do
    col <- Gtk.treeViewColumnNew
    Gtk.set col [Gtk.treeViewColumnTitle := "Window Name"]
    renderer <- Gtk.cellRendererTextNew
    Gtk.treeViewColumnPackStart col renderer False
    return col
  
  
