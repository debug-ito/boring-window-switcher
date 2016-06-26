-- |
-- Module: Graphics.UI.BoringWindowSwitcher.Internal.Dialog
-- Description: Dialog UI. 
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- This is an internal module. End-users should not use this.
module Graphics.UI.BoringWindowSwitcher.Internal.Dialog
       ( createDialog
       ) where

import Graphics.UI.BoringWindowSwitcher.Internal.Control (Window)

data Dialog

createDialog :: [Window] -> IO Dialog
createDialog = undefined

