-- |
-- Module: Graphics.UI.BoringWindowSwitcher.Dialog
-- Description: Dialog UI. 
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Graphics.UI.BoringWindowSwitcher.Dialog
       ( createDialog
       ) where

import Graphics.UI.BoringWindowSwitcher.Control (Window)

data Dialog

createDialog :: [Window] -> IO Dialog
createDialog = undefined

