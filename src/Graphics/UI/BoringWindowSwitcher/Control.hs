-- |
-- Module: Graphics.UI.BoringWindowSwitcher.Control
-- Description: Controls windows.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Graphics.UI.BoringWindowSwitcher.Control
       ( Control, withControl,
         Window, windowName, activeWindows, raiseWindow
       ) where

data Control

withControl :: (Control -> IO a) -> IO a
withControl = undefined

data Window

windowName :: Window -> String
windowName = undefined

activeWindows :: Control -> IO [Window]
activeWindows = undefined

raiseWindow :: Control -> Window -> IO ()
raiseWindow = undefined

