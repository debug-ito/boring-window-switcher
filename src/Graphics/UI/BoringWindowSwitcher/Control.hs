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

import Control.Applicative ((<|>), (<$>))
import Control.Exception (bracket)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.IO.Class (liftIO)
import qualified Graphics.X11.Xlib as Xlib
import qualified Graphics.X11.Xlib.Extras as XlibE

newtype Control = Control { controlDisplay :: Xlib.Display }

withControl :: (Control -> IO a) -> IO a
withControl = bracket (Control <$> Xlib.openDisplay "") (Xlib.closeDisplay . controlDisplay)

data Window = Window { windowID :: !Xlib.Window
                     } deriving (Show,Eq,Ord)

windowName :: Window -> String
windowName = undefined

toOurWindow :: Xlib.Window -> Window
toOurWindow = Window

activeWindows :: Control -> IO [Window]
activeWindows = ((fmap . fmap) toOurWindow) . xActiveWindows . controlDisplay

-- | c.f. @getWindowList@ function in
-- https://github.com/debug-ito/numpaar/blob/master/src/window_utils.c
-- , which is based on the source code of wmctrl and libxdo-2.
xActiveWindows :: Xlib.Display -> IO [Xlib.Window]
xActiveWindows disp = nothingToExcept
                      $ wins "_NET_CLIENT_LIST_STACKING"
                      <|> wins "_NET_CLIENT_LIST"
                      <|> wins "_WIN_CLIENT_LIST"
  where
    wins request = do
      req_atom <- liftIO $ Xlib.internAtom disp request False
      (fmap . map) fromIntegral $ MaybeT $ XlibE.getWindowProperty32 disp req_atom (Xlib.defaultRootWindow disp)
    nothingToExcept m = maybe (fail "Cannot obtain X client list.") return =<< runMaybeT m

raiseWindow :: Control -> Window -> IO ()
raiseWindow = undefined

