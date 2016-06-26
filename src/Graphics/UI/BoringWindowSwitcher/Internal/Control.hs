-- |
-- Module: Graphics.UI.BoringWindowSwitcher.Internal.Control
-- Description: Controls windows.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- This is an internal module. End-users should not use this.
module Graphics.UI.BoringWindowSwitcher.Internal.Control
       ( Control, withControl,
         Window, windowName, selectableWindows, raiseWindow
       ) where

import Control.Applicative ((<|>), (<$>))
import Control.Exception (bracket)
import Control.Monad (mapM)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.IO.Class (liftIO)
import qualified Graphics.X11.Xlib as Xlib
import qualified Graphics.X11.Xlib.Extras as XlibE

newtype Control = Control { controlDisplay :: Xlib.Display }

withControl :: (Control -> IO a) -> IO a
withControl = bracket (Control <$> Xlib.openDisplay "") (Xlib.closeDisplay . controlDisplay)

data Window = Window { windowID :: !Xlib.Window,
                       windowName :: !String
                     } deriving (Show,Eq,Ord)

toOurWindow :: Xlib.Window -> IO Window
toOurWindow wid = Window wid <$> xGetWindowName wid

xGetWindowName :: Xlib.Window -> IO String
xGetWindowName = undefined

selectableWindows :: Control -> IO [Window]
selectableWindows = (mapM toOurWindow =<<) . xSelectableWindows . controlDisplay

-- | c.f. @getWindowList@ function in
-- https://github.com/debug-ito/numpaar/blob/master/src/window_utils.c
-- , which is based on the source code of wmctrl and libxdo-2.
xSelectableWindows :: Xlib.Display -> IO [Xlib.Window]
xSelectableWindows disp = nothingToExcept
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

