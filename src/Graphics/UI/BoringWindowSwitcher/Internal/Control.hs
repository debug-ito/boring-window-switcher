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
import Control.Monad (mapM, guard, filterM)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (listToMaybe)
import qualified Graphics.X11.Xlib as Xlib
import qualified Graphics.X11.Xlib.Extras as XlibE

newtype Control = Control { controlDisplay :: Xlib.Display }

withControl :: (Control -> IO a) -> IO a
withControl = bracket (Control <$> Xlib.openDisplay "") (Xlib.closeDisplay . controlDisplay)

data Window = Window { windowID :: !Xlib.Window,
                       windowName :: !String
                     } deriving (Show,Eq,Ord)

toOurWindow :: Xlib.Display -> Xlib.Window -> IO Window
toOurWindow disp wid = Window wid <$> xGetWindowName disp wid

withDefaultOf :: Functor m => a -> MaybeT m a -> m a
withDefaultOf def_value = (fmap $ maybe def_value id) . runMaybeT

-- | c.f. @xdo_get_window_name@ function in libxdo-2.
xGetWindowName :: Xlib.Display -> Xlib.Window -> IO String
xGetWindowName disp win = withDefaultOf "" $ propAt "_NET_WM_NAME" <|> propAt "WM_NAME"
  where
    propAt prop_name_str = do
      prop_name_atom <- liftIO $ Xlib.internAtom disp prop_name_str False
      tprop <- liftIO $ XlibE.getTextProperty disp win prop_name_atom
      guard (XlibE.tp_nitems tprop > 0)
      MaybeT $ listToMaybe <$> XlibE.wcTextPropertyToTextList disp tprop

selectableWindows :: Control -> IO [Window]
selectableWindows cont = mapM (toOurWindow disp) =<< xSelectableWindows disp where
  disp = controlDisplay cont

nothingToExcept :: String -> MaybeT IO a -> IO a
nothingToExcept error_message m = maybe (fail error_message) return =<< runMaybeT m

-- | c.f. @getWindowList@ function in
-- https://github.com/debug-ito/numpaar/blob/master/src/window_utils.c
-- , which is based on the source code of wmctrl and libxdo-2.
xSelectableWindows :: Xlib.Display -> IO [Xlib.Window]
xSelectableWindows disp = filterM (xIsWindowForPager disp) =<< allWindows
  where
    allWindows = nothingToExcept "Cannot obtain X client list."
                 $ winsFor "_NET_CLIENT_LIST_STACKING"
                 <|> winsFor "_NET_CLIENT_LIST"
                 <|> winsFor "_WIN_CLIENT_LIST"
    winsFor request = do
      req_atom <- liftIO $ Xlib.internAtom disp request False
      raw_list <- (fmap . map) fromIntegral $ MaybeT $ XlibE.getWindowProperty32 disp req_atom (Xlib.defaultRootWindow disp)
      return $ reverse raw_list

-- | c.f. @isWindowForPager@ function in
-- https://github.com/debug-ito/numpaar/blob/master/src/interpreter.c
xIsWindowForPager :: Xlib.Display -> Xlib.Window -> IO Bool
xIsWindowForPager disp win = impl where
  impl = do
    state_req <- Xlib.internAtom disp "_NET_WM_STATE" False
    mret <- XlibE.getWindowProperty32 disp state_req win
    case mret of
      Nothing -> return True
      Just raw_atoms -> do
        let state_atoms = map fromIntegral raw_atoms
        skip_atom <- Xlib.internAtom disp "_NET_WM_STATE_SKIP_PAGER" False
        return $ not $ skip_atom `elem` state_atoms

raiseWindow :: Control -> Window -> IO ()
raiseWindow = undefined

