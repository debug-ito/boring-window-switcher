module Graphics.UI.BoringWindowSwitcher.Internal.ControlSpec (main,spec) where

import System.IO (stderr, hPutStrLn)
import Test.Hspec

import Graphics.UI.BoringWindowSwitcher.Internal.Control
  ( withControl, selectableWindows,
    raiseWindow
  )


main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Control" $ do
  describe "selectableWindows" $ do
    it "returns one or more Windows" $ withControl $ \control -> do
      win_list <- selectableWindows control
      hPutStrLn stderr $ show win_list
      length win_list `shouldSatisfy` (>= 1)
  describe "raiseWindow" $ do
    it "raises the selected window" $ withControl $ \control -> do
      win_list <- selectableWindows control
      case win_list of
        (_ : second_win : _) -> do
          hPutStrLn stderr ("Raise " ++ show second_win)
          raiseWindow control second_win
        _ -> do
          hPutStrLn stderr ("Not enough windows. Skipped.")
      
