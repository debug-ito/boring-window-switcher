module Graphics.UI.BoringWindowSwitcher.ControlSpec (main,spec) where

import System.IO (stderr, hPutStrLn)
import Test.Hspec

import Graphics.UI.BoringWindowSwitcher.Control (withControl, selectableWindows)


main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Control" $ do
  describe "selectableWindows" $ do
    it "returns one or more Windows" $ withControl $ \control -> do
      win_list <- selectableWindows control
      hPutStrLn stderr $ show win_list
      length win_list `shouldSatisfy` (>= 1)
