
import Test.Hspec

import System.Linux.NetInfo

main :: IO ()
main = hspec $ do
	describe "getNetInfo" $ do
		it "won't crash" $ do
			withNetInfo (const $ return ()) `shouldReturn` ()
--		it "returns something" $ do
--			fmap (const ()) getNetInfo `shouldReturn` ()

