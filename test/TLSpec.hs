import Test.Hspec
import System.IO
import TL

main :: IO ()
main = do
  testOne "index"
  testOne "index2"
  testOne "first"
  testOne "prev"
  testOne "next"
  testOne "fby"
  testOne "ybf"

testOne func = do
  textIn <- readFile ("test/in/" ++ func ++ ".tl")
  textOut <- readFile ("test/out/" ++ func ++ ".out")
  hspec $ do
    describe func $ do
      it ("tests the " ++ func ++ " function") $
        processText textIn False False `shouldBe` Right textOut
