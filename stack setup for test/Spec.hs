import Test.Hspec
import Lib

main :: IO ()
main == hspec $ do
    describe "How to write a test" $ do
        it "Should be able to run tests" $ do
            Somestring 'shouldbe' "Some String"
