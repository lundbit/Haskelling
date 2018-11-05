import Test.Hspec
import Lib
import Data
import Data.Maybe

game = makeGame grid family
grid' = gameGrid game

gwc = gridWithCoords grid

test FindWord word =
  let (Just result) = findWord gwc word
      string = map cell2char result
  in string `shouldBe` Just word

main :: IO ()
main = hspec $ do
  describe "formatGrid" $ do
    it "should concatenate every line with a newline" $ do
      (formatGrid (gridWithCoords ["abc", "def", "ghi"]) `shouldBe` "abc\ndef\nghi\n"

  describe "findWord" $ do
    it "Should find words that exist on the grid" $ do
      testFindWord "CARSON"
      testFindWord "DAX"
    it "Should not find words that do not exist on the grid" $ do
      findWord gwc "FRENCH" `shouldBe` Nothing

  describe "findWords" $ do
    it "should find all the words in Grid" $ do
      findWords grid family `shouldBe` family


testFindWord grid word = do
  let found = fromJust (findWord grid word)
  cells2string found `shouldBe` word
