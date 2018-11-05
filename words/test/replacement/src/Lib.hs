module Lib
    ( Game(gameGrid, gameWords)
    , makeGame
    , makeRandomGrid
    , fillInBlanks 
    , score
    , completed
    , totalWords
    , playWord
    , Grid
    , Cell(Cell, Empty)
    , formatGrid
    , formatGame
    , formatGameGrid
    , outputgrid
    , findWord
    , findWordInLine
    , findWords
    , skew
    , ZipOverGrid
    , ZipOverGridWith
    , cell2char
    , getLines
    , gridWithCoords
    , findWordInCellPrefix
    , cells2string
    , mapOverGrid
    ) where
 
import System.IO
import Data.List (isInfixOf, transpose)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Char (toLower)
import System.Random
import qualified Data.Map as M

data Game = Game { gameGrid  :: Grid Cell
                 , gameWords :: M.Map String (Maybe [Cell])
                 }

type Grid a = [[a]]

data Cell = Cell (Int, Int) Char | Empty
            deriving (Eq, Ord, Show)

zipOverGrid :: Grid a -> Grid b -> Grid (a,b)
zipOverGrid = zipWith zip

zipOverGridWith :: (a -> b -> c) -> Grid a -> Grid b -> Grid c
zipOverGridWith = zipWith . zipWith

mapOverGrid :: (a -> b) -> Grid a -> Grid b
mapOverGrid = map . map  

coordsGrid :: Grid (Integer, Integer)  
coordsGrid =
  let rows = map repeat [0..]
      cols = repeat [0..]
  in zipOverGrid rows cols 

makeGame :: Grid Char -> [String] -> Game
makeGame grid words =
  let gwc  = gridWithCoords grid
      words' = M.fromList $ map (\word -> (word, Nothing)) words
  in Game grid' words'
  
-- instead tuplify word = (word, Nothing) // list = map tuplify words // dict = M.fromList list

totalWords :: Game -> Int
totalWords game = length . M.keys $ gameWords game

score :: Game -> Int
score game = length . catMaybes . M.elems $ gameWords game

completed :: Game -> Bool
completed game = score game == totalWords game

playWord :: Game -> String -> Game
playWord game word | not (M.member word (gameWords game)) = game
playWord game word =
  let grid = gameGrid game
      foundWord = findWord grid word
      newGame = case foundWord of
        Nothing -> game
        Just cs ->
          let words = gameWords game
              words' = M.insert word foundWord words
          in Game grid words'
  in newGame

formatGame :: Game -> String
formatGame game = formatGameGrid game
                              ++ "\n\n"
                              ++ (show $ score game)
                              ++ "/"
                              ++ (show $ totalWords game)

makeRandomGrid gen =
  let (gen1, gen2) = split gen
     row = randomRs ('A','Z') gen1
  in row : makeRandomGrid gen2

fillInBlanks gen grid =
    let r = makeRandomGrid gen 
      fill '_' r = r  
      fill c _ = c
    in zipOverGridWith fill grid r
      

gridWithCoords :: Grid Char -> Grid Cell
gridWithCoords grid = zipOverGridWith Cell coordsGrid grid

outputGrid :: Grid Cell -> IO ()
outputGrid grid = putStrLn (formatGrid grid)

formatGameGrid :: Game ->String
format GameGrid game =
  let grid = gameGrid game
      words = gameWords game :: M.Map String (Maybe [Cell])
      cellSet = concat . catMaybes . M.elems $ dict
      formatCell cell =
        let char = cell2char cell
        in if cell `elem` cellSet then char else toLower char
      charGrid = mapOverGrid formatCell grid
  in unlines charGrid

formatGrid :: Grid Cell -> String
formatGrid grid = unlines . mapOverGrid cell2char

findWord :: Grid Cell -> String -> Maybe [Cell]
findWord grid word =
  let lines = getLines grid
      foundWords = map (findWordInLine word) lines
  in listToMaybe (catMaybes foundWords)

findWords :: Grid Cell -> [String] -> [[Cell]]
findWords grid words =
  let findWord' = findWord grid
      foundWords = map findWord' words
  in catMaybes foundWords

getLines :: Grid Cell -> Grid Cell
getLines grid =
  let horizontal = grid
      vertical = transpose horizontal
      diagonal = diagonalize horizontal
      diagonal' = diagonalize (map reverse horizontal)
      lines = horizontal ++ vertical ++ diagonal ++ diagonal'
  in lines ++ (map reverse lines)

diagonalize :: Grid Cell -> Grid Cell
-- diagonalize grid = transpose (skew grid)
diagonalize = transpose . skew

skew :: Grid Cell -> Grid Cell
skew [] = []
skew (x:xs) = x : skew (map indent xs)
  where indent line = Empty : line

findWordInLine :: String -> [Cell] -> Maybe [Cell]
findWordInLine word line =
  let found = findWordInCellPrefix [] word line
  in case found of
       Nothing -> findWordInLine word (tail line)
       cs@(Just_) -> cs

cell2char :: Cell -> Char
cell2char (Cell _ c) = c
cell2char Empty '?'

cells2string = map cell2char

findWordInCellPrefix :: [Cell] -> String -> [Cell] -> Maybe [Cell]
findWordInCellPrefix acc (x:xs) (c:cs) | x == cell2char c
                                  = findWordInCellPrefix (c : acc) xs cs
findWordInCellPrefix acc []     _ = Just $ reverse acc
findWordInCellPrefix _    _     _ = Nothing
