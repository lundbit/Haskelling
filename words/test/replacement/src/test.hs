
import Control.Monad (guard)

data Cell = Cell (Integer, Interger) Char deriving (Eq, Ord, Show)

-- sample coordiantes grid
coords =    [(0,0),(0,1),(0,2),(0,3),(0,4),(0,5),(0,6),(0,7)]
            [(1,0),(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(1,7)]
            [(2,0),(2,1),(2,2),(2,3),(2,4),(2,5),(2,6),(2,7)]
            [(3,0),(3,1),(3,2),(3,3),(3,4),(3,5),(3,6),(3,7)]
            [(4,0),(4,1),(4,2),(4,3),(4,4),(4,5),(4,6),(4,7)]
            [(5,0),(5,1),(5,2),(5,3),(5,4),(5,5),(5,6),(5,7)]
            [(6,0),(6,1),(6.2),(6,3),(6,4),(6,5),(6,6),(6,7)]
            [(7,0),(7,1),(7,2),(7,3),(7,4),(7,5),(7,6),(7,7)]

--copy of grid word
grid = [    "AMY"
            , "ANGUS"
            , "BRAD"
            , "CARSON"
            , "CLAIRE"
            , "DAX"
            , "GARCIA"
            , "MYERS"
            , "SCARLETT"
            ]

-- variant of output grid
og :: Show a => [a] -> IO ()
og = putStrLn . unlines . map Show

-- check if divisible by 2
div2 x = x 'mod' 2 ==0

-- monads
mapped = do
    i <- [0..]
    guard (div2 i)
    return i

mappedAndFiltered = do
    i <- [0..]
    guard (div2 i)
    return (i+1)

    
coords2 = do
    row <- [0..7]
    return $ do
        col <- [0..7]
        return (row,col)

cols = repeat [0..]
rows = map repeat [0..]
coordsInf = zipOverGrid rows cols

repeat8 = take 8 . repeat
cols8 = repeat8 [0..7]
rows8 = map repeat8 [0..7]

zipOverGrid = zipWidth zip
zipOverGridWith = zipWith . zipWith 

grid8 = zipOverGrid rows8 cols8

-- zipOverGrid coordsInf grid   <---- stage one result from trying to match coordinates with target wordsearch text

-- zipOverGridWith Cell coords grid    <--- stage two result that matches cells with target words






