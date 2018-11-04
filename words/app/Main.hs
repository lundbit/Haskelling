module Main where

import Lib
import Data

g = makeGame grid family

main :: IO ()
main = playGame g

