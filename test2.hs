
-- add a type declaration for main as IO

main :: IO ()
main = do
    putStrLn "Enter two lines"   -- this is an action
    do
        line1 <- getLine                                -- line1 :: String
        do
            line2 <- getLine                            -- line2 :: String
            putStrLn ("you said: " ++ line1 ++ " and " ++ line2)


-- dont forget to:
-- 1)  Go to command prompt in Windows (after installing ghci)
-- 2)  type: "stack ghci"
-- 3)  make sure this test2 file is in the target directory
--        you can change the directory using :cd <dir>
--        to see what dir you are in, type: :! cd
-- 4) type "":load test2" after "Prelude>"  case matters! 
-- 5) type in "main" to run it
--   check out ":type main"  Its an input Output function with no return value
-- 6) Makie changes?  Type ":reload" after *