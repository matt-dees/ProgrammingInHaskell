module Exercises where

import Data.Char

-- 10.10.1
putStr :: String -> IO ()
putStr s = sequence_ [putChar c | c <- s]

-- 10.10.2
type Board = [Int]

putRow :: Int -> Int -> IO ()
putRow row num = do Prelude.putStr (show row)
                    Prelude.putStr ": "
                    Prelude.putStr (concat (replicate num "* "))

newline = putChar '\n'

putBoard' :: Board -> IO ()
putBoard' xs = putBoardHelper xs 1 

putBoardHelper :: Board -> Int -> IO ()
putBoardHelper [] _ = return ()
putBoardHelper (x:xs) currentRow = do putRow currentRow x
                                      newline
                                      putBoardHelper xs (currentRow + 1)
-- 10.10.3

putBoard :: Board -> IO ()
putBoard xs = sequence_ [putRow (i+1) x >> newline | (x,i) <- zip xs [0..]]

-- 10.10.4
getDigit :: String -> IO Int
getDigit prompt = do Prelude.putStr prompt
                     x <- getChar
                     if isDigit x then
                        return (digitToInt x)
                     else
                        do Prelude.putStr "error: not a digit"
                           newline
                           getDigit prompt


accumulateLoop :: Int -> Int -> IO Int
accumulateLoop acc 0 = return acc
accumulateLoop acc numsLeft = do x <- getDigit ""
                                 newline
                                 accumulateLoop (acc+x) (numsLeft-1)
                                      

adder :: IO ()
adder = do nums <- getDigit "How many numbers? "
           newline
           total <- accumulateLoop 0 nums
           Prelude.putStr "The total is "
           print total

-- 10.10.5
adder' :: IO ()
adder' = do Prelude.putStr "How many numbers? "
            l <- getLine
            let total_nums = read l :: Int
            nums <- sequence [newline >> getDigit "" | _ <- [1..total_nums]]
            newline
            Prelude.putStr "The total is: "
            print (sum nums)

-- 10.10.6
-- TBD
