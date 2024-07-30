#!/usr/bin/env runhaskell
-- src: https://gist.github.com/kristopherjohnson/2384d9dcd5ed744734f0
-- ghci 2015...
-- Print sequence of moves for n discs from pin "A" to pin "C"
main = do 
          putStr "Enter number n of disks: "
          a <- readLn
          putStrLn "Move stack of n discs from A to C:"
          let moves = hanoi a "A" "C" "B"
          printNumberedList moves
          --  where moves = hanoi a "A" "C" "B"

-- Return list of moves for moving stack of N discs from one pin to another
hanoi :: Show a => Int -> a -> a -> a -> [String]
hanoi 0 _ _ _             = []
hanoi count from to other = moveToOther ++ [moveBottomDisc] ++ moveFromOther
  where
    moveBottomDisc = (show from) ++ " -> " ++ (show to)
    moveToOther    = hanoi (count - 1) from other to
    moveFromOther  = hanoi (count - 1) other to from

printNumberedList :: [String] -> IO ()

printNumberedList xs = mapM_ printLine $ zip [1..] xs
  where printLine (n, s) = putStrLn $ show n ++ ": " ++ s