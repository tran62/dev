module Main where
-- https://haskelle.blogspot.com/2013/04/haskell-tower-of-hanoi-solution.html
    main :: IO ()
    main = do
        putStr "Towers of Hanoi Disk Movement:\n\
               \ Given three stationary rods: A, B, and C;\n\
               \ Enter number of disks you want to move from A to B: "
        a <- readLn
        let d = hanoi a "rod A" "rod B" "rod C"
        putStrLn "Solution: "
        putStr d
 
-- Towers of Hanoi by recursion: to move n disks, move n-1 out of the way,
--   then move lower disk to destination rod, move n-1 to destination rod.
    hanoi :: Integer -> String -> String -> String -> String
    hanoi 0 r1 r2 _ = "No disk to move\n"
    hanoi 1 r1 r2 r3 = " Move one disk from " ++ r1 ++ " to " ++ r2 ++ "\n"
    hanoi n r1 r2 r3 =(hanoi (n-1) r1 r3 r2) ++  -- move n-1 disks to r3
                      (hanoi 1 r1 r2 r3) ++      -- move lower disk to r2
                      (hanoi (n-1) r3 r2 r1)     -- move n-1 disks to r2