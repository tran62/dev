-- ghci rosetta4.hs then under ghci > main
module Main where
  import Data.Monoid ((<>), mempty)
  import Data.List (intercalate, transpose)
-- source rosettacode.org https://rosettacode.org/wiki/Towers_of_Hanoi#Haskell

  hanoi :: Int -> t -> t -> t -> [[t]]
  hanoi 0 _ _ _ = mempty
  hanoi 1 l r _  = [[l,r]]
  hanoi n l r m = hanoi (n - 1) l m r <> hanoi 1 l r m <> hanoi (n - 1) m r l
 
  showHanoi :: Int -> String
  showHanoi n =
    unlines $
    intercalate " ->   " <$>
    transpose
      ((justifyLeft 6 ' ' <$>) <$> transpose (hanoi n "left" "right" "mid"))
 
  justifyLeft :: Int -> Char -> String -> String
  justifyLeft n c s = take n (s <> replicate n c)
 
-- TEST -------------------------------------------------------------
  main :: IO ()
  main = do
    putStr "Enter nb of disks: " 
    n <- readLn 
    putStrLn $ showHanoi n