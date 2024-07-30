module Main where
-- https://gist.github.com/nicolasff/224222 
-- compile by ghc 2009... then cmd ./2009..  3 (nb of disks)
import Control.Monad.Writer
import System.Environment(getArgs, getProgName)
 
data Column = LeftCol | MiddleCol | RightCol
instance Show Column where
  show LeftCol = "left"
  show MiddleCol = "middle"
  show RightCol = "right"
 
-- move a list of blocks from column x to column y with column z as temporary storage
move :: [Int] -> Column -> Column -> Column -> Writer String ()
move [] _ _ _ = return ()  -- nothing to move.
 
move (largest:rest) src dst tmp = do
  move rest src tmp dst    -- move the top blocks from src to tmp using dest as temp
  tell $ "move ring " ++ show largest ++ " from " ++ show src ++ " col to " ++ show dst ++ " col\n"  -- move the largest one from src to dst
  move rest tmp dst src     -- move the tmp ones back onto the largest one on dst using src as temp
 
main :: IO ()
main = do
  args <- getArgs
  if null args then do
    getProgName >>= \argv0 -> putStrLn $ "Usage: " ++ argv0 ++ " <rings> (3 if void)"
    putStrLn . snd . runWriter $ move [3,2,1] LeftCol RightCol MiddleCol
  else let n = read (head args) :: Int in do
    putStrLn . snd . runWriter $ move [n,n-1..1] LeftCol RightCol MiddleCol