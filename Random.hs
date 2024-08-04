import System.Random
-- import System.Random.Shuffle
-- import Control.Monad (replicateM)
import Control.Monad
import Data.List
-- import Control.Monad.ST
-- import Control.Monad.Random
-- import Data.Array.ST

rand1 = replicateM 10 (randomIO :: IO Float) >>= print

rand2 = do
    g <- getStdGen
    
    print $ take 10 (randomRs ('a','z') g)

rand3 = do
    g <- getStdGen
    print $ take 10 $ (randoms g :: [Float])

rand4 = do
    g <- getStdGen
    print $ take 10  $ (randomRs (1,10) g)

rollDice :: IO Int
rollDice = getStdRandom (randomR (1,6))

createRList = do
    seed  <- newStdGen
    let rs = randomList 10 seed
    print rs
randomList :: Int -> StdGen -> [Int]
randomList n = take n . unfoldr (Just . randomR (1,10))
{-}
removeItem :: Int -> [a]-> [a]
removeItem i items = take i items ++ drop (1+i) items
-}
-- data Coin = Heads | Tails deriving (Show, Enum, Bounded)
data Coin = Heads | Tails deriving ({-hi-}Eq,{-/hi-} Show, Enum, Bounded)

instance Random Coin where
  randomR (a, b) g =
    case randomR (fromEnum a, fromEnum b) g of
      (x, g') -> (toEnum x, g')
  random g = randomR (minBound, maxBound) g

tossCoin = do
  g <- newStdGen
  print . take 10 $ (randoms g :: [Coin])

-- show
-- Number of samples to take
count = 10000

-- Function to process our random sequence
process :: [Coin] -> (Int, Int)
process cs = (length cs, length (filter (== Heads) cs))

-- Function to display the running value.
display:: (Int, Int) -> String
display (coins, heads) = "We got " ++ (show $ 100.0 * fromIntegral heads / fromIntegral coins) ++ "% heads."

simulate = do
  g <- newStdGen
  putStrLn . display . process . take count $ randoms g
