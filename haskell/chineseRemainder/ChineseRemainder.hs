--  The Chinese remainder theorem states that if one knows the remainders of
--   the Euclidean division of an integer n by several integers,
--   then one can determine uniquely the remainder of the division of n
--   by the product of these integers, under the condition that the divisors
--   are pairwise coprime.
--   ex : (Sunzi's formulation) : x = 2 mod 3 = 3 mod 5 = 2 mod 7
--         x = 23 mod 105 ( 105=3*5*7 )
module Main where
import Control.Monad (zipWithM)
 
egcd :: Int -> Int -> (Int, Int)
egcd _ 0 = (1, 0)
egcd a b = (t, s - q * t)
  where
    (s, t) = egcd b r
    (q, r) = a `quotRem` b
 
modInv :: Int -> Int -> Either String Int
modInv a b =
  case egcd a b of
    (x, y)
      | a * x + b * y == 1 -> Right x
      | otherwise ->
        Left $ "No modular inverse for " ++ show a ++ " and " ++ show b
 
chineseRemainder :: [Int] -> [Int] -> Either String Int
chineseRemainder residues modulii =
  zipWithM modInv crtModulii modulii >>=
  (Right . (`mod` modPI) . sum . zipWith (*) crtModulii . zipWith (*) residues)
  where
    modPI = product modulii
    crtModulii = (modPI `div`) <$> modulii
 
main :: IO ()
main = 
  mapM_ (putStrLn . either id show) $ uncurry chineseRemainder <$>
  [ ([10, 4, 12], [11, 12, 13])
  , ([10, 4, 9], [11, 22, 19])
  , ([2, 3, 2], [3, 5, 7])
  ]
