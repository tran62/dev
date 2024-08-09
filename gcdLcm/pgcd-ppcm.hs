module Main where
  import System.IO
  import Data.List
  import Data.Char

  pgcd :: (Integral a) => a -> a -> a
  pgcd x y = gcd_ (abs x) (abs y)
    where
      gcd_ a 0 = a
      gcd_ a b = gcd_ b (a `rem` b)

  ppcm :: (Integral a) => a -> a -> a
  ppcm _ 0 =  0
  ppcm 0 _ =  0
  ppcm x y =  abs ((x `quot` (pgcd x y)) * y)

  main :: IO()
  main = do
    putStr "Entrer nb a:"
    number <- getLine
    let a = read number
    putStr "Entrer nb b:"
    number <- getLine
    let b = read number
    putStrLn "a,b, pgcd(a,b), ppcm(a,b)"
    print (a,b, "pgcd->", pgcd a b, "ppcm->", ppcm a b)



