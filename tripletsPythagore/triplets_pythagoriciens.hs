-- Programme en Haskell pour lister
-- les triplets pythagoriciens primitifs (a,b,c)
-- tels que a^2 + b^2 = c^2 avec c = maxNum (100 par défaut)
-- Exécution : ghci triplets_pythagoriciens.hs
-- *Main>main 
pgcd :: Int -> Int -> Int 
-- PGCD de a et b p
pgcd 0 k = k
pgcd k 0 = k
pgcd a b = pgcd c (d `mod` c)
    where d = max a b
          c = min a b 


triplets ::  [(Int,Int,Int)] 
-- paramètres maxNum =100 pour a,b,c, 
maxNum = 100
triplets =  [(a,b,c) | 
   a <- [3..maxNum-1], 
   a `mod` 2 == 1,   -- a impair
   b <- [4..maxNum-1],   
   pgcd a b == 1,    -- a b premiers
   c <- [5..maxNum],
   c^2 == a^2 + b^2 ]

main :: IO()
main = do
  print ("Liste des triplets pythagoriciens primitifs (a,b,c) tq $a^2+b^2=c^2$ avec c < ",maxNum,"...=>...", triplets)
  print ("Nb triplets : ", length triplets)
