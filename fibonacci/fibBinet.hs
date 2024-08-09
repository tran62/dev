-- Using Binet's formula and exact real arithmetic library we can calculate arbitrary Fibonacci number exactly.

-- import math.sqrt
-- fib :: Integral -> Float
fib n  =  (phi^n - (-phi)^(-n))/sqrt(5) + 1/2  where
        phi = (1 + sqrt( 5 ))/2