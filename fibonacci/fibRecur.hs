-- recurvive
fib :: Integer -> Integer
fib n   | n <= 1    =   1
        | n >= 2     =   fib(n-1) + fib(n-2)
main = do
    putStrLn "fib 5 :"
    print (fib 5)
        