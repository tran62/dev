-- Matrix Exponentiation
def mul( a, b ) =
  res = array( a.length(), b(0).length() )

  for i <- 0:a.length(), j <- 0:b(0).length()
    res( i, j ) = sum( a(i, k)*b(k, j) | k <- 0:b.length() )

  vector( res )

def
  pow( _, 0 ) = ((1, 0), (0, 1))
  pow( x, 1 ) = x
  pow( x, n )
    | 2|n = pow( mul(x, x), n\2 )
    | otherwise = mul(x, pow( mul(x, x), (n - 1)\2 ) )

def fib( n ) = pow( ((0, 1), (1, 1)), n )(0, 1)

for i <- 0..10
  println( fib(i) )