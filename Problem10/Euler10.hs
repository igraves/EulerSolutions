module Euler10 where


main = do
          putStrLn $ show $ sum $ sieve 2000000

--A prime sieve of all primes up to the argument
sieve :: Integer -> [Integer]
sieve x = 2 : (sieve' $ nums x)
  where
    nums z = [3,5..z]
    sieve' [] = []
    sieve' (p:xs) | p*p > x  =  p:xs
                  | True     = p : sieve' [ x | x <- xs, x `rem` p /= 0]
