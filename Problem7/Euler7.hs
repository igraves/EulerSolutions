module Euler7 where
import Debug.Trace

--Prime numbers account for ~5-6% of all numbers
--We can shoot for this by finding the first .05 * 30,000,000 primes or so and ask for the 10,001
--by using a simple prime sieve

--res 10000 gets you the answer
res n = filter isPrime nums !! n 

nums = [2..]

isPrime n = let r = primefactors n in n == head r && length r == 1
primefactors n = case takeNext nums of
                        Nothing -> [n]
                        Just n@(n',rem) -> n' : primefactors rem
  where
         nums = [2..(n `div` 2)]
         takeNext [] = Nothing
         takeNext (x:xs) = if n `mod` x == 0
                              then Just $ (x,n `div` x)
                              else takeNext xs
