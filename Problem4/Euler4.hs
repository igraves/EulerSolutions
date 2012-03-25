module Euler4 where
import Data.List
import Data.Maybe


--exhaustive search of the space
--I wanted to review the search space to better optimize how to attack the problem
--f(x,y) = x*y is the model, which is a hyperbolic function
--If you were able to lazily generate the pairs in order according to the hyperbola, then
res = maximum $ filter intPalindrome $ map (\(x,y) -> x*y) allpairs

allpairs = concat $ map pair ns

--Palindrome testing without converting to strings
intPalindrome :: Int -> Bool
intPalindrome k = isP $ breaknum ([],k) (radicies k)

--Break the numbers into an x length list of each radix
breaknum (l,_) 0  = l
breaknum (l,n) x =  let r = (n `div` 10) 
                        s = (n `mod` 10)
                      in breaknum (s : l,r) (x-1)


--Compute the radicies of the number
radicies k = radicies' k 1 
radicies' k i = if k < 10 then i else radicies' (k `div` 10) (i+1)

--List is a Palindrome?
isP [] = True
isP (x:[]) = True
isP xs = if head xs == last xs 
          then isP (tail $ init xs) 
          else False

--Ancillary
pair p = map (\x -> (p,x)) ns

ns = nums (999::Int) (101::Int)
nums x y | x == y = [y] 
         | otherwise = x : nums (x-1) y
