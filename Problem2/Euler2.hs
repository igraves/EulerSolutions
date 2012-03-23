module Euler2 where



--result
res = foldr1 (+) $ filter (\x -> x `mod` 2 == 0) $ fibwhile (< 4000000)



--Gen Fibs while predicate
fibwhile p = fiblp 0 1 p

--Gen n Fib Terms
fibn n = take n $ fibl 0 1

--Fib Generator with predicate
fiblp x y p | p (x+y) == True = (x+y) : fiblp y (x+y) p
	    | otherwise = []
--Fib Generator
fibl x y = (x+y) : fibl y (x+y)

--Faster fibbonacci
fib 0 = 1
fib 1 = 2
fib n = fib' (n-2) 1 2

fib' n x y | n == 0 = x + y
	   | otherwise = fib' (n-1) y (x+y)

--Bad fibbonacci
badfib 0 = 1
badfib 1 = 2
badfib n = (badfib (n-1)) + (badfib (n-2))
