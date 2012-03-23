module Euler3 where




factors n = filter (\x -> n `mod` x == 0) nums  
	where
    	   nums = [2..(n `div` 2)]

intSqrt = floor . sqrt . fromIntegral

