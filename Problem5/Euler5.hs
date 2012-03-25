module Euler5 where
import Data.List


res = foldr (\(x,y) c -> (x^y)*c) 1 $ foldr buildup [] $ concat desc

desc = map (map (\x -> (head x, length x))) $ map (group . primefactors) nums

nums = [1..21]

buildup :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
buildup (a,b) xs = case lookup a xs of
                          Nothing -> (a,b) : xs
                          Just y  -> if (b > y)
                                             then (a,b) : delete (a,y) xs 
                                             else xs


--Brought back from Problem #3
primefactors n = case takeNext nums of
                        Nothing -> [n]
                        Just n@(n',rem) -> n' : primefactors rem
  where
         nums = [2..(n `div` 2)]
         takeNext [] = Nothing
         takeNext (x:xs) = if n `mod` x == 0
                              then Just $ (x,n `div` x)
                              else takeNext xs
