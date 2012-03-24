module Euler3 where

res n = maximum $ primefactors n

primefactors n = case takeNext nums of
                        Nothing -> [n]
                        Just n@(n',rem) -> n' : primefactors rem
  where
         nums = [2..(n `div` 2)]
         takeNext [] = Nothing
         takeNext (x:xs) = if n `mod` x == 0
                              then Just $ (x,n `div` x)
                              else takeNext xs


