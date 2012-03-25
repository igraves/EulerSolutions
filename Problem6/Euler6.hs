module Euler6 where

res = sqs - ssq

nats = [1..100]

ssq = foldr (\x y -> x^2 + y) 0 nats

sqs = (foldr (\x y -> x + y) 0 nats)^2
