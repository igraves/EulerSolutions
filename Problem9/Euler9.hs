module Euler9 where

main = do
        let res@(a,b,c) = findtriple $ reverse $ squares (1000^2)
        putStrLn $ show res
        putStrLn $ show $ a*b*c

--Filter perfect squares
nums n = [1..n]

--http://www.haskell.org/haskellwiki/Generic_number_type#isSquare
squares n = filter (\x -> (floor . sqrt $ fromIntegral x) ^ 2 == x) $ nums n

findtriple :: [Int] -> (Int,Int,Int)
findtriple [] = error "Can't find a match."
findtriple (c:cs) = case (findb cs) of
                          Nothing -> findtriple cs
                          Just x -> x
  where
    findb [] = Nothing
    findb (b:bs)   = case (findc b bs) of
                            Nothing -> findb bs
                            x -> x
    findc _ [] = Nothing
    findc b (a:as) = if (a + b == c) && ((floor . sqrt $ fromIntegral a) + (floor . sqrt $ fromIntegral b) + (floor . sqrt $ fromIntegral c) == 1000)
                      then Just (round . sqrt $ fromIntegral a,round . sqrt $ fromIntegral b,round . sqrt $ fromIntegral c) 
                      else findc b as
