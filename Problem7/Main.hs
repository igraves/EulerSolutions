module Main where

import Euler7
import System


main = 
        do 
          num <- getArgs 
          putStr $ show $ res $ (read (num !! 0))
