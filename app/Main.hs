module Main where

import Queens
import Horse
import Data.Time.Clock ( diffUTCTime, getCurrentTime )

main :: IO ()
main = mainHorse

mainHorse = do 
   putStr " nxy > "
   s <- getLine
   let a = [read [x] | x <- s, x /= ' ']

   t0 <- getCurrentTime
   print $ "--- start --- " ++ show t0
   ---
   print $ runHorse (a!!0) (a!!1) (a!!2)
   ---
   t1 <- getCurrentTime
   putStr "--- time ---"
   print $  diffUTCTime t1 t0


