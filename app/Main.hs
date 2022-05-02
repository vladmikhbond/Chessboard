module Main where

import Queens
import Horse
import Data.Time.Clock ( diffUTCTime, getCurrentTime )

main :: IO ()
main = mainHorse

mainHorse = do 
   putStrLn " nxy> "
   s <- getLine
   let [n, x ,y] = [read [x] | x <- s, x /= ' ']

   t0 <- getCurrentTime
   print $ "--- start --- " ++ show t0
   ---
   print $ runHorse n (x, y)
   ---
   t1 <- getCurrentTime
   putStrLn $ "--- time --- " ++ show (diffUTCTime t1 t0)


