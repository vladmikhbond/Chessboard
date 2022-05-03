module Main where

import Queens
import Horse
import Data.Time.Clock ( diffUTCTime, getCurrentTime )

main :: IO ()
main = mainHorse

mainHorse = do 
   putStrLn " r c x y > "
   s <- getLine
   let [r, c, x ,y] = [read [x] | x <- s, x /= ' ']

   t0 <- getCurrentTime
   print $ "--- start --- " ++ show t0
   ---
   let path = head $ runHorse r c (x, y)
   print $ length path
   putStrLn $ showPath path r c
   ---
   t1 <- getCurrentTime
   putStrLn $ "--- time --- " ++ show (diffUTCTime t1 t0)


