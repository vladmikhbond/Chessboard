module Main where

import Queens
import Horse
import Data.Time.Clock ( diffUTCTime, getCurrentTime )
import System.IO (hFlush, stdout )
import Data.List.Split ( splitOn )

main :: IO ()
main = mainHorse

mainHorse = do
   (r, c) : start : finish <- getPairs " rc st fi > "
   --closed <- getPairs " closed: xy xy ... > "
   let closed = []
   ---------------------------------------
   t0 <- getCurrentTime
   print $ "--- start --- " ++ show t0
   ---
   let pathes = runHorse r c start closed
   let validPathes = if null finish 
       then pathes 
       else filter (\p -> head p == head finish ) pathes 
   if null validPathes
   then putStr "no answers"
   else do
      let path = head validPathes
      putStrLn $ showOnePath path r c closed
   ---
   t1 <- getCurrentTime
   putStrLn $ "--- time --- " ++ show (diffUTCTime t1 t0)


getPairs :: String -> IO [(Int, Int)]
getPairs prompt = do
   putStr prompt
   hFlush stdout
   str <- getLine
   let ns = if null str 
       then [] 
       else read <$> splitOn " " str
   return $ map (`divMod` 10) ns
   




  