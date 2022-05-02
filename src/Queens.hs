-------------------------
-- Задачі на шахівниці --
-------------------------
module Queens (runQueens) where

--{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}


-- Розставити на шахівниці розміром n х n рівно n ферзів так, 
-- щоб вони не били один одного.
type Line = [Int]

runQueens :: Int -> [Line]
runQueens = queens [[]] 0

queens :: [Line] -> Int-> Int -> [Line]
queens lines k n | k == n = lines
queens lines k n  = do
   line <- lines
   q <- filter (`ok` line) [0 .. (n-1)]
   queens  [q : line] (k + 1) n
    where
      ok q line = and [ok1 q x i | (x, i) <- zip line [0..]]
      ok1 q x i = (q /= x) && abs (q - x) /= i + 1


