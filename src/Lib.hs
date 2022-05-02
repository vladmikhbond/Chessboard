-------------------------
-- Задачі на шахівниці --
-------------------------
module Lib (runQueens, runHorse) where

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


-- Обійти конем усі клітинки шахівниці розміром n х n, 
-- побувавши рівно один раз на кожній клітинці.
type Path  = [(Int, Int)]

runHorse :: Int -> Int -> Int -> Path
runHorse n x y = head $ horse [[(x, y)]] 1 n

horse :: [Path] -> Int -> Int -> [Path]
horse paths len n | len == n * n = paths
horse paths len n = do
   path <- paths
   step <- steps (head path) n path
   horse [step : path] (len + 1) n
    where
      steps (x, y) n path = filter
         (\(x,y) -> x >= 0 && y >= 0 && x < n && y < n && notElem (x, y) path)
         [(x+1, y+2), (x+1, y-2), (x-1, y+2), (x-1, y-2),
         (x+2, y+1), (x+2, y-1), (x-2, y+1), (x-2, y-1)]

