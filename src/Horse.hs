module Horse (runHorse, showOnePath) where
-- Обійти конем усі клітинки шахівниці розміром n х n, 
-- побувавши рівно один раз на кожній клітинці.

import Consul
import Data.List ( sort )
import Data.List.Split ( chunksOf )

type Pos = (Int, Int)
type Path  = [Pos]

doStep :: [Path] -> Int -> Int -> Int -> [Path]
doStep paths len nR nC = doStepR paths len
 where
   doStepR paths len
      | len == nR * nC = paths
   doStepR paths len = do
      path <- paths
      let lastPos = head path
      step <-  getWarnsdorffSteps lastPos nR nC path
      doStepR [step : path] (len + 1)

theSteps (r, c) = [(r+1, c+2), (r+1, c-2), (r-1, c+2), (r-1, c-2),
                   (r+2, c+1), (r+2, c-1), (r-2, c+1), (r-2, c-1)]

getSteps :: Pos -> Int -> Int -> Path -> [Pos]
getSteps (r, c) nR nC path = filter f (theSteps (r, c))
 where
   f (r, c) = r >= 0 && c >= 0 && r < nR && c < nC
              && notElem (r, c) path


getWarnsdorffSteps :: Pos -> Int -> Int -> Path -> [Pos]
getWarnsdorffSteps (r, c) nR nC path = let
   steps = getSteps (r, c) nR nC path
   pricedSteps :: [(Int, Pos)]
   pricedSteps = map (\s -> (length $ getSteps s nR nC (s : path), s)) steps
 in
    map snd (sort pricedSteps)


-- SHOW -------------------------------------------------

showOnePath :: Path -> Int -> Int -> String
showOnePath path nRows nCols =
   concat $ insertSepByN nCols path' "\n"
 where
   n = length path
   pairs :: [(Pos, Int)]
   pairs = sort $ zip path [n, n-1..1]   -- 

   path' = map (show2 . snd) pairs  -- sort on positions

   show2 0 = "   "
   show2 n | n < 10 = "  " ++ show n
   show2 n = " " ++ show n

runHorse :: Int -- number of rows
 -> Int         -- number of cols
 -> Pos         -- start
 -> Bool        -- pathes are closed
 -> [Path]
runHorse nRows nCols pos isClose = if isClose
   then filter (\path -> pos `elem` theSteps (head path)) pathes
   else pathes
 where
   pathes = doStep [[pos]] 1 nRows nCols


-- UTILS ------------------------------------------------

insertSepByN ::  Int -> [a] -> a -> [a]
insertSepByN n xs sep = let
   chunks = chunksOf n xs
 in
   head chunks ++ concatMap (sep :) (tail chunks)






