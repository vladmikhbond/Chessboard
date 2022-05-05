module Horse (runHorse, showOnePath) where
-- Обійти конем усі клітинки шахівниці розміром n х n, 
-- побувавши рівно один раз на кожній клітинці.

import Consul
import Data.List ( sort )
import Data.List.Split ( chunksOf )

type Pos = (Int, Int)
type Path  = [Pos]

doStep :: [Path] -> Int -> Int -> Int -> [Pos] -> [Path]
doStep paths len nR nC closed = doStepR paths len
 where
   doStepR paths len
      | len == nR * nC - length closed = paths
   doStepR paths len = do
      path <- paths
      let lastPos = head path
      let steps = getStepsOrd lastPos nR nC path closed
      step <- steps
      doStepR [step : path] (len + 1)


getSteps :: Pos -> Int -> Int -> Path -> [Pos] -> [Pos]
getSteps (r, c) nR nC path closed = filter f [
    (r+1, c+2), (r+1, c-2), (r-1, c+2), (r-1, c-2),
    (r+2, c+1), (r+2, c-1), (r-2, c+1), (r-2, c-1)]
 where
   f (r, c) = r >= 0 && c >= 0 && r < nR && c < nC
              && notElem (r, c) path
              && notElem (r, c) closed

getStepsOrd :: Pos -> Int -> Int -> Path -> [Pos] -> [Pos]
getStepsOrd (r, c) nR nC path closed = let
   gs = getSteps (r, c) nR nC path closed
   pricedSteps :: [(Int, Pos)]
   pricedSteps = map (\s -> (length $ getSteps s nR nC (s : path) closed, s)) gs 
 in
    map snd (sort pricedSteps)  



-- UTILS ------------------------------------------------

{-- 
Є рядок s, символ r і число n. Треба вставити символ r в рядок s через однакові проміжки у n символів.

insSepN xs sep n = undefined
--}

insN ::  Int -> [a] -> a -> [a]
insN n xs sep = let
   chunks = chunksOf n xs
 in
   head chunks ++ concatMap (sep :) (tail chunks)

-- SHOW -------------------------------------------------

showOnePath :: Path -> Int -> Int -> [Pos] -> String
showOnePath path nRows nCols closed =
   concat $ insN nCols path2 "\n"
 where
   pathNclosed = reverse path ++ closed
   pairs :: [(Pos, Int)]
   pairs = sort $ zip pathNclosed ([1..length path] ++ repeat 0 )   -- 
   path2 = map (show2 . snd) pairs  -- sort on positions

   show2 0 = "   "
   show2 n | n < 10 = "  " ++ show n
   show2 n = " " ++ show n

runHorse :: Int -- n rows
 -> Int         -- n cols
 -> Pos         -- start
 -> [Pos]       -- closed cells
 -> [Path]
runHorse nRows nCols pos = doStep [[pos]] 1 nRows nCols





