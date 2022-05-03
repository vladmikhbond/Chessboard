module Horse (runHorse, showPath) where
-- Обійти конем усі клітинки шахівниці розміром n х n, 
-- побувавши рівно один раз на кожній клітинці.

import Consul
import Data.List ( sort )

type Pos = (Int, Int)
type Path  = [Pos]


runHorse :: Int -> Int -> Pos -> [Path]
runHorse nR nC pos = doStep [[pos]] 1 nR nC

doStep :: [Path] -> Int -> Int -> Int ->  [Path]
doStep paths len nR nC | len == nR * nC = paths
doStep paths len nR nC = do
   path <- paths
   let lastPos = head path
   let steps = getSteps lastPos nR nC path
   step <- steps
   doStep [step : path] (len + 1) nR nC


getSteps :: Pos -> Int -> Int -> Path -> [Pos]
getSteps (r, c) nR nC path = filter f [
    (r+1, c+2), (r+1, c-2), (r-1, c+2), (r-1, c-2),
    (r+2, c+1), (r+2, c-1), (r-2, c+1), (r-2, c-1)]
 where
   f (r, c) = r >= 0 && c >= 0 && r < nR && c < nC && notElem (r, c) path

showPath :: Path -> Int -> Int -> String
showPath path nRows nCols = let
   path' :: [String] 
   path' = map (show2 . snd) (sort $ zip (reverse path) [1..])
   show2 n =  (if n < 10 then " " ++ show n else show n) ++ " "
   in
     concat $ insN path' "\n" nCols
    

{-- 
Є рядок s, символ r і число n. Треба вставити символ r в рядок s через однакові проміжки у n символів.
--}

insN ::  [a] -> a -> Int -> [a]
insN cs a n = concat [ if i `mod` n == (n-1) then [nC, a] else [nC] | (i, nC) <- zip [0..] cs] 


