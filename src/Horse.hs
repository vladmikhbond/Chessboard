module Horse (runHorse) where

-- Обійти конем усі клітинки шахівниці розміром n х n, 
-- побувавши рівно один раз на кожній клітинці.
type Pos = (Int, Int)
type Path  = [Pos]

runHorse :: Int -> Pos -> Path
runHorse n pos = head $ doStep [[pos]] 1 n

doStep :: [Path] -> Int -> Int -> [Path]
doStep paths len n | len == n * n = paths
doStep paths len n = do
   path <- paths
   let lastPos = head path
   let steps = getSteps lastPos n path
   step <- steps
   doStep [step : path] (len + 1) n


getSteps :: Pos -> Int -> Path -> [Pos]
getSteps (x, y) n path = filter f [
   -- (x-1, y-2), (x+1, y+2), (x+2, y-1), (x-2, y+1), 
   -- (x-2, y-1), (x+2, y+1), (x+1, y-2), (x-1, y+2) ]     
    (x+1, y+2), (x+1, y-2), (x-1, y+2), (x-1, y-2),
    (x+2, y+1), (x+2, y-1), (x-2, y+1), (x-2, y-1)]
 where
   f (x, y) = x >= 0 && y >= 0 && x < n && y < n && notElem (x, y) path

