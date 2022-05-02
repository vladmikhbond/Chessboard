module Horse (runHorse) where

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

