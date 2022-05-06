
module Main where

import Queens
import Horse
import Data.Time ( diffUTCTime, getCurrentTime )
import System.IO (hFlush, stdout )
import Data.List.Split ( splitOn )
import Control.Monad (when, unless)

main :: IO ()
main = mainQueens

mainQueens = do
   putStr "q-quit | n > "  -- 8
   hFlush stdout
   line <- getLine
   when ('q' `notElem` line) (do
      let states = runQueens (read line)
      print states
      unless (null states) ((putStrLn . showOneState . head) states)
      mainQueens )


----------------------------------------------------------------------------
mainHorse = do
   putStr "q-quit | nRows nCols  row col [c] > "  -- 8 8 1 2 c
   hFlush stdout
   line <- getLine
   let isClose = 'c' `elem` line
   let line' = if isClose
                then take (length line - 2) line
                else line

   when ('q' `notElem` line') (do
      let [nRows, nCols, r, c] = map read (splitOn " " line')

      t0 <- getCurrentTime
      print $ "--- start --- " ++ show t0
      ---
      let pathes = runHorse nRows nCols (r, c) isClose  -- < main part

      if null pathes
      then putStr "no answers"
      else putStrLn $ showOnePath (head pathes) nRows nCols
      ---
      t1 <-getCurrentTime
      putStrLn ("--- time --- " ++ show (diffUTCTime t1 t0))
      mainHorse )
