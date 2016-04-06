{-# LANGUAGE BangPatterns #-}   

import Tuura.Concurrency
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Tuura.Graph
import Tuura.Log
import Tuura.CmdLineFlags
import System.FilePath

addId :: [String] -> [String]
addId expressions = do
    let report = unlines $ addIds expressions
    return report
  where
    addIds = zipWith (\n s -> "p" ++ show n ++ " = " ++ s) [(1 :: Int)..]

toIntLog :: Log String -> (Log Int, Set Int, Int -> String)
toIntLog log = (intLog, Set.map a2i alphabet, i2a)
  where
    alphabet = events log
    a2i s    = Set.findIndex s alphabet
    i2a i    = Set.elemAt i alphabet
    intLog   = map (map a2i) log


main :: IO ()
main = do
    options <- parseArgs
    if (inputPath options == "") 
        then putStrLn "Enter some traces: "
        else do putStr ""
    let input = optInput options
    let outPath = outputPath options 
    let out = optOutput options
    (logOriginal, alphabet, decode) <- fmap toIntLog $ readLog input
    let log    = if (optSplit options) then dropSubtraces $ split logOriginal else logOriginal
        oracle = oracleMokhovCarmona log
        events = Set.elems alphabet
        cache  = Map.fromSet (uncurry oracle) $
                 Set.fromList [ (x, y) | x <- events, y <- events, x <= y ]
        co a b = cache Map.! (min a b, max a b)
        graphs = map (fmap decode) $ reduceLog co log 
    let !concurrencyReport = [ (decode x, decode y) | (x:xs) <- tails events, y <- xs, x `co` y ]
    
    putStrLn "\nConcurrent pairs:"
    print concurrencyReport
    if (outPath == "") 
        then
          putStrLn "\nComplete. Results: "
        else 
          putStrLn $ "\nComplete. See results in '" ++ outPath ++ "'."
    let [result] = addId (map printGraphExpr graphs)
    out result
    putStrLn ""
