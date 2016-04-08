module Tuura.PGminer.Main (main) where

import Data.List
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Tuura.Concurrency
import Tuura.Graph
import Tuura.Log
import Tuura.PGminer.Options

addIds :: [String] -> [String]
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
    options <- getOptions
    input   <- optInput options
    let (logOriginal, alphabet, decode) = toIntLog . canonicalise $ parse input
        log    = if optSplit options
                 then dropSubtraces $ split logOriginal
                 else logOriginal
        oracle = oracleMokhovCarmona log
        events = Set.elems alphabet
        cache  = Map.fromSet (uncurry oracle) $
                 Set.fromList [ (x, y) | x <- events, y <- events, x <= y ]
        co a b = cache Map.! (min a b, max a b)
        graphs = map (fmap decode) $ reduceLog co log
        result = unlines . addIds $ map printGraphExpr graphs
    if optReport options
    then optOutput options $ result ++ "\nConcurrent pairs: " ++ show
        [ (decode x, decode y) | (x:xs) <- tails events, y <- xs, x `co` y ]
    else optOutput options result
