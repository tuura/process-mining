{-# LANGUAGE TypeFamilies #-}
module Tuura.Concurrency (
    ConcurrencyOracle, oracleMokhovCarmona, reduce, reduceLog
    ) where

import Data.Array
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Tuura.Log
import Tuura.Graph

type ConcurrencyOracle a = a -> a -> Bool

-- Given a log determine if two events are concurrent
oracleMokhovCarmona :: Ord e => Log e -> ConcurrencyOracle e
oracleMokhovCarmona log a b =
    not (null logAB || null logBA)
    &&
    persistentEvents logAB == persistentEvents logBA
    &&
    events logAB == events logBA
  where
    logAB = filter ([a, b] `isSubsequenceOf`) log
    logBA = filter ([b, a] `isSubsequenceOf`) log

-- Reduce a given trace to a partial order using a concurrency oracle
-- The resulting partial order is represented by a Graph in a canonical way
reduce :: (Graph g, Ord e, Vertex g ~ e) => ConcurrencyOracle e -> Trace e -> g
reduce co trace
    | null singles = fromList arcs
    | null arcs    = vertices singles
    | otherwise    = vertices singles `overlay` fromList arcs
  where
    x >< y  = not (x `co` y)
    totals  = blocks (><) trace
    spine   = concatMap (\xs -> zip xs (tail xs)) totals
    extra   = concat [ cross (><) x y | (x:xs) <- tails totals, y <- xs ]
    arcs    = sort $ spine ++ extra
    singles = sort $ trace \\ (uncurry (++) $ unzip arcs)

-- Split a list into blocks where neighboring elements are in a given relation
blocks :: (a -> a -> Bool) -> [a] -> [[a]]
blocks _ []         = []
blocks _ [x]        = [[x]]
blocks r (x:y:rest) = if r x y then (x:b):bs else [x]:b:bs
  where
    (b:bs) = blocks r (y:rest)

-- Given a relation and two total orders, compute the transitive reduction of
-- the relation w.r.t. to the total orders
cross :: (a -> b -> Bool) -> [a] -> [b] -> [(a, b)]
cross r as = reverse . go (reverse as)
  where
    go []     _   = []
    go _      []  = []
    go (x:xs) ys
        | null rest = go xs ys
        | otherwise = (x, head rest) : go xs notr
      where
        (notr, rest) = span (not . r x) ys

-- Reduce concurrency in each trace and drop repetitions in the result
reduceLog :: (Graph g, Ord e, Ord g, Vertex g ~ e) => ConcurrencyOracle e -> Log e -> [g]
reduceLog co = canonicalise . map (reduce co)
