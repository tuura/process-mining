module Log (
    Trace (..), Log (..), parse, split, dropSubtraces, canonicalise, events, persistentEvents
    ) where

import Data.Function
import Data.List
import Data.Ord
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

type Trace e = [e]
type Log   e = [Trace e]

-- * Split the given string into lines (traces)
-- * Split each trace into space separated substrings (events)
parse :: String -> Log String
parse = map words . lines

-- Add integer tags to events corresponding to the scenario they are part of.
-- For example, trace [1, 2, 1, 3] becomes [(1, 1), (2, 1), (1, 2), (3, 2)],
-- which means that [1, 2] and [1, 3] form separate scenarios (heuristic).
tagScenarios :: Ord e => Trace e -> Trace (e, Int)
tagScenarios trace = go Map.empty 1 trace
  where
    go _ _ []     = []
    go m s (x:xs) = (x, k) : go (Map.insert x k m) k xs
      where
        k  = max s $ 1 + Map.findWithDefault 0 x m

-- Split a trace into a log of traces corresponding to different scenarios
splitTrace :: Ord e => Trace e -> Log e
splitTrace = fmap (fmap fst) . groupBy ((==) `on` snd) . tagScenarios

-- Split each trace in a log into scenarios
split :: Ord e => Log e -> Log e
split = concatMap splitTrace

-- Drop traces which are subsequences of other traces
dropSubtraces :: Ord e => Log e -> Log e
dropSubtraces = dropSubs . sortBy (comparing length)
  where
    dropSubs xs = [ y | (y:ys) <- tails xs, all (not . isSubsequenceOf y) ys ]

-- Sort the traces lexicographically and drop repeated traces
canonicalise :: Ord a => [a] -> [a]
canonicalise = map head . group . sort

events :: Ord e => Log e -> Set e
events = Set.unions . map Set.fromList

persistentEvents :: Ord e => Log e -> Set e
persistentEvents [] = Set.empty
persistentEvents ts = foldl1' Set.intersection $ map Set.fromList ts
