module Tuura.Relation (
    Relation (..), empty, singleton, union, unions, complete
    ) where

import Data.Set (Set)
import qualified Data.Set as Set

data Relation a = Relation { domain :: Set a, relation :: Set (a, a) }
                  deriving (Show, Eq, Ord)

empty :: Relation a
empty = Relation Set.empty Set.empty

singleton :: a -> Relation a
singleton x = Relation (Set.singleton x) Set.empty

union :: Ord a => Relation a -> Relation a -> Relation a
union p q = Relation (domain   p `Set.union` domain   q)
                     (relation p `Set.union` relation q)

unions :: Ord a => [Relation a] -> Relation a
unions rs = Relation (Set.unions $ map domain   rs)
                     (Set.unions $ map relation rs)

complete :: Ord a => Set a -> Set a -> Relation a
complete p q = Relation (p `Set.union` q) $
    Set.fromAscList [ (x, y) | x <- Set.elems p, y <- Set.elems q ]
