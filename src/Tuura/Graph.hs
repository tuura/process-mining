{-# LANGUAGE TypeFamilies #-}
module Tuura.Graph (
    Graph (..), fromList, vertices, clique, overlayList, connectList, printGraph,
    GraphExpr (..), printGraphExpr
    ) where

import Tuura.Relation (Relation (..))
import qualified Tuura.Relation as Relation
import Data.Set (Set)
import qualified Data.Set as Set

class Graph g where
    type Vertex g
    empty   :: g
    vertex  :: Vertex g -> g
    overlay :: g -> g -> g
    connect :: g -> g -> g

fromList :: Graph g => [(Vertex g, Vertex g)] -> g
fromList = overlayList . map (\(x, y) -> vertex x `connect` vertex y)

vertices :: Graph g => [Vertex g] -> g
vertices = overlayList . map vertex

clique :: Graph g => [Vertex g] -> g
clique = connectList . map vertex

-- 'foldr f empty' adds a redundant empty to the result; foldg avoids this
foldg :: Graph g => (g -> g -> g) -> [g] -> g
foldg _ [] = empty
foldg f gs = foldr1 f gs

overlayList :: Graph g => [g] -> g
overlayList = foldg overlay

connectList :: Graph g => [g] -> g
connectList = foldg connect

instance Ord a => Graph (Relation a) where
    type Vertex (Relation a) = a
    empty       = Relation.empty
    vertex      = Relation.singleton
    overlay     = Relation.union
    connect p q = Relation.unions [p, q, Relation.complete (domain p) (domain q)]

data Expression = Open String | Closed String deriving (Eq, Ord)

close :: Expression -> String
close (Open   s) = "(" ++ s ++ ")"
close (Closed s) = s

printGraph :: Expression -> String
printGraph (Open   s) = s
printGraph (Closed s) = s

instance Graph Expression where
    type Vertex Expression = String
    empty       = Closed "()"
    vertex      = Closed . id
    overlay p q = Open $ printGraph p ++ " + " ++ printGraph q
    connect p q = Closed $ close p ++ " -> " ++ close q

data GraphExpr a = Empty
                 | Vertex a
                 | Overlay (GraphExpr a) (GraphExpr a)
                 | Connect (GraphExpr a) (GraphExpr a)
                 deriving (Eq, Ord)

instance Graph (GraphExpr a) where
    type Vertex (GraphExpr a) = a
    empty   = Empty
    vertex  = Vertex
    overlay = Overlay
    connect = Connect

foldGraph :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> GraphExpr a -> b
foldGraph e v o c ge = case ge of
    Empty       -> e
    Vertex x    -> v x
    Overlay x y -> o (foldGraph e v o c x) (foldGraph e v o c y)
    Connect x y -> c (foldGraph e v o c x) (foldGraph e v o c y)

instance Functor GraphExpr where
    fmap f = foldGraph Empty (Vertex . f) Overlay Connect

printGraphExpr :: GraphExpr String -> String
printGraphExpr = printGraph . foldGraph empty vertex overlay connect
