{-# LANGUAGE TypeFamilies #-}
module Tuura.Function (
    printBooleanFunction
    ) where

import Tuura.Graph

-- data Function = Open String | Closed String deriving (Eq, Ord)

instance Graph Function where
    type Vertex Function = String
    empty       = Closed ""
    vertex      = Closed . id
    overlay p q = Open $ printFunction p ++ " * " ++ printFunction q
    connect p _ = Closed $ printFunction p

foldFunction :: b -> (String -> b) -> (b -> b -> b) -> (b -> b -> b) -> String -> GraphExpr String -> b
foldFunction e v o c s ge = case ge of
        Empty       -> e
        Vertex x    -> v x
        Overlay x y -> if test s x
            then if test s y
                then o (foldFunction e v o c s x) (foldFunction e v o c s y)
                else foldFunction e v o c s x
            else if test s y
                then foldFunction e v o c s y
                else e
        Connect x y -> if test s (Connect x y)
            then c (foldFunction e v o c s x) (foldFunction e v o c s y)
            else e

data Function = Open String | Closed String deriving (Eq, Ord)

printBooleanFunction :: String -> [GraphExpr String] -> [String]
printBooleanFunction s ge = trim $ map (\g -> printFunction $ foldFunction empty vertex overlay connect s g) ge
  where
    trim = filter (not . null)

printFunction :: Function -> String
printFunction (Open   s) = s
printFunction (Closed s) = s

test :: String -> GraphExpr String -> Bool
test _ (Empty) = False
test s (Vertex y) = (s == y)
test s (Overlay x y) = test s x || test s y
test s (Connect _ y) = test s y
