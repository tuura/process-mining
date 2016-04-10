{-# LANGUAGE BangPatterns, FlexibleInstances #-}

module Tuura.Macro (Alphabet, MacroEvent (..), MacroTrace (..), macroTrace) where

import Tuura.Log

import Data.Array
import Data.List
import Data.Ord

-- Macro events parameterised by the alphabet type.
data MacroEvent a = MacroEvent
    { first  :: a     -- ^ First symbol
    , second :: a     -- ^ Second symbol
    , begin  :: Int   -- ^ Position within the trace (beginning)
    , end    :: Int   -- ^ Position within the trace (end)
    , weight :: Int } -- ^ Weight
    deriving Show

-- Evaluate how good a particular event is.
evaluate :: MacroEvent a -> Int
evaluate e = value * value
  where
    skipped = end e - begin e - weight e + 1
    value   = max 0 (weight e - skipped)

instance Functor MacroEvent where
    fmap f (MacroEvent x y b e w) = MacroEvent (f x) (f y) b e w

data MacroTrace a = MacroTrace { trace :: Trace (MacroEvent a), totalWeight :: Int }

empty :: MacroTrace a
empty = MacroTrace [] 0

type Alphabet a = [a]

-- Add a macro event to a macro trace, accumulating the overall weight.
(|+>) :: MacroEvent a -> MacroTrace a -> MacroTrace a
(|+>) e t = MacroTrace (e : trace t) (evaluate e + totalWeight t)

-- Given an event trace compute an optimal trace of macro events, maximising
-- the sum of squared weights of discovered macro events.
macroTrace :: Eq a => Alphabet a -> Trace a -> MacroTrace a
macroTrace as tr = solve 0
  where
    n  = length tr
    dp = listArray (0, n) [ go k | k <- [0..n] ]

    go k
        | k == n    = (0, MacroEvent undefined undefined 0 0 0)
        | otherwise = maximumBy (comparing fst) (candidates k)

    candidates k = [ (evaluate e + fst (dp ! rest), e)
                   | (e, rest) <- zip (chain as k $ drop k tr) [(k + 1)..] ]

    solve k
        | k == n    = empty
        | otherwise = let e = snd (dp ! k) in e |+> solve (end e + 1)

-- Given an alphabet and a trace find successive macro events of maximum weight.
chain :: Eq a => Alphabet a -> Int -> Trace a -> [MacroEvent a]
chain as begin tr = map (maximumBy $ comparing weight) (transpose chains)
  where
    chains = [ go 0 x y begin tr | x <- as, y <- as, x /= y ]
    go _    _ _ _   []   = []
    go !res x y end (e:es)
        | x == e    = MacroEvent x y begin end (res + 1) : go (res + 1) y x (end + 1) es
        | otherwise = MacroEvent x y begin end res       : go res       x y (end + 1) es
