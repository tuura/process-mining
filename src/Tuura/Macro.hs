{-# LANGUAGE BangPatterns, FlexibleInstances #-}

module Tuura.Macro (
    Alphabet, HasDuration (..), MacroEvent (..), MacroTrace (..), macroTrace
    ) where

import Tuura.Log

import Data.Array
import Data.List
import Data.Ord

-- TODO: Move to a separate module.
class HasDuration a where
    duration :: a -> Double

-- Macro events parameterised by the alphabet type.
data MacroEvent a = MacroEvent
    { first   :: a        -- ^ First symbol
    , second  :: a        -- ^ Second symbol
    , begin   :: Int      -- ^ Position within the trace (beginning)
    , end     :: Int      -- ^ Position within the trace (end)
    , skipped :: Double   -- ^ Total duration of skipped events
    , weight  :: Double } -- ^ Total duration of non-skipped events
    deriving Show

-- Evaluate how good a particular event is.
evaluate :: MacroEvent a -> Double
evaluate e = value * value
  where
    value = max 0 (weight e - skipped e)

data MacroTrace a = MacroTrace
    { trace       :: Trace (MacroEvent a)
    , totalWeight :: Double }

empty :: MacroTrace a
empty = MacroTrace [] 0

type Alphabet a = [a]

-- Add a macro event to a macro trace, accumulating the overall weight.
(|+>) :: MacroEvent a -> MacroTrace a -> MacroTrace a
(|+>) e t = MacroTrace (e : trace t) (evaluate e + totalWeight t)

-- Given an event trace compute an optimal trace of macro events, maximising
-- the sum of squared weights of discovered macro events.
macroTrace :: (HasDuration a, Ord a) => Alphabet a -> Trace a -> MacroTrace a
macroTrace as tr = solve 0
  where
    n  = length tr
    dp = listArray (0, n) [ go k | k <- [0..n] ]

    go k
        | k == n    = (0, MacroEvent undefined undefined 0 0 0 0)
        | otherwise = maximumBy (comparing fst) (candidates k)

    candidates k = [ (evaluate e + fst (dp ! rest), e)
                   | (e, rest) <- zip (chain as k $ drop k tr) [(k + 1)..] ]

    solve k
        | k == n    = empty
        | otherwise = let e = snd (dp ! k) in e |+> solve (end e + 1)

-- Given an alphabet and a trace find successive macro events of maximum weight.
chain :: (HasDuration a, Ord a) => Alphabet a -> Int -> Trace a -> [MacroEvent a]
chain _  _     [] = []
chain as begin tr = map (maximumBy $ comparing evaluate) (transpose chains)
  where
    chains = [ tail $ scanl' append (MacroEvent x y begin (begin - 1) 0 0) tr
             | x <- as, y <- as, x < y ]

    append (MacroEvent x y begin end s w) e
        | e == x || e == y = MacroEvent x y begin (end + 1) s (w + duration e)
        | otherwise        = MacroEvent x y begin (end + 1) (s + duration e) w
