{-# LANGUAGE BangPatterns #-}

module Tuura.Macro (Alphabet, MacroEvent (..), MacroTrace, macroTrace) where

import Tuura.Log
import Data.List
import Data.Ord

data MacroEvent a = MacroEvent { first :: a, second :: a, weight :: Int }
    deriving Show

instance Functor MacroEvent where
    fmap f (MacroEvent x y w) = MacroEvent (f x) (f y) w

type MacroTrace a = (Trace (MacroEvent a), Int)

type Alphabet a = [a]

-- Add a macro event to a macro trace, accumulating the overall weight.
(|+>) :: MacroEvent a -> MacroTrace a -> MacroTrace a
(|+>) e t = (e : fst t, weight e * weight e + snd t)

-- Given an event trace compute an optimal trace of macro events, maximising
-- the sum of squared weights of discovered macro events.
macroTrace :: Eq a => Alphabet a -> Trace a -> MacroTrace a
macroTrace _  [] = ([], 0)
macroTrace as tr = maximumBy (comparing snd) candidates
  where
    ts         = tails (drop 1 tr)
    candidates = [ e |+> macroTrace as es | (e, es) <- zip (chain as tr) ts ]

-- Given an alphabet and a trace find successive macro events of maximum weight.
chain :: Eq a => Alphabet a -> Trace a -> [MacroEvent a]
chain as tr = map (maximumBy $ comparing weight) (transpose chains)
  where
    chains = [ map (MacroEvent x y) $ go 0 x y tr | x <- as, y <- as, x /= y ]
    go _    _ _ []   = []
    go !res x y (e:es)
        | x == e    = (res + 1) : go (res + 1) y x es
        | otherwise = res       : go res       x y es
