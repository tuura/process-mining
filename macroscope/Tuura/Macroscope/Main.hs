module Tuura.Macroscope.Main (main) where

import Data.List.Extra

import Tuura.Macro

data Event a = Event a Double deriving Show

instance Eq a => Eq (Event a) where
    Event a _ == Event b _ = a == b

instance Ord a => Ord (Event a) where
    compare (Event a _) (Event b _) = compare a b

instance HasDuration (Event a) where
    duration (Event _ d) = d

event :: String -> Event String
event s = Event a (read d)
  where
    [a, d] = words s

main :: IO ()
main = do
    tr <- (map event . lines) <$> getContents
    let alphabet = nubOrd $ sort tr
        result   = macroTrace alphabet tr
    mapM_ print $ trace result
    putStrLn $ "Total weight = " ++ show (totalWeight result)
