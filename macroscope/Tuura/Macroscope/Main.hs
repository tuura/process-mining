module Tuura.Macroscope.Main (main) where

import Data.List.Extra

import Tuura.Macro

data Event a = Event a Double deriving Show

label :: Event a -> a
label (Event a _) = a

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

showEvent :: MacroEvent (Event String) -> String
showEvent e = "(" ++ label (first e) ++ ", " ++ label (second e)
    ++ ") at " ++ show [begin e, end e]
    ++ " (weight = " ++ show (weight e) ++ ")"

main :: IO ()
main = do
    tr <- (map event . lines) <$> getContents
    let alphabet = nubOrd $ sort tr
        long me  = end me - begin me > 5
        result   = filter long $ trace $ macroTrace alphabet tr
    mapM_ (putStrLn . showEvent) result
