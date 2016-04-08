module Tuura.Macroscope.Main (main) where

import Data.Set (Set)
import qualified Data.Set as Set

import Tuura.Log
import Tuura.Macro

-- TODO: Get rid of code duplication (see pgminer.hs).
toIntTrace :: Trace String -> (Trace Int, Set Int, Int -> String)
toIntTrace tr = (intTrace, Set.map a2i alphabet, i2a)
  where
    alphabet = events [tr]
    a2i s    = Set.findIndex s alphabet
    i2a i    = Set.elemAt i alphabet
    intTrace = map a2i tr

main :: IO ()
main = do
    trace <- words <$> getLine
    let (intTrace, alphabet, decode) = toIntTrace trace
        result   = macroTrace (Set.elems alphabet) intTrace
    mapM_ (print . fmap decode) $ fst result
    putStrLn $ "Total weight = " ++ show (snd result)
