import Tuura.Concurrency
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Tuura.Log

readLog :: String -> IO (Log String)
readLog strLog = do
    return . canonicalise $ parse strLog

toIntLog :: Log String -> (Log Int, Set Int, Int -> String)
toIntLog log1 = (intLog, Set.map a2i alphabet, i2a)
  where
    alphabet = events log1
    a2i s    = Set.findIndex s alphabet
    i2a i    = Set.elemAt i alphabet
    intLog   = map (map a2i) log1
    
findConcurrentPairs :: String -> IO()
findConcurrentPairs inputLog = do
    (logOriginal, alphabet, decode) <- fmap toIntLog $ readLog inputLog
    let log    = dropSubtraces $ split logOriginal 
        oracle = oracleMokhovCarmona log
        events = Set.elems alphabet
        cache  = Map.fromSet (uncurry oracle) $
                 Set.fromList [ (x, y) | x <- events, y <- events, x <= y ]
        co a b = cache Map.! (min a b, max a b)

    putStrLn "Concurrent pairs:"
    print [ (decode x, decode y) | (x:xs) <- tails events, y <- xs, x `co` y ]
    putStrLn ""
    putStrLn "______________"
    putStrLn ""



main :: IO ()
main = do
    let log1 = "a b c a c b"
    putStrLn "Log 1: "
    print log1    
    findConcurrentPairs log1

    let log2 = "a b c d a c b d"
    putStrLn "Log 2: "
    print log2
    findConcurrentPairs log2
    
    let log3 = "a b c d a c b d a c d b a c b d"
    putStrLn "Log 3: "
    print log3
    findConcurrentPairs log3
    
    let log4 = "a p q b a q p b a p q c a q p c"
    putStrLn "Log 4: "
    print log4
    findConcurrentPairs log4
    
    

