import System.Exit (exitFailure)
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

findConcurrentPairs :: String -> [(String,String)] -> IO()
findConcurrentPairs inputLog expPairs = do
    (logOriginal, alphabet, decode) <- fmap toIntLog $ readLog inputLog
    let lg    = dropSubtraces $ split logOriginal
        oracle = oracleMokhovCarmona lg
        evnts = Set.elems alphabet
        cache  = Map.fromSet (uncurry oracle) $
                 Set.fromList [ (x, y) | x <- evnts, y <- evnts, x <= y ]
        co a b = cache Map.! (min a b, max a b)

    putStr "Found concurrent pairs:    "
    let foundPairs = [ (decode x, decode y) | (x:xs) <- tails evnts, y <- xs, x `co` y ]
    print foundPairs
    if expPairs /= foundPairs
        then do putStrLn "Test failure, expected and found concurrent pairs do not match"
                exitFailure
        else putStrLn "Test passed, expected and found concurrent pairs match"
    putStrLn ""
    putStrLn "______________"
    putStrLn ""



main :: IO ()
main = do

    putStrLn ""

    let log1 = "a b c\na c b"
    let expPairs1 = [("b","c")]
    putStrLn "Log 1: "
    print log1
    putStr "Expected concurrent pairs: "
    print expPairs1
    findConcurrentPairs log1 expPairs1

    let log2 = "a b c d\na c b d"
    let expPairs2 = [("b","c")]
    putStrLn "Log 2: "
    print log2
    putStr "Expected concurrent pairs: "
    print expPairs2
    findConcurrentPairs log2 expPairs2

    let log3 = "a b c d\na c b d\na c d b\na c b d"
    let expPairs3 = [("b","c"),("b","d")]
    putStrLn "Log 3: "
    print log3
    putStr "Expected concurrent pairs: "
    print expPairs3
    findConcurrentPairs log3 expPairs3

    let log4 = "a p q b\na q p b\na p q c\na q p c"
    let expPairs4 = [("p","q")]
    putStr "Log 4: "
    print log4
    putStr "Expected concurrent pairs: "
    print expPairs4
    findConcurrentPairs log4 expPairs4



