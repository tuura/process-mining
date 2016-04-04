import Tuura.Concurrency
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Foldable
import Tuura.Graph
import Tuura.Log
import System.Environment
import System.FilePath
import System.Console.GetOpt
import System.Exit
import System.IO


readLog :: FilePath -> IO (Log String)
readLog filename = do
    contents <- readFile filename
    return . canonicalise $ parse contents

writeResult :: FilePath -> [String] -> IO ()
writeResult filename expressions = do
    let report = unlines $ addIds expressions
    writeFile filename report
  where
    addIds = zipWith (\n s -> "g" ++ show n ++ " = " ++ s) [1..]

toIntLog :: Log String -> (Log Int, Set Int, Int -> String)
toIntLog log = (intLog, Set.map a2i alphabet, i2a)
  where
    alphabet = events log
    a2i s    = Set.findIndex s alphabet
    i2a i    = Set.elemAt i alphabet
    intLog   = map (map a2i) log

--getSplit :: [String] -> ([String], Bool)
--getSplit [] = ([], False)
--getSplit (s:ss)
--    | s == "-split" = (ss  , True )
--    | otherwise     = (s:ss, False)


data Options = Options
    { optSplit    :: Bool
    , optInputLog :: FilePath
    , optOutput   :: Maybe FilePath
    } deriving Show

defaultOptions    = Options
    { optSplit    = False
    , optInputLog = ""
    , optOutput   = Nothing
    }

options :: [OptDescr (Options -> IO Options)]
options =
    [ Option ['s']     ["split"]
        (NoArg (\ opts -> do
                   return opts { optSplit = True }))
        "Repeat events are set as separate as new events"
    , Option ['i']     []
        (ReqArg (\ f opts -> do
                    return opts { optInputLog = optInputLog opts ++ f }) "FILE")
        "Input event log"
    , Option ['o']     ["output"]
        (OptArg ((\ f opts -> do
                    return opts { optOutput = Just f }) . fromMaybe "FILE")
                "FILE")
        "Optional output .cpog file"
    ]

parseArgs :: IO Options
parseArgs = do
  argv <- getArgs
  progName <- getProgName
  let header = "Usage: " ++ progName ++ " [OPTION...]"
  case getOpt Permute (options) argv of
    (opts, [], []) -> foldlM (flip id) defaultOptions opts
    (_, _, errs) -> ioError (userError (concat errs))


main :: IO ()
main = do
    options <- parseArgs
    let [filename, result] = [optInputLog options] ++ [filename `replaceExtension` "cpog"]
    (logOriginal, alphabet, decode) <- fmap toIntLog $ readLog filename
    let log    = if optSplit options then dropSubtraces $ split logOriginal else logOriginal
        oracle = oracleMokhovCarmona log
        events = Set.elems alphabet
        cache  = Map.fromSet (uncurry oracle) $
                 Set.fromList [ (x, y) | x <- events, y <- events, x <= y ]
        co a b = cache Map.! (min a b, max a b)
        graphs = map (fmap decode) $ reduceLog co log
    putStrLn "Concurrent pairs:"
    print [ (decode x, decode y) | (x:xs) <- tails events, y <- xs, x `co` y ]
    writeResult result $ map printGraphExpr graphs
    putStrLn $ "\nComplete. See results in '" ++ result ++ "'."
