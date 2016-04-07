module Tuura.CmdLineFlags (Options(..), parseArgs, readLog
    ) where

import Data.Foldable
import System.Console.GetOpt
import System.Environment
import Tuura.Log

data Options = Options
    { optSplit    :: Bool
    , optInput    :: IO String
    , optOutput   :: String -> IO ()
    , optReport   :: Bool
    }

defaultOptions    = Options
    { optSplit    = False
    , optInput    = getContents
    , optOutput   = putStr
    , optReport   = False
    }

options :: [OptDescr (Options -> IO Options)]
options =
    [ Option ['s']     ["split"]
        (NoArg (\ opts -> do
                   return opts { optSplit = True }))
        "Repeat events are set as separate as new events"
    , Option ['o']     ["output"]
        (ReqArg (\ f opts -> do
                    return opts { optOutput = writeFile f}) "FILE")
        "Optional output .cpog file"
    , Option ['c']     ["concurrency-report"]
        (NoArg (\ opts -> do
                   return opts { optReport = True }))
        "Print a list of concurrent pairs"
    ]

parseArgs :: IO Options
parseArgs = do
  argv <- getArgs
  progName <- getProgName
  let header = "Usage: \n" ++ progName ++ " <inputFilePath> [-s | --split] [-c | --concurrency-report] [-o | --output <outputFilePath>]"
  let helpMessage = usageInfo header options
  case getOpt Permute (options) argv of
    (opts, [], []) -> foldlM (flip id) defaultOptions opts
    (opts, [input], []) -> foldlM (flip id) defaultOptions {optInput = readFile input} opts
    (opts, nonOpts, []) -> ioError (userError ("Only one input event log\n" ++ helpMessage))
    (_, _, errs) -> ioError (userError (concat errs))

readLog :: IO String -> IO (Log String)
readLog input = do
    contents <- input
    return . canonicalise $ parse contents    
