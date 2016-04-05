module Tuura.CmdLineFlags (Options(..), parseArgs, getInput, getOutput, getSplit, 
    ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Foldable
import System.Console.GetOpt
import System.Environment

data Options = Options
    { optSplit    :: Bool
    , optInput    :: FilePath
    , optOutput   :: FilePath
    } deriving Show

defaultOptions    = Options
    { optSplit    = False
    , optInput    = ""
    , optOutput   = ""
    }

options :: [OptDescr (Options -> IO Options)]
options =
    [ Option ['s']     ["split"]
        (NoArg (\ opts -> do
                   return opts { optSplit = True }))
        "Repeat events are set as separate as new events"
    --, Option ['i']     ["input"]
    --    (ReqArg (\ f opts -> do
    --                return opts { optInputLog = optInputLog opts ++ f }) "FILE")
    --    "Input event log"
    , Option ['o']     ["output"]
        (ReqArg (\ f opts -> do
                    return opts { optOutput = optOutput opts ++ f }) "FILE")
        "Optional output .cpog file"
    ]

parseArgs :: IO Options
parseArgs = do
  argv <- getArgs
  progName <- getProgName
  let header = "Usage: " ++ progName ++ " <inputFilePath> [-s][--split] [-o][--output] <outputFilePath>"
  let helpMessage = usageInfo header options
  case getOpt Permute (options) argv of
    (opts, [], []) -> ioError (userError ("No input event log given\n" ++ helpMessage))
    (opts, [input], []) -> foldlM (flip id) defaultOptions {optInput = input} opts
    (opts, nonOpts, []) -> ioError (userError ("Only one input event log\n" ++ helpMessage))
    (_, _, errs) -> ioError (userError (concat errs))

getInput :: Options -> FilePath
getInput = return optInput options

getOutput :: Options -> FilePath
getOutput = return optOutput options

getSplit :: Options -> Bool
getSplit = return optSplit options

--getOptions :: Options -> [OptDescr (Options -> IO Options)]
--getOptions = return options
