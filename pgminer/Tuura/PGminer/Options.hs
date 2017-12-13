module Tuura.PGminer.Options (Options(..), getOptions) where

import Data.Foldable (foldlM)
import Control.Monad
import System.Console.GetOpt
import System.Environment
import System.Exit

data Options = Options
    { optHelp   :: Bool
    , optInput  :: IO String
    , optOutput :: String -> IO ()
    , optReport :: Bool
    , optSplit  :: Bool
    , optBool   :: String}

defaultOptions :: Options
defaultOptions  = Options
    { optHelp   = False
    , optInput  = getContents
    , optOutput = putStr
    , optReport = False
    , optSplit  = False
    , optBool   = "" }

options :: [OptDescr (Options -> IO Options)]
options =
    [ Option ['c'] ["concurrency-report"]
      (NoArg (\opts -> return opts { optReport = True }))
      "Print a list of concurrent pairs"
    , Option ['o'] ["output"]
      (ReqArg (\f opts -> return opts { optOutput = writeFile f }) "FILE")
      "Write output to a file"
    , Option ['s'] ["split"]
      (NoArg (\opts -> return $ opts { optSplit = True }))
      "Split traces with multiple event occurrences"
    , Option ['b'] ["bool"]
      (ReqArg (\f opts -> return opts { optBool = f}) "STRING")
      "Variable for which a boolean function should be generated"
    , Option ['h'] ["help"]
      (NoArg (\opts -> return $ opts { optHelp = True }))
      "Show this help message" ]

getOptions :: IO Options
getOptions = do
    argv   <- getArgs
    result <- case getOpt Permute options argv of
        (opts, [] , []  ) -> foldlM (flip id) defaultOptions opts
        (opts, [f], []  ) -> foldlM (flip id)
                             defaultOptions { optInput = readFile f } opts
        (_   , _  , []  ) -> ioError $ userError "Multiple input files"
        (_   , _  , errs) -> ioError . userError $ concat errs
    when (optHelp result) $ do
        progName <- getProgName
        let header = "Usage: " ++ progName ++ " [input file] [OPTION...]"
            helpMessage = usageInfo header options
        putStrLn helpMessage
        exitSuccess
    return result
