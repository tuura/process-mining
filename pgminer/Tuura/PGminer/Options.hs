module Tuura.PGminer.Options (Options(..), getOptions) where

import Data.Foldable (foldlM)
import System.Console.GetOpt
import System.Environment

data Options = Options
    { optSplit  :: Bool
    , optInput  :: IO String
    , optOutput :: String -> IO ()
    , optReport :: Bool }

defaultOptions :: Options
defaultOptions  = Options
    { optSplit  = False
    , optInput  = getContents
    , optOutput = putStr
    , optReport = False }

options :: [OptDescr (Options -> IO Options)]
options =
    [ Option ['c'] ["concurrency-report"]
      (NoArg (\opts -> return opts { optReport = True }))
      "Print a list of concurrent pairs"
    , Option ['o'] ["output"]
      (ReqArg (\f opts -> return opts { optOutput = writeFile f}) "FILE")
      "Output file"
    , Option ['s'] ["split"]
      (NoArg (\opts -> return $ opts { optSplit = True }))
      "Split traces with multiple event occurrences" ]

getOptions :: IO Options
getOptions = do
    argv     <- getArgs
    progName <- getProgName
    let header = concat [ "Usage: \n"
                        , progName
                        , " <inputFilePath>"
                        , " [-c | --concurrency-report]"
                        , " [-o | --output <outputFilePath>]"
                        , " [-s | --split]" ]
        helpMessage = usageInfo header options
    case getOpt Permute options argv of
        (opts, [] , []  ) -> foldlM (flip id) defaultOptions opts
        (opts, [f], []  ) -> foldlM (flip id) defaultOptions
                             { optInput = readFile f } opts
        (_   , _  , []  ) -> ioError . userError $
                             "Multiple input files\n" ++ helpMessage
        (_   , _  , errs) -> ioError . userError $ concat errs
