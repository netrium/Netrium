-- |Netrium is Copyright Anthony Waite, Dave Hetwett, Shaun Laurens 2009-2015, and files herein are licensed
-- |under the MIT license,  the text of which can be found in license.txt
--
{-# LANGUAGE PatternGuards #-}

module Main where

import Contract
import DecisionTreeSimplify
import Display
import Paths_netrium

import Data.Maybe
import Data.Version
import System.Environment
import System.Exit
import System.Console.GetOpt
import System.IO
import System.Directory
import System.Process
import System.FilePath

import Text.XML.HaXml.XmlContent


data OutputMode = OutputSvg | OutputPng | OutputDot

outputExtension OutputSvg = "svg"
outputExtension OutputPng = "png"
outputExtension OutputDot = "dot"

data Options =
  Options
    { optMode         :: OutputMode
    , optDecisionTree :: Bool
    , optStartTime    :: Maybe Time
    , optDepth        :: Maybe Int
    , optVersion      :: Bool
    }

defaultOptions :: Options
defaultOptions =
  Options
    { optMode         = OutputSvg
    , optDecisionTree = False
    , optStartTime    = Nothing
    , optDepth        = Nothing
    , optVersion      = False
    }

optDepthDefault :: Int
optDepthDefault = 8

options :: [OptDescr (Options -> Options)]
options =
  [ Option [] ["syntax-tree"]
           (NoArg (\ opts -> opts { optDecisionTree = False }))
           "Generate the contract syntax tree (default mode)"
  , Option [] ["decision-tree"]
           (NoArg (\ opts -> opts { optDecisionTree = True }))
           "Generate the contract decision tree"
  , Option [] ["start-time"]
           (ReqArg (\ arg opts -> opts {
                        optStartTime = Just (readArg "start-time" arg)
                      }) "TIME")
           "Contract start time (required in decision tree mode)"
  , Option [] ["tree-depth"]
           (ReqArg (\ arg opts -> opts {
                        optDepth = Just (readArg "tree-depth" arg)
                      }) "NUM")
           ("Limit the tree depth (decision tree mode default is "
            ++ show optDepthDefault ++ ")")
  , Option [] ["svg"]
           (NoArg (\ opts -> opts { optMode = OutputSvg }))
           "Output in SVG image format (default format)"
  , Option [] ["png"]
           (NoArg (\ opts -> opts { optMode = OutputPng }))
           "Output in PNG image format"
  , Option [] ["dot"]
           (NoArg (\ opts -> opts { optMode = OutputDot }))
           "Output in DOT graph format"
  , Option [] ["version"]
           (NoArg (\ opts -> opts { optVersion = True }))
           "Print version information"
  ]

readArg :: Read a => String -> String -> a
readArg optname arg =
  case reads arg of
    [(v,"")] -> v
    _        -> error $ "unrecognised value '" ++ arg
                     ++ "' for option --" ++ optname

main :: IO ()
main =
  do
    plainArgs <- getArgs
    let (optMods, args, errs) = getOpt Permute options plainArgs
    let opts = foldl (flip ($)) defaultOptions optMods
    case (args, errs) of
      _ | optVersion opts -> printVersion
        | optDecisionTree opts
        , Nothing <- optStartTime opts
                    -> exit ["In --decision-tree mode, --start-time= is required"]

      ([contract],         []) -> visualise opts contract output
                                    where output = replaceExtension contract
                                                     (outputExtension (optMode opts))
      ([contract, output], []) -> visualise opts contract output
      _                        -> exit errs


exit :: [String] -> IO ()
exit errs = do
    p <- getProgName
    let output | null errs = usageInfo usage options
               | otherwise = p ++ ": " ++ unlines errs
                               ++ usageInfo usage options
        usage = "Usage: " ++ p ++ " <contract.xml> [<output>]\n\nFlags:"
    putStrLn output
    exitFailure


printVersion :: IO ()
printVersion = do
  p <- getProgName
  putStrLn $ "netrium-demo " ++ p ++ " version " ++ showVersion version


visualise :: Options -> FilePath -> FilePath -> IO ()
visualise opts contractFile outputFile = do

    contract <- fReadXml contractFile

    let tree | optDecisionTree opts
             , Just startTime <- optStartTime opts
             = trimDepth (fromMaybe optDepthDefault (optDepth opts))
             $ toTree (decisionTreeSimple startTime contract)

             | otherwise
             = maybe id trimDepth (optDepth opts)
             $ toTree contract

    case optMode opts of
      OutputDot -> writeFile outputFile (renderDotGraph tree)
      _ -> do
        tmpdir          <- getTemporaryDirectory
        (tmpfile, htmp) <- openTempFile tmpdir "contract.dot"
        hPutStr htmp (renderDotGraph tree)
        hClose htmp
        let format = case optMode opts of
                       OutputSvg -> "-Tsvg"
                       OutputPng -> "-Tpng"
        exitcode <- rawSystem "dot" [format, "-o", outputFile, tmpfile]
        removeFile tmpfile
        exitWith exitcode
