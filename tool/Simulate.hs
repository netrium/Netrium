-- |Netrium is Copyright Netrium Consulting Ltd, 2009-2012, and files herein are licensed
-- |under the Affero General Public License version 3, the text of which can
-- |be found in agpl.txt, or any later version of the AGPL, unless otherwise
-- |noted. 
--
module Main where

import Contract (Contract, Time)
import Interpreter
import Observations
import Paths_netrium_demo

import Data.Monoid
import Control.Monad
import qualified Data.Map as Map
import Data.Version
import System.Environment
import System.Exit
import System.Console.GetOpt
import System.FilePath

import Text.XML.HaXml.Types
import Text.XML.HaXml.Pretty (document)
import Text.XML.HaXml.XmlContent
import Text.PrettyPrint.HughesPJ (render)


data OutputMode = XmlOutput | TextOutput

data Options =
  Options
    { optMode    :: OutputMode
    , optTrace   :: Bool
    , optVersion :: Bool
    }

defaultOptions =
  Options
    { optMode    = XmlOutput
    , optTrace   = False
    , optVersion = False
    }

options :: [OptDescr (Options -> Options)]
options = [Option [] ["xml"]
                  (NoArg (\ opts -> opts { optMode = XmlOutput }))
                  "Output in xml format (this is the default)"
          ,Option [] ["text"]
                  (NoArg (\ opts -> opts { optMode = TextOutput }))
                  "Output as readable text"
          ,Option [] ["trace"]
                  (NoArg (\ opts -> opts { optTrace = True }))
                  "Output a trace of contract steps (--text mode only)"
          ,Option [] ["version"]
                  (NoArg (\ opts -> opts { optVersion = True }))
                  "Print version information"
          ]

main :: IO ()
main =
  do
    plainArgs <- getArgs
    let (optMods, args, errs) = getOpt Permute options plainArgs
    let opts = foldl (flip ($)) defaultOptions optMods
    case args of
      _ | optVersion opts -> printVersion
      [contract, observations]         | null errs
          -> simulate opts contract observations output
               where output = addExtension contract "xml"
      [contract, observations, output] | null errs
          -> simulate opts contract observations output
      _   -> exit


exit :: IO ()
exit =
  do
    p <- getProgName
    let txt = "Usage: " ++ p ++ " <contract.xml> <observations.xml> [<output.xml>]\n\n"
           ++ "Flags:"
    putStrLn (usageInfo txt options)
    exitFailure


printVersion :: IO ()
printVersion = do
  p <- getProgName
  putStrLn $ "netrium-demo " ++ p ++ " version " ++ showVersion version


simulate :: Options -> FilePath -> FilePath -> FilePath -> IO ()
simulate opts contractFile observationsFile outputFile = do

    contract <- fReadXml contractFile

    (SimulationInputs startTime mStopTime
                      valObsvns condObsvns
                      optionsTaken choicesMade) <- fReadXml observationsFile

    let simenv = SimEnv valObsvns condObsvns optionsTaken choicesMade
        simout = runContract simenv startTime mStopTime contract

    case optMode opts of
      XmlOutput  -> writeFile outputFile (renderContractRunXml simout)
      TextOutput -> putStr               (renderContractRun simout')
        where
          simout' | optTrace opts = simout
                  | otherwise     = simout { simTrace = TEs [] }


renderContractRun :: SimOutputs -> String
renderContractRun (SimOutputs (TEs trace) (TEs outs)
                              stopReason stopTime residualContract) =
  unlines $
       [ "============ Contract trace: ============" | not (null trace) ]
    ++ [ show time' ++ ": " ++ msg | (time', msg) <- trace ]
    ++ [ "\n============ Contract output: ============" ]
    ++ [ show out | out <- outs ]
    ++ [ "\n============ Final contract result: ============"
       , show stopReason
       , show stopTime ]

renderContractRunXml :: SimOutputs -> String
renderContractRunXml (SimOutputs _ outs stopReason stopTime residualContract) =
    render (document (Document prolog emptyST body []))

  where
    prolog = Prolog (Just (XMLDecl "1.0" Nothing Nothing)) [] Nothing []
    body   = Elem "SimulationResult" [] $
                     toContents (fromTimedEvents outs)
                  ++ toContents stopReason
                  ++ toContents stopTime
                  ++ toContents residualContract

data SimulationInputs = SimulationInputs
                          Time (Maybe Time)
                          (Observations Double) (Observations Bool)
                          (Choices ()) (Choices Bool)

instance HTypeable SimulationInputs where
  toHType _ = Defined "ChoiceSeries" [] []

instance XmlContent SimulationInputs where
  parseContents = do
    e@(Elem t _ _) <- element ["SimulationInputs"]
    commit $ interior e $ case t of
      "SimulationInputs" -> do
        startTime    <- parseContents
        mStopTime    <- parseContents
        obsSeriess   <- parseContents
        choiceSeries <- parseContents
        let (valObsvns, condObsvns)     = convertObservationSeries obsSeriess
            (optionsTaken, choicesMade) = convertChoiceSeries      choiceSeries

        return (SimulationInputs startTime mStopTime
                                 valObsvns condObsvns
                                 optionsTaken choicesMade)

    where
      convertObservationSeries :: [ObservationSeries]
                               -> (Observations Double, Observations Bool)
      convertObservationSeries obsSeriess = (valObsvns, condObsvns)
        where
          valObsvns  = Map.fromList
                         [ (var, toTimeSeries ts)
                         | ObservationsSeriesDouble var ts <- obsSeriess ]
          condObsvns = Map.fromList
                         [ (var, toTimeSeries ts)
                         | ObservationsSeriesBool var ts <- obsSeriess ]

      convertChoiceSeries :: ChoiceSeries -> (Choices (), Choices Bool)
      convertChoiceSeries (ChoiceSeries choiceSeriesXml) = (optionsTaken, choicesMade)
        where
          optionsTaken = Map.fromListWith mergeEventsBiased
                           [ (cid, TEs [(t,())])
                           | (t, AnytimeChoice cid) <- choiceSeries ]
          choicesMade  = Map.fromListWith mergeEventsBiased
                           [ (cid, TEs [(t,v)])
                           | (t, OrChoice cid v) <- choiceSeries ]
          TEs choiceSeries = toTimedEvents choiceSeriesXml 

  toContents (SimulationInputs startTime mStopTime
                               valObsvns condObsvns
                               optionsTaken choicesMade) =
      [mkElemC "SimulationInputs" $
                   toContents startTime  ++ toContents mStopTime
                ++ toContents obsSeriess ++ toContents choiceSeries]
    where
      obsSeriess   = [ ObservationsSeriesDouble var (fromTimeSeries ts)
                     | (var, ts) <- Map.toList valObsvns ]
                  ++ [ ObservationsSeriesBool   var (fromTimeSeries ts)
                     | (var, ts) <- Map.toList condObsvns ]
      choiceSeries = ChoiceSeries $ fromTimedEvents $ TEs $
                     [ (t, AnytimeChoice cid)
                     | (cid, TEs tes) <- Map.toList optionsTaken
                     , (t, ()) <- tes ]
                  ++ [ (t, OrChoice cid v)
                     | (cid, TEs tes) <- Map.toList choicesMade
                     , (t, v) <- tes ]
