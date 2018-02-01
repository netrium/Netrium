-- |Netrium is Copyright Anthony Waite, Dave Hetwett, Shaun Laurens 2009-2015, and files herein are licensed
-- |under the MIT license,  the text of which can be found in license.txt
--
module Main where

import Contract (Contract, Time)
import Interpreter
import DecisionTree
import Observations
import Paths_netrium

import Data.Maybe
import Data.Monoid
import Control.Monad
import qualified Data.Map as Map
import Data.Version
import System.Environment
import System.Exit
import System.Console.GetOpt
import System.FilePath

import Text.XML.HaXml.Namespaces (localName)
import Text.XML.HaXml.Types
import Text.XML.HaXml.Pretty (document)
import Text.XML.HaXml.XmlContent
import Text.PrettyPrint.HughesPJ (render)


data OutputMode = XmlOutput | TextOutput

data Options =
  Options
    { optMode    :: OutputMode
    , optTrace   :: Bool
    , optTest    :: Bool
    , optVersion :: Bool
    }

defaultOptions =
  Options
    { optMode    = XmlOutput
    , optTrace   = False
    , optTest    = False
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
          ,Option [] ["tests"]
                  (NoArg (\ opts -> opts { optTest = True }))
                  "Run internal tests as well"
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

    (SimulationInputs startTime mStopTime mStopWait
                      valObsvns condObsvns
                      optionsTaken choicesMade
                      simState) <- fReadXml observationsFile

    let initialState = case simState of
                         Nothing -> Left contract
                         Just st -> Right st
        simenv = SimEnv valObsvns condObsvns optionsTaken choicesMade
        simout = runContract simenv startTime mStopTime mStopWait initialState

    when (optTest opts) $
      case testRunContract simenv startTime contract of
        Nothing  -> return ()
        Just err -> fail ("internal tests failed: " ++ err)

    case optMode opts of
      XmlOutput  -> writeFile outputFile (renderContractRunXml simout)
      TextOutput -> putStr               (renderContractRun simout')
        where
          simout' | optTrace opts = simout
                  | otherwise     = simout { simTrace = TEs [] }


renderContractRun :: SimOutputs -> String
renderContractRun (SimOutputs (TEs trace) (TEs outs)
                              stopReason stopTime residualContract _ mWaitInfo) =
  unlines $
       [ "============ Contract trace: ============" | not (null trace) ]
    ++ [ show time' ++ ": " ++ msg | (time', msg) <- trace ]
    ++ [ "\n============ Contract output: ============" ]
    ++ [ show out | out <- outs ]
    ++ [ "\n============ Contract result: ============"
       , show stopReason
       , show stopTime ]
    ++ case mWaitInfo of
         Nothing -> []
         Just (WaitInfo obss mHorizon opts) ->
              [ "\n============ Horizon: ============" | isJust mHorizon ]
           ++ [ show horizon | horizon <- maybeToList mHorizon ]
           ++ [ "\n============ Wait conditions: ============" | not (null obss) ]
           ++ [ show obs | obs <- obss ]
           ++ [ "\n============ Available options: ============" | not (null opts) ]
           ++ [ show opt | opt <- opts ]

renderContractRunXml :: SimOutputs -> String
renderContractRunXml (SimOutputs _ outs stopReason stopTime residualContract simState mWaitInfo) =
    render (document (Document prolog emptyST body []))

  where
    prolog = Prolog (Just (XMLDecl "1.0" Nothing Nothing)) [] Nothing []
    body   = Elem (N "SimulationResult") [] $
                     toContents (fromTimedEvents outs)
                  ++ toContents stopReason
                  ++ toContents stopTime
                  ++ toContents residualContract
                  ++ toContents simState
                  ++ toContents mWaitInfo

data SimulationInputs = SimulationInputs
                          Time (Maybe Time) StopWait
                          (Observations Double) (Observations Bool)
                          (Choices ()) (Choices Bool)
                          (Maybe ProcessState)

instance HTypeable SimulationInputs where
  toHType _ = Defined "ChoiceSeries" [] []

instance XmlContent SimulationInputs where
  parseContents = do
    e@(Elem t _ _) <- element ["SimulationInputs"]
    commit $ interior e $ case localName t of
      "SimulationInputs" -> do
        startTime    <- parseContents
        mStopTime    <- parseContents
        mStopWait    <- parseContents
        obsSeriess   <- parseContents
        choiceSeries <- parseContents
        simState     <- parseContents
        let (valObsvns, condObsvns)     = convertObservationSeries obsSeriess
            (optionsTaken, choicesMade) = convertChoiceSeries      choiceSeries

        return (SimulationInputs startTime mStopTime mStopWait
                                 valObsvns condObsvns
                                 optionsTaken choicesMade
                                 simState)

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

  toContents (SimulationInputs startTime mStopTime mStopWait
                               valObsvns condObsvns
                               optionsTaken choicesMade simState) =
      [mkElemC "SimulationInputs" $
                   toContents startTime  ++ toContents mStopTime
                ++ toContents mStopWait
                ++ toContents obsSeriess ++ toContents choiceSeries
                ++ toContents simState]
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

-------------------------------------------------------------------------------
-- testing
--

-- Check:
--  * contract and process state can round trip via xml ok
--  * trace from single stepping is the same as running from scratch

testRunContract :: SimEnv -> Time -> Contract -> Maybe String
testRunContract simenv startTime contract
  | simStopReason overallOut /= simStopReason finalStep
  = Just $ show (simStopReason overallOut, simStopReason finalStep)

  | simStopTime overallOut /= simStopTime finalStep
  = Just $ show (simStopTime overallOut, simStopTime finalStep)

  | simOutputs overallOut /= foldr1 mergeEventsBiased (map simOutputs steps)
  = Just $ "outputs do not match:\n"
        ++ show (simOutputs overallOut)
        ++ "\nvs:\n"
        ++ show (foldr1 mergeEventsBiased (map simOutputs steps))

  | simTrace overallOut /= foldr1 mergeEventsBiased (map simTrace steps)
  = Just $ "trace does not match:\n"
        ++ show (simTrace overallOut)
        ++ "\nvs:\n"
        ++ show (foldr1 mergeEventsBiased (map simTrace steps))

  | not (all checkXmlRoundTrip steps)
  = Just "xml round trip failure"

  | otherwise = Nothing
  where
    steps = contractWaitSteps simenv startTime contract
    finalStep = last steps

    overallOut = runContract simenv startTime Nothing NoStop (Left contract)

    checkXmlRoundTrip simOut = roundTripProperty (simStopContract simOut)
                            && roundTripProperty (simStopState simOut)

    roundTripProperty :: (XmlContent a, Eq a) => a -> Bool
    roundTripProperty x = readXml (showXml False x) == Right x

contractWaitSteps :: SimEnv -> Time -> Contract -> [SimOutputs]
contractWaitSteps simenv startTime contract =
    remainingSteps step0
  where
    step0 = runContract simenv startTime Nothing StopFirstWait (Left contract)

    remainingSteps out
      | simStopReason out == StoppedWait = out : remainingSteps out'
      | otherwise                        = out : []
      where
        resumeState@(PSt resumeTime _ _) = simStopState out
        out' = runContract simenv resumeTime
                           Nothing StopNextWait
                           (Right resumeState)
