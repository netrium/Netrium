-- |Netrium is Copyright Netrium Consulting Ltd, 2009-2012, and files herein are licensed
-- |under the Affero General Public License version 3, the text of which can
-- |be found in agpl.txt, or any later version of the AGPL, unless otherwise
-- |noted. 
{-# LANGUAGE DeriveFunctor #-}

module Interpreter where

import Contract
import Observable (Steps(..), VarName)
import qualified Observable as Obs
import DecisionTree hiding (Trade)
import qualified DecisionTree as TD (DecisionStep(Trade))
import DecisionTreeSimplify
import Observations

import Prelude hiding (product, until, and)
import Data.List hiding (and)
import Data.Monoid
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad
import Text.XML.HaXml.XmlContent hiding (next)
import XmlUtils
import Control.Exception (assert)


-- ---------------------------------------------------------------------------
-- * Main interpreter, using observables and choice data
-- ---------------------------------------------------------------------------

data Output = Trade Party Party Double Tradeable
            | OptionUntil   ChoiceId Time
            | OptionForever ChoiceId
  deriving (Eq, Show)

data StopReason = Finished                      -- ^ contract reduced to 'zero'
                | StoppedTime                   -- ^ stop time reached (in timeout mode)
                | StoppedWait                   -- ^ stopped at first wait point (in wait mode)
                | WaitForever                   -- ^ a non-terminating wait
                | ChoiceRequired       Party ChoiceId
                | ObservationExhausted VarName
                | ObservationMissing   VarName  -- ^ really an error
  deriving (Eq, Show)

data SimEnv
   = SimEnv {
       valueObservations :: Observations Double, -- ^ primitive real-valued obs
       condObservations  :: Observations Bool,   -- ^ primitive bool-valued obs
       optionsTaken      :: Choices (),          -- ^ 'anytime' options taken
       choicesMade       :: Choices Bool         -- ^ 'or' choices made
     }

data SimOutputs
   = SimOutputs {
       simTrace        :: TimedEvents String,
       simOutputs      :: TimedEvents Output,
       simStopReason   :: StopReason,
       simStopTime     :: Time,
       simStopContract :: Contract,
       simStopState    :: ProcessState,
       simStopWaitInfo :: Maybe WaitInfo
     }
  deriving (Show, Eq)

data StopWait = NoStop | StopFirstWait | StopNextWait
  deriving (Show, Eq)

data WaitInfo
   = WaitInfo {
       waitObs     :: [Obs Bool],
       waitHorizon :: Maybe Time,
       waitOptions :: [ChoiceId]
     }
  deriving (Show, Eq)

runContract :: SimEnv
            -> Time                -- ^ start time
            -> Maybe Time          -- ^ optional stop time
            -> StopWait            -- ^ stop at first waitpoint
            -> Either Contract ProcessState
            -> SimOutputs
runContract _ startTime (Just stopTime) _ _
  | not (stopTime > startTime)
  = error "runContract: stop time must be after start time"

runContract simenv startTime mStopTime mStopWait0 startState =
    let st0 = case startState of
                Left contract -> initialProcessState startTime contract
                Right st@(PSt time' _ _)
                  | startTime == time' -> st
                  | otherwise          -> error $ "runContract: resuming from the wrong time "
                                               ++ show time' ++ " vs " ++ show startTime

     in go [] [] mStopWait0 st0

  where
    go :: [(Time, String)] -> [(Time, Output)] -> StopWait
       -> ProcessState -> SimOutputs
    go trace output mStopWait st@(PSt time _ _) =
      -- if we go past the stop time, we've done something wrong...
      assert (maybe True (time <) mStopTime) $

      let obsenv = currentObsEnv (valueObservations simenv)
                                 (condObservations  simenv) time
          result = result' output
          result' out reason time' st' =
                   SimOutputs {
                     simTrace        = TEs (reverse trace),
                     simOutputs      = TEs (reverse out),
                     simStopReason   = reason,
                     simStopTime     = time',
                     simStopContract = currentContract st',
                     simStopState    = st',
                     simStopWaitInfo = Nothing
                   }
          step   = decisionStep st
          trace' = (time, show step) : trace
      in
      case decisionStep st of
        Done ->
          result Finished time st

        TD.Trade dir sf t next ->
          go trace' ((time, Trade p p' sf t) : output) mStopWait next
          where
            (p, p') = tradeDirParties dir

        Choose p cid next1 next2 ->
          case lookupChoice (choicesMade simenv) cid time of
            Nothing            -> result (ChoiceRequired p cid) time st
            Just v | v         -> go trace' output mStopWait next1
                   | otherwise -> go trace' output mStopWait next2

        ObserveCond obs next1 next2 ->
          case evalObs obsenv time obs of
            ObsExhausted varname    -> result (ObservationExhausted varname) time st
            ObsMissing   varname    -> result (ObservationMissing   varname) time st
            ObsResult v | v         -> go trace' output mStopWait next1
                        | otherwise -> go trace' output mStopWait next2

        ObserveValue obs next ->
          case evalObs obsenv time obs of
            ObsExhausted varname -> result (ObservationExhausted varname) time st
            ObsMissing   varname -> result (ObservationMissing   varname) time st
            ObsResult    v       -> go trace' output mStopWait (next v)

        Wait obsExprs optionsAvail | mStopWait == StopFirstWait ->
          case simplifyWait time obsExprs (not (null optionsAvail)) of
            Left  next   -> go trace' output mStopWait next
            Right []     -> result Finished time st
            Right conds' -> (result StoppedWait time st) { 
                              simStopWaitInfo = Just WaitInfo {
                                waitObs     = fmap fst conds',
                                waitHorizon = fmap fst (Obs.earliestTimeHorizon time conds'),
                                waitOptions = fmap fst optionsAvail
                              }
                            }

        Wait obsExprs optionsAvail ->

          let (time', waitResult) = runWait simenv obsenv
                                            mStopTime time
                                            obsExprs optionsAvail
          in case waitResult of
            ObsResult waitreason ->
                case waitreason of
                  WaitContinue next  -> go trace' outputU' mStopWait' (next time')
                  WaitStopped        -> result'   outputU' StoppedTime time' st
                  WaitFinished       -> result'   outputF' Finished    time' st
                  WaitNonTerm        -> result'   outputF' WaitForever time' st
              where
                outputU' = [ (time, OptionUntil choiceid time')
                           | (choiceid, _k) <- optionsAvail ] ++ output
                outputF' = [ (time, OptionForever choiceid)
                           | (choiceid, _k) <- optionsAvail ] ++ output
                mStopWait' | mStopWait == StopNextWait = StopFirstWait
                           | otherwise                 = mStopWait

            ObsExhausted varname -> result (ObservationExhausted varname) time' st
            ObsMissing   varname -> result (ObservationMissing   varname) time' st

data WaitResult k = WaitContinue k
                  | WaitStopped
                  | WaitFinished
                  | WaitNonTerm

runWait :: SimEnv
        -> ObsEnv
        -> Maybe Time
        -> Time
        -> [(Obs Bool, k)]
        -> [(ChoiceId, k)]
        -> (Time, ObsResult (WaitResult k))
runWait simenv obsenv mStopTime time obsExprs optionsAvail =
    checkEvents time (unTEs events)

  where
    timeouts = (case Obs.earliestTimeHorizon time obsExprs of
                 Nothing         -> []
                 Just (time', k) -> [(time', Just k)])
            ++ (case mStopTime of
                 Nothing         -> []
                 Just stopTime   -> [(stopTime, Nothing)])
    --events :: TimedEvents (WaitEvent k)
    events = mergeWaitEvents
               (valueObservations simenv) (condObservations simenv)
               (optionsTaken simenv)
               timeouts time obsenv

    -- Did we reach the time horizon?
    checkEvents time' [] = (time', ObsResult WaitNonTerm)
    checkEvents _ ((time', Timeout (Just k)):_remaining) =
      (time', ObsResult (WaitContinue k))

    checkEvents _ ((time', Timeout Nothing):_remaining) =
      (time', ObsResult WaitStopped)

    -- Check if we took an available option
    checkEvents _ ((time', TakeOption cid):remaining) =
      case lookup cid optionsAvail of
        Just k  -> (time', ObsResult (WaitContinue k))
        Nothing -> checkEvents time' remaining

    -- Check if any observable is true at this time
    checkEvents _ ((time', ObsChanged obsEnv): remaining) =
        case foldr accum (ObsResult Nothing) obsExprs of
          ObsResult (Just k)   -> (time', ObsResult (WaitContinue k))
          ObsResult Nothing
            | all (Obs.evermoreFalse time' . fst) obsExprs
                        -> (time', ObsResult WaitFinished)
            | otherwise -> checkEvents time' remaining
          ObsExhausted varname -> (time', ObsExhausted varname)
          ObsMissing   varname -> (time', ObsExhausted varname)

      where
        accum (obs, k) rest =
          case evalObs obsEnv time' obs of
            ObsResult    True    -> ObsResult (Just k)
            ObsResult    False   -> rest
            ObsExhausted varname -> ObsExhausted varname
            ObsMissing   varname -> ObsMissing   varname


-- | When in a wait state there are three different things that can happen
-- one of the observables can become true, we can choose to take an 'anytime'
-- option that is available to us.
--
-- There are two ways an observable can become true, one is due to a change in
-- a primitive/external ovservable, and the other is via the passage of time.
--
-- Hence, overall, there are three events we are interested in while waiting.
--
data WaitEvent k = TakeOption ChoiceId
                 | ObsChanged ObsEnv
                 | Timeout    k
  deriving Show

-- | Take all three sources of events we are interested in and produce a
-- unified event list
--
mergeWaitEvents :: Observations Double  -- ^ time series for real primitive obs
                -> Observations Bool    -- ^ time series for bool primitive obs
                -> Choices ()           -- ^ 'anytime' options taken
                -> [(Time, k)]          -- ^ optional timeouts
                -> Time                 -- ^ initial time
                -> ObsEnv               -- ^ initial values of all primitive obs
                -> TimedEvents (WaitEvent k)
mergeWaitEvents valObss condObss options timeouts time0 obsenv0 =
    events'
  where

    -- Firstly, combine all the observations into a unified event list
    obsTS :: TimedEvents [(VarName, Either (Maybe Double) (Maybe Bool))]
    obsTS = mconcat (valTSs ++ condTSs)

    valTSs, condTSs :: [TimedEvents [(VarName, Either (Maybe Double) (Maybe Bool))]]

    valTSs  = [ fmap (\e -> [(varname, Left e)])
                     (pruneTimedEvents time0 (timeSeriesEvents ts))
              | (varname, ts) <- Map.toList valObss ]

    condTSs = [ fmap (\v -> [(varname, Right v)])
                     (pruneTimedEvents time0 (timeSeriesEvents ts))
              | (varname, ts) <- Map.toList condObss ]

    -- similarly for the options, a unified event list
    optionsTS :: TimedEvents ChoiceId
    optionsTS = mconcat
                [ fmap (const cid) (pruneTimedEvents time0 ts)
                | (cid, ts) <- Map.toList options ]

    -- for the observations, convert the list of changes in observations
    -- into a list of ObsEnv values at each time
    obsEnvTS :: TimedEvents ObsEnv
    obsEnvTS = insertEventBefore time0 obsenv0
             $ snd (mapAccumTS accumObsEnv obsenv0 obsTS)

    accumObsEnv :: ObsEnv
                -> [(VarName, Either (Maybe Double) (Maybe Bool))]
                -> (ObsEnv, ObsEnv)
    accumObsEnv obsenv obschanges = (obsenv', obsenv')
      where
        obsenv' = foldl' update obsenv obschanges

        update (ObsEnv realObsvns boolObsvns) (varname, Left  v) =
            ObsEnv realObsvns' boolObsvns
          where
            realObsvns' = Map.insert varname v realObsvns

        update (ObsEnv realObsvns boolObsvns) (varname, Right v) =
            ObsEnv realObsvns boolObsvns'
          where
            boolObsvns' = Map.insert varname v boolObsvns

    -- Now combine the different events into one event list
    -- firstly the observations and the options
    events  = mergeEventsBiased (fmap ObsChanged obsEnvTS)
                                (fmap TakeOption optionsTS)

    -- and lastly any timeouts
    events' = foldr (\(time, k) -> insertEventAfter time (Timeout k))
                    events timeouts


-- ---------------------------------------------------------------------------
-- * Evaluating observables in the presense of observables data
-- ---------------------------------------------------------------------------

data ObsEnv = ObsEnv (Map VarName (Maybe Double))
                     (Map VarName (Maybe Bool))
  deriving Show

currentObsEnv :: Observations Double
              -> Observations Bool
              -> Time
              -> ObsEnv
currentObsEnv realObsvns boolObsvns time =
    ObsEnv (fmap (flip lookupTimeSeries time) realObsvns)
           (fmap (flip lookupTimeSeries time) boolObsvns)


data ObsResult a = ObsResult a
                 | ObsExhausted VarName
                 | ObsMissing   VarName
  deriving (Functor, Show)

evalObs :: ObsEnv
        -> Time
        -> Obs a
        -> ObsResult a
evalObs (ObsEnv realObsvns boolObsvns) time =
    go . Obs.eval time
  where
    go :: Steps a -> ObsResult a
    go (Result v) = ObsResult v

    go (NeedNamedVal _ varname k) =
      case Map.lookup varname realObsvns of
        Nothing       -> ObsMissing varname
        Just Nothing  -> ObsExhausted varname
        Just (Just v) -> go (k v)

    go (NeedNamedCond _ varname k) =
      case Map.lookup varname boolObsvns of
        Nothing       -> ObsMissing varname
        Just Nothing  -> ObsExhausted varname
        Just (Just v) -> go (k v)


-- ---------------------------------------------------------------------------
-- * XML instances
-- ---------------------------------------------------------------------------

instance HTypeable Output where
    toHType _ = Defined "Output" [] []

instance XmlContent Output where
    parseContents = do
      e@(Elem t _ _) <- element ["Trade","OptionUntil","OptionForever"]
      commit $ interior e $ case t of
        "Trade"         -> liftM4 Trade parseContents parseContents
                                        parseContents parseContents
        "OptionUntil"   -> liftM2 OptionUntil   (attrStr "choiceid" e) parseContents
        "OptionForever" -> liftM  OptionForever (attrStr "choiceid" e)

    toContents (Trade p p' sf t)       = [mkElemC "Trade" (toContents p
                                                          ++ toContents p'
                                                          ++ toContents sf
                                                          ++ toContents t)]
    toContents (OptionUntil cid time') = [mkElemAC "OptionUntil"
                                                   [("choiceid", str2attr cid)]
                                                   (toContents time')]
    toContents (OptionForever cid)     = [mkElemAC "OptionForever"
                                                   [("choiceid", str2attr cid)] []]


instance HTypeable StopReason where
    toHType _ = Defined "StopReason" [] []

instance XmlContent StopReason where
    parseContents = do
      e@(Elem t _ _) <- element ["Finished", "StoppedTime", "StoppedWait","WaitForever"
                                ,"ChoiceRequired"
                                ,"ObservationMissing","ObservationExhausted"]
      commit $ interior e $ case t of
        "Finished"       -> return Finished
        "StoppedTime"    -> return StoppedTime
        "StoppedWait"    -> return StoppedWait
        "WaitForever"    -> return WaitForever
        "ChoiceRequired" -> liftM2 ChoiceRequired parseContents
                                                  (attrStr "choiceid" e)
        "ObservationMissing"   -> liftM ObservationMissing   (attrStr "var" e)
        "ObservationExhausted" -> liftM ObservationExhausted (attrStr "var" e)

    toContents Finished    = [mkElemC "Finished"    []]
    toContents StoppedTime = [mkElemC "StoppedTime" []]
    toContents StoppedWait = [mkElemC "StoppedWait" []]
    toContents WaitForever = [mkElemC "WaitForever" []]

    toContents (ChoiceRequired party choiceid) =
        [mkElemAC "ChoiceRequired" [("choiceid", str2attr choiceid)]
                                   (toContents party)]
    toContents (ObservationExhausted varname) =
        [mkElemAC "ObservationExhausted" [("var", str2attr varname)] []]
    toContents (ObservationMissing   varname) =
        [mkElemAC "ObservationMissing" [("var", str2attr varname)] []]


instance HTypeable StopWait where
    toHType _ = Defined "StopWait" [] []

instance XmlContent StopWait where
  parseContents = (do
    e@(Elem t _ _) <- element ["StopFirstWait", "StopNextWait"]
    commit $ interior e $ case t of
      "StopFirstWait" -> return StopFirstWait
      "StopNextWait"  -> return StopNextWait)
    `onFail` return NoStop

  toContents NoStop        = []
  toContents StopFirstWait = [mkElemC "StopFirstWait" []]
  toContents StopNextWait  = [mkElemC "StopNextWait" []]


instance HTypeable WaitInfo where
    toHType _ = Defined "WaitInfo" [] []

instance XmlContent WaitInfo where
  parseContents = inElement "WaitInfo" $ do
                    obss <- parseContents
                    t    <- parseContents
                    opts <- parseContents
                    return $ WaitInfo ((map (\(WaitCondition obs) -> obs)) obss)
                                      t
                                      (map (\(WaitOption cid) -> cid) opts)
  toContents (WaitInfo obss t opts) = [mkElemC "WaitInfo" (toContents (map WaitCondition obss)
                                                        ++ toContents t
                                                        ++ toContents (map WaitOption opts))]

newtype WaitCondition = WaitCondition (Obs Bool)

instance HTypeable WaitCondition where
    toHType _ = Defined "WaitCondition" [] []

instance XmlContent WaitCondition where
  parseContents = inElement "WaitCondition" $
                    liftM WaitCondition Obs.parseObsCond
  toContents (WaitCondition obs) = [mkElemC "WaitCondition" [Obs.printObs obs]]


newtype WaitOption = WaitOption ChoiceId

instance HTypeable WaitOption where
    toHType _ = Defined "WaitOption" [] []

instance XmlContent WaitOption where
  parseContents = inElement "WaitOption" $
                    liftM WaitOption text
  toContents (WaitOption cid) = [mkElemC "WaitOption" (toText cid)]
