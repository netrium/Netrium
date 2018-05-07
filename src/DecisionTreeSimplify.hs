-- |Netrium is Copyright Anthony Waite, Dave Hewett, Shaun Laurens & Contributors 2009-2018, and files herein are licensed
-- |under the MIT license,  the text of which can be found in license.txt
--
{-# LANGUAGE DeriveFunctor, GADTs, PatternGuards #-}

module DecisionTreeSimplify (
    decisionTreeSimple,
    decisionStepWithTime,
    simplifyWait
  ) where

import Contract
import Observable (Steps(..))
import qualified Observable as Obs
import DecisionTree

import Prelude hiding (product, until, and, id)
import Data.List hiding (and)


-- ---------------------------------------------------------------------------
-- * Apply our knowledge of time
-- ---------------------------------------------------------------------------

decisionTreeSimple :: Time -> Contract -> DecisionTree
decisionTreeSimple t c = unfoldDecisionTree
                           decisionStepWithTime
                           (initialProcessState t c)

decisionStepWithTime :: ProcessState -> (DecisionStep ProcessState, Time)
decisionStepWithTime st@(PSt time _ _) = case decisionStep st of
    Done                   -> (Done, time)

    Trade d sf t st1       -> (Trade d sf t st1, time)

    Choose p id st1 st2    -> (Choose p id st1 st2, time)

    ObserveCond o st1 st2  -> case Obs.eval time o of
                                Result True  -> decisionStepWithTime st1
                                Result False -> decisionStepWithTime st2
                                _            -> (ObserveCond o st1 st2, time)

    ObserveValue o k       -> case Obs.eval time o of
                                Result v     -> decisionStepWithTime (k v)
                                _            -> (ObserveValue o k, time)

    Wait conds opts        -> case simplifyWait time conds (not (null opts)) of
                                Left  st'    -> decisionStepWithTime st'
                                Right []     -> (Done, time)
                                Right conds' -> (Wait conds' opts, time)

-- The Wait action is the complicated one
--
simplifyWait :: Time
             -> [(Obs Bool, Time -> ProcessState)]
             -> Bool
             -> Either ProcessState
                       [(Obs Bool, Time -> ProcessState)]
simplifyWait time conds opts =

    -- Check if any conditions are currently true,
    case checkCondTrue time conds of

      -- if so we can run one rather than waiting.
      Left k -> Left (k time)

      -- If all the conditions are evermore false...
      Right [] | opts      -> Right [(konst False, \time' -> PSt time' [] [])]
               | otherwise -> Right []

      -- Otherwise, all conditions are either false or are unknown.
      Right otherConds ->

        -- We look at the remaining conditions and check if there is
        -- a time at which one of the conditions will become true.
        case Obs.earliestTimeHorizon time otherConds of

          -- Of course, there may be no such time, in which case we
          -- simply return a new Wait using the remaining conditions
          Nothing -> Right otherConds

          -- but if this time does exists (call it the time horizon)
          -- then we can use it to simplify or eliminate the
          -- remaining conditions.
          -- Note that we also get the continuation step associated
          -- with the condition that becomes true at the horizon.
          Just (horizon, k) ->

            -- For each remaining condition we try to simplify it
            -- based on the knowledge that the time falls in the
            -- range between now and the time horizon (exclusive).
            -- If a condition will be false for the whole of this
            -- time range then it can be eliminated.
            let simplifiedConds = [ (obs', k')
                                  | (obs,  k') <- otherConds
                                  , let obs' = Obs.simplifyWithinHorizon
                                                 time horizon obs
                                  , not (Obs.isFalse time obs') ]

               -- It is possible that all the conditions are false
               -- in the time period from now up to (but not
               -- including) the horizon.
            in if null simplifiedConds

                 -- In that case the condition associated with the
                 -- time horizon will become true first, and we
                 -- can advance time to the horizon and follow its
                 -- associated continuation.
                 then if opts then Right [(at horizon, k)]
                              else Left (k horizon)

                 -- Otherwise, we return a new Wait, using the
                 -- simplified conditions
                 else Right ((at horizon, k) : simplifiedConds)

checkCondTrue :: Time -> [(Obs Bool, a)] -> Either a [(Obs Bool, a)]
checkCondTrue time conds
  | ((_,k) :_) <- trueConds = Left  k
  | otherwise               = Right otherConds'
  where
    (trueConds, otherConds) = partition (Obs.isTrue time . fst) conds
    otherConds' = filter (not . Obs.evermoreFalse time . fst) otherConds
