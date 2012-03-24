-- |Netrium is Copyright Netrium Consulting Ltd, 2009-2012, and files herein are licensed
-- |under the Affero General Public License version 3, the text of which can
-- |be found in agpl.txt, or any later version of the AGPL, unless otherwise
-- |noted. 
--
{-# LANGUAGE DeriveFunctor, GADTs, PatternGuards #-}

module DecisionTree where

import Contract
import Observable (Steps(..))
import qualified Observable as Obs
import Display

import Prelude hiding (product, until, and)
import Data.List hiding (and)


-- ---------------------------------------------------------------------------
-- * Contract decision trees
-- ---------------------------------------------------------------------------

-- | A single step in a decision tree
--
data DecisionStep x = Done
                    | Trade Party Double Tradeable x

                    | Choose Party ChoiceId     x x
                    | ObserveCond  (Obs Bool)   x x
                    | ObserveValue (Obs Double) (Double -> x)

                      -- Waiting for any observable to become true,
                      -- or alternativly we can exercise an option that is
                      -- available in the current state
                    | Wait [(Obs Bool, Time -> x)] [(ChoiceId, Time -> x)]
  deriving Functor

data Party = Party | Counterparty
  deriving (Eq, Show)

reverseParty :: Party -> Party
reverseParty Party = Counterparty
reverseParty Counterparty = Party

-- | A full decision tree
--
data DecisionTree = DecisionTree Time (DecisionStep DecisionTree)

unfoldDecisionTree :: (a -> (DecisionStep a, Time)) -> a -> DecisionTree
unfoldDecisionTree next = unfold
  where
    unfold x = case next x of
                 (step, time) -> DecisionTree time (fmap unfold step)

decisionTree :: Time -> Contract -> DecisionTree
decisionTree t c = unfoldDecisionTree
                     (\pst@(PSt t' _ _) -> (decisionStep pst, t'))
                     (initialProcessState t c)

-- ---------------------------------------------------------------------------
-- * Basic step
-- ---------------------------------------------------------------------------

data ProcessState = PSt Time                   -- ^ current time
                        [Blocked ThreadState]  -- ^ blocked
                        [ThreadState]          -- ^ runnable
  deriving Show

data ThreadState  = TSt Contract          -- ^ remaining contract
                        [Obs Bool]        -- ^ 'until' conditions
                        ScaleFactor       -- ^ inherited scaling
                        Party             -- ^ direction of trade
  deriving Show

data Blocked c =

     -- | waiting for obs to become True
     BlockedOnWhen                  (Obs Bool) c

     -- | waiting for obs to become value v
   | BlockedOnAnytime Bool ChoiceId (Obs Bool) c
  deriving Show


initialProcessState :: Time -> Contract -> ProcessState
initialProcessState time contract =
  let initialThread = TSt contract [] 1 Party
   in PSt time [] [initialThread]


currentContract :: ProcessState -> Contract
currentContract (PSt _time blocked runnable) =
    allOf
      [ giveTo party
          (scaleBy scaleFactor
            (foldr until contract untilObss))
      | let threads = runnable ++ map unBlocked blocked
      , TSt contract untilObss scaleFactor party <- threads
      , contract /= zero ]
  where
    giveTo Party        = id
    giveTo Counterparty = give

    scaleBy 1.0 c = c
    scaleBy s   c = scale (konst s) c

    unBlocked (BlockedOnWhen          o cth) = update (when o) cth
    unBlocked (BlockedOnAnytime _ cid o cth) = update (anytime cid o) cth
    update f (TSt c uos sf p) = TSt (f c) uos sf p

    allOf [] = zero
    allOf xs = foldr1 and xs


decisionStep :: ProcessState -> DecisionStep ProcessState
decisionStep (PSt time blocked runnable) =
    go blocked runnable

  where
    -- We have at least one runnable thread
    go bs (TSt c uos sf p:rs) = case c of
      Zero             -> go bs rs

      One t            -> Trade p sf t (PSt time bs rs)

      Give c1          -> let r' = (TSt c1 uos sf (reverseParty p))
                           in go bs (r':rs)

      And c1 c2        -> let r1 = TSt c1 uos sf p
                              r2 = TSt c2 uos sf p
                           in go bs (r1:r2:rs)

      Or cid c1 c2     -> let r1 = TSt c1 uos sf p
                              r2 = TSt c2 uos sf p
                           in Choose p cid (PSt time bs (r1:rs))
                                           (PSt time bs (r2:rs))

      Cond o c1 c2     -> let r1 = TSt c1 uos sf p
                              r2 = TSt c2 uos sf p
                           in ObserveCond o (PSt time bs (r1:rs))
                                            (PSt time bs (r2:rs))

      Scale o c1       -> let r' v = TSt c1 uos (v * sf) p
                           in ObserveValue o (\v -> PSt time bs (r' v:rs))

      Read n o c1      -> let r' v = TSt (subst n v c1) uos sf p
                           in ObserveValue o (\v -> PSt time bs (r' v:rs))

      When o c1        -> let b = BlockedOnWhen o (TSt c1 uos sf p)
                           in go (b:bs) rs

      Anytime cid o c1 -> let b = BlockedOnAnytime True cid o (TSt c1 uos sf p)
                           in go (b:bs) rs

      Until o c1       -> let r' = TSt c1 (o:uos) sf p
                           in ObserveCond  o (PSt time bs rs)
                                             (PSt time bs (r':rs))

    -- No threads at all, we're done
    go [] [] = Done

    -- All threads are blocked
    go bs [] = Wait (whens ++ untils) opts

      where
        -- the threads blocked on 'when'/'anytime'
        whens  = [ ( blockedObs b
                   , case nextThread b of
                        Left  r' -> \time' -> PSt time'     bs'  [r']
                        Right b' -> \time' -> PSt time' (b':bs') [] )
                 | (b, bs') <- each bs ]

        -- some blocked threads also have 'until' conditions
        untils = [ ( uo
                   , \time' -> PSt time' bs' [] )
                 | (b, bs') <- each bs
                 , let TSt _ uos _ _ = blockedThr b
                 , uo <- uos ]

        -- blocked 'anytime' threads, for which their observable is currently
        -- true, give us an option that we may choose to exercise
        -- Note: BlockedOnAnytime False means waiting for it to *become* False
        opts   = [ (cid, \time' -> PSt time' bs' [r'])
                 | (BlockedOnAnytime False cid _ r', bs') <- each bs ]

        -- the observable that it is blocking on,
        -- remember that we're waiting for the obs to become True
        blockedObs (BlockedOnWhen            o _) = o
        blockedObs (BlockedOnAnytime True  _ o _) = o
        -- so invert for blocked anytime threads where we're waiting for False:
        blockedObs (BlockedOnAnytime False _ o _) = Obs.not o

        blockedThr (BlockedOnWhen        _ x) = x
        blockedThr (BlockedOnAnytime _ _ _ x) = x

        -- either the new runnable thread or new blocked thread
        nextThread (BlockedOnWhen          _ r) = Left r
        nextThread (BlockedOnAnytime v cid o b) =
          Right (BlockedOnAnytime (not v) cid o b)

subst :: String -> Double -> Contract -> Contract
subst n v c = case c of
      Zero         -> c
      One _        -> c
      Give c1      -> Give  (subst n v c1)
      And  c1 c2   -> And   (subst n v c1) (subst n v c2)
      Or  id c1 c2 -> Or id (subst n v c1) (subst n v c2)
      Cond o c1 c2 -> Cond  (Obs.subst n v o) (subst n v c1) (subst n v c2)
      Scale o c1   -> Scale (Obs.subst n v o) (subst n v c1)
      Read n' o c1
        | n == n'  -> Read n' (Obs.subst n v o) c1
        | otherwise-> Read n' (Obs.subst n v o) (subst n v c1)
      When        o c1 -> When    (Obs.subst n v o) (subst n v c1)
      Anytime cid o c1 -> Anytime cid (Obs.subst n v o) (subst n v c1)
      Until       o c1 -> Until   (Obs.subst n v o) (subst n v c1)


each :: [a] -> [(a, [a])]
each xs = [ (xs !! n, [ x' | (n',x') <- nxs, n' /= n ] )
          | n <- [0..length xs-1] ]
  where
    nxs = zip [0..] xs

-- ---------------------------------------------------------------------------
-- * Display tree instance
-- ---------------------------------------------------------------------------

instance Show (DecisionStep x) where
  show  Done                = "Done"
  show (Trade Party n t _)  = "Receive " ++ show n ++ " " ++ show t
  show (Trade _     n t _)  = "Provide " ++ show n ++ " " ++ show t
  show (Choose p cid _ _)  = "Choose " ++ show p ++ " " ++ cid
  show (ObserveCond  o _ _) = "ObserveCond " ++ show o
  show (ObserveValue o _)   = "ObserveValue " ++ show o
  show (Wait conds opts)    = "Wait for one to become true...\n"
                           ++ unlines (map (show . fst) conds)
                           ++ "Or pick an available option\n"
                           ++ unlines (map (show . fst) opts)

instance Display DecisionTree where
  toTree (DecisionTree time st) = case st of
    Done                -> Node "done" []
    Trade Party n t st1  -> Node ("receive " ++ show n ++ " " ++ show t ++ "\n" ++ show time)
                                [toTree st1]
    Trade _     n t st1  -> Node ("provide " ++ show n ++ " " ++ show t ++ "\n" ++ show time)
                                [toTree st1]

    Choose Party cid st1 st2 -> Node ("choose " ++ cid ++ "\n" ++ show time)
                                        [toTree st1, toTree st2]
    Choose _     cid st1 st2 -> Node ("counterparty choice " ++ cid ++ "\n" ++ show time)
                                        [toTree st1, toTree st2]
    ObserveCond obs st1 st2 -> Node "observe cond" [toTree obs
                                                   ,toTree st1
                                                   ,toTree st2]
    ObserveValue obs st1 -> Node "observe val" [ toTree obs
                                              , case Obs.eval time obs of
                                                  Result v -> toTree (st1 v)
                                                  _        -> toTree (st1 0) ]
    Wait conds opts     -> Node ("wait\n" ++ show time)
                              $ [ Node (show obs)
                                       [ case Obs.timeHorizon time obs of
                                           Nothing    -> toTree (cont time)
                                           Just time' -> toTree (cont time') ]
                                | (obs, cont) <- conds ]
                             ++ [ Node "option" [toTree (cont time)]
                                | (_c, cont) <- opts ]
