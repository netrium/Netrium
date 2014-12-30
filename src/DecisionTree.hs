-- |Netrium is Copyright Anthony Waite, Dave Hetwett, Shaun Laurens 2009-2015, and files herein are licensed
-- |under the MIT license,  the text of which can be found in license.txt
--
{-# LANGUAGE DeriveFunctor, GADTs, PatternGuards #-}

module DecisionTree where

import Contract
import Observable (Steps(..))
import qualified Observable as Obs
import Display

import Prelude hiding (product, until, and)
import Data.List hiding (and)
import Control.Monad hiding (when)
import Text.XML.HaXml.XmlContent

-- ---------------------------------------------------------------------------
-- * Contract decision trees
-- ---------------------------------------------------------------------------

-- | A single step in a decision tree
--
data DecisionStep x = Done
                    | Trade TradeDir Double Tradeable x

                    | Choose Party ChoiceId     x x
                    | ObserveCond  (Obs Bool)   x x
                    | ObserveValue (Obs Double) (Double -> x)

                      -- Waiting for any observable to become true,
                      -- or alternativly we can exercise an option that is
                      -- available in the current state
                    | Wait [(Obs Bool, Time -> x)] [(ChoiceId, Time -> x)]
  deriving Functor

data Party = FirstParty | Counterparty | ThirdParty PartyName
  deriving (Eq, Show)

-- See also data TradeDir below

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
  deriving (Show, Eq)

data ThreadState  = TSt Contract          -- ^ remaining contract
                        [Obs Bool]        -- ^ 'until' conditions
                        ScaleFactor       -- ^ inherited scaling
                        TradeDir          -- ^ direction of trade
  deriving (Show, Eq)

data Blocked c =

     -- | waiting for obs to become True
     BlockedOnWhen                  (Obs Bool) c

     -- | waiting for obs to become value v
   | BlockedOnAnytime Bool ChoiceId (Obs Bool) c
  deriving (Show, Eq)


initialProcessState :: Time -> Contract -> ProcessState
initialProcessState time contract =
  let initialThread = TSt contract [] 1 TradeDir2To1
   in PSt time [] [initialThread]


currentContract :: ProcessState -> Contract
currentContract (PSt _time blocked runnable) =
    allOf
      [ evalTradeDir tradeDir
          (scaleBy scaleFactor
            (foldr until contract untilObss))
      | let threads = runnable ++ map unBlocked blocked
      , TSt contract untilObss scaleFactor tradeDir <- threads
      , contract /= zero ]
  where
    scaleBy 1.0 c = c
    scaleBy s   c = scale (konst s) c

    unBlocked (BlockedOnWhen          o cth) = update (when o) cth
    unBlocked (BlockedOnAnytime _ cid o cth) = update (anytime cid o) cth
    update f (TSt c uos sf d) = TSt (f c) uos sf d

    allOf [] = zero
    allOf xs = foldr1 and xs


decisionStep :: ProcessState -> DecisionStep ProcessState
decisionStep (PSt time blocked runnable) =
    go blocked runnable

  where
    -- We have at least one runnable thread
    go bs (TSt c uos sf d:rs) = case c of
      Zero             -> go bs rs

      One t            -> Trade d sf t (PSt time bs rs)

      Give c1          -> let r' = TSt c1 uos sf (flipTradeDir d)
                           in go bs (r':rs)

      Party p c1       -> let d' = setThirdParty p d
                              r' = TSt c1 uos sf d'
                           in go bs (r':rs)

      And c1 c2        -> let r1 = TSt c1 uos sf d
                              r2 = TSt c2 uos sf d
                           in go bs (r1:r2:rs)

      Or cid c1 c2     -> let r1 = TSt c1 uos sf d
                              r2 = TSt c2 uos sf d
                              (p,_) = tradeDirParties d
                           in Choose p cid (PSt time bs (r1:rs))
                                           (PSt time bs (r2:rs))

      Cond o c1 c2     -> let r1 = TSt c1 uos sf d
                              r2 = TSt c2 uos sf d
                           in ObserveCond o (PSt time bs (r1:rs))
                                            (PSt time bs (r2:rs))

      Scale o c1       -> let r' v = TSt c1 uos (v * sf) d
                           in ObserveValue o (\v -> PSt time bs (r' v:rs))

      Read n o c1      -> let r' v = TSt (subst n v c1) uos sf d
                           in ObserveValue o (\v -> PSt time bs (r' v:rs))

      When o c1        -> let b = BlockedOnWhen o (TSt c1 uos sf d)
                           in go (b:bs) rs

      Anytime cid o c1 -> let b = BlockedOnAnytime True cid o (TSt c1 uos sf d)
                           in go (b:bs) rs

      Until o c1       -> let r' = TSt c1 (o:uos) sf d
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
      Party p c1   -> Party p (subst n v c1)
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
-- * Trade directions
-- ---------------------------------------------------------------------------

-- Warning: whis is all rather subtle.

-- It is for handling the 'party' contract combinator and its interactions with
-- the 'give' combinator. The point of 'party' is to transfer the rights and
-- obligations of the implicit counterparty to an explicit named third party.
-- Using various combinations of 'party' and 'give' we can construct trades in
-- either direction between any pair of 1st, 2nd and named 3rd parties.
--
-- There is an algebra relating the combinators, in particular the laws:
--
-- > give    . give    = id
-- > party q . party p = party p
--
-- and a more subtle one:
--
-- > give . party q . give . party p = party q . give . party p
--
-- This says that once we have a trade between two third parties it is no
-- longer affected by 'give', because 'give' only swaps between the 1st and 2nd
-- parties.
--
-- Combined, this means that there's actually only a finite number of
-- combinations of 'give' and 'party', the following eight:

data TradeDir
   = TradeDir2To1            --        id              2nd --> 1st party
   | TradeDir1To2            -- give . id              1st --> 2nd party

   | TradeDirPTo1 PartyName  --        party p         named 3rd --> 1st party
   | TradeDirPTo2 PartyName  -- give . party p         named 3rd --> 2nd party

   | TradeDir1ToP PartyName  --        party p . give  1st --> named 3rd party
   | TradeDir2ToP PartyName  -- give . party p . give  2nd --> named 3rd party

   | TradeDirPToQ PartyName PartyName -- party q . give . party p          p --> q
   | TradeDirQToP PartyName PartyName -- party q . give . party p . give   q --> p
  deriving (Show, Eq)

-- | Give the interpretation of a TradeDir as a combination
-- of 'party' and 'give'.
--
evalTradeDir :: TradeDir -> (Contract -> Contract)
evalTradeDir TradeDir2To1       = id
evalTradeDir TradeDir1To2       = give
evalTradeDir (TradeDirPTo1 p)   =        party p
evalTradeDir (TradeDirPTo2 p)   = give . party p
evalTradeDir (TradeDir1ToP p)   =        party p . give
evalTradeDir (TradeDir2ToP p)   = give . party p . give
evalTradeDir (TradeDirPToQ p q) = party q . give . party p
evalTradeDir (TradeDirQToP p q) = party q . give . party p . give

-- | Precompose a TradeDir with 'party' to get a new combined TradeDir.
--
-- That is, it respects the law:
--
-- > evalTradeDir (setThirdParty p dir) = evalTradeDir dir . party p
--
setThirdParty :: PartyName -> TradeDir -> TradeDir
setThirdParty p TradeDir2To1     = TradeDirPTo1 p    -- id   . party p     ~~ TradeDirPTo1 p
setThirdParty p TradeDir1To2     = TradeDirPTo2 p    -- give . party p     ~~ TradeDirPTo2 p
setThirdParty p (TradeDirPTo1 q) = TradeDirPTo1 p    --        party q . party p =        party p  ~~ TradeDirPTo1 p
setThirdParty p (TradeDirPTo2 q) = TradeDirPTo2 p    -- give . party q . party p = give . party p  ~~ TradeDirPTo2 p
setThirdParty p (TradeDir1ToP q) = TradeDirPToQ p q  --        party q . give . party p                            ~~ TradeDirPToQ p q
setThirdParty p (TradeDir2ToP q) = TradeDirPToQ p q  -- give . party q . give . party p = party q . give . party p ~~ TradeDirPToQ p q
setThirdParty p (TradeDirPToQ q r) = TradeDirPToQ p r  -- party r . give . party q .        party p = party r . give . party p ~~ TradeDirPToQ p r
setThirdParty p (TradeDirQToP q r) = TradeDirPToQ p q  -- party r . give . party q . give . party p = party q . give . party p ~~ TradeDirPToQ p q

-- | Precompose a TradeDir with 'give' to get a new combined TradeDir.
--
-- That is, it respects the law:
--
-- > evalTradeDir (flipTradeDir dir) = evalTradeDir dir . give
--
flipTradeDir :: TradeDir -> TradeDir
flipTradeDir  TradeDir2To1    = TradeDir1To2   -- id . give = give
flipTradeDir  TradeDir1To2    = TradeDir2To1   -- give . give = id
flipTradeDir (TradeDirPTo1 p) = TradeDir1ToP p -- party p . give
flipTradeDir (TradeDirPTo2 p) = TradeDir2ToP p -- give . party p . give
flipTradeDir (TradeDir1ToP p) = TradeDirPTo1 p -- party p . give . give = party p
flipTradeDir (TradeDir2ToP p) = TradeDirPTo2 p -- give . party p . give . give = give . party p
flipTradeDir (TradeDirPToQ p q) = TradeDirQToP p q -- party q . give . party p . give
flipTradeDir (TradeDirQToP p q) = TradeDirPToQ p q -- party q . give . party p . give . give = party q . give . party p

-- | Return the two parties in a TradeDir in the order @(recieving party, giving party)@.
--
tradeDirParties :: TradeDir -> (Party, Party)
tradeDirParties  TradeDir2To1      = (FirstParty,   Counterparty)
tradeDirParties  TradeDir1To2      = (Counterparty, FirstParty)
tradeDirParties (TradeDirPTo1 p)   = (FirstParty,   ThirdParty p)
tradeDirParties (TradeDirPTo2 p)   = (Counterparty, ThirdParty p)
tradeDirParties (TradeDir1ToP p)   = (ThirdParty p, FirstParty)
tradeDirParties (TradeDir2ToP p)   = (ThirdParty p, Counterparty)
tradeDirParties (TradeDirPToQ p q) = (ThirdParty q, ThirdParty p)
tradeDirParties (TradeDirQToP p q) = (ThirdParty p, ThirdParty q)


-- ---------------------------------------------------------------------------
-- * Display tree instance
-- ---------------------------------------------------------------------------

instance Show (DecisionStep x) where
  show  Done                = "Done"
  show (Trade dir n t _)    =  case dir of
                                 TradeDir2To1   -> "Receive " ++ quantityOfStuff
                                 TradeDir1To2   -> "Provide " ++ quantityOfStuff

                                 TradeDirPTo1 p -> "Receive from " ++ partyQuantityOfStuff p
                                 TradeDirPTo2 p -> "Counterparty receives from " ++ partyQuantityOfStuff p

                                 TradeDir1ToP p -> "Provide to " ++ partyQuantityOfStuff p
                                 TradeDir2ToP p -> "Counterparty provides to " ++ partyQuantityOfStuff p

                                 TradeDirPToQ p q -> p ++ " provides to " ++ q ++ " " ++ quantityOfStuff
                                 TradeDirQToP p q -> q ++ " provides to " ++ p ++ " " ++ quantityOfStuff
                               where
                                 quantityOfStuff = show n ++ " " ++ show t
                                 partyQuantityOfStuff p = p ++ " " ++ quantityOfStuff

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
    Trade dir n t st1   -> Node descr [toTree st1]
                           where
                             descr = dirDescr ++ " " ++ show n ++ " " ++ show t
                                              ++ "\n" ++ show time
                             dirDescr = case dir of
                               TradeDir2To1 -> "receive"
                               TradeDir1To2 -> "provide"

                               TradeDirPTo1 p -> "receive from " ++ p
                               TradeDirPTo2 p -> "counterparty receives from " ++ p

                               TradeDir1ToP p -> "provide to " ++ p
                               TradeDir2ToP p -> "counterparty provides to " ++ p

                               TradeDirPToQ p q -> p ++ " provides to " ++ q
                               TradeDirQToP p q -> q ++ " provides to " ++ p

    Choose p cid st1 st2 -> Node (partyDescr ++ cid ++ "\n" ++ show time)
                                 [toTree st1, toTree st2]
                            where
                              partyDescr = case p of
                                FirstParty   -> "choose "
                                Counterparty -> "counterparty choice "
                                ThirdParty p -> "3rd party " ++ p ++ " choice "
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

-- XML instances
instance HTypeable Party where
    toHType _ = Defined "Party" [] []

instance XmlContent Party where
  parseContents = do
    e@(Elem t _ _) <- element ["Party", "Counterparty", "ThirdParty"]
    commit $ interior e $ case t of
      "Party"        -> return FirstParty
      "Counterparty" -> return Counterparty
      "ThirdParty"   -> liftM  ThirdParty text

  toContents FirstParty     = [mkElemC "Party"  []]
  toContents Counterparty   = [mkElemC "Counterparty" []]
  toContents (ThirdParty p) = [mkElemC "ThirdParty" (toText p)]

instance HTypeable TradeDir where
    toHType _ = Defined "TradeDir" [] []

instance XmlContent TradeDir where
  parseContents = do
    e@(Elem t _ _) <- element ["TradeDir2To1","TradeDir1To2"
                              ,"TradeDirPTo1","TradeDirPTo2"
                              ,"TradeDir1ToP","TradeDir2ToP"
                              ,"TradeDirPToQ","TradeDirQToP"]
    commit $ interior e $ case t of
      "TradeDir2To1" -> return TradeDir2To1
      "TradeDir1To2" -> return TradeDir1To2
      "TradeDirPTo1" -> liftM TradeDirPTo1 text
      "TradeDirPTo2" -> liftM TradeDirPTo2 text
      "TradeDir1ToP" -> liftM TradeDir1ToP text
      "TradeDir2ToP" -> liftM TradeDir2ToP text
      "TradeDirPToQ" -> liftM2 TradeDirPToQ text text
      "TradeDirQToP" -> liftM2 TradeDirQToP text text

  toContents TradeDir2To1     = [mkElemC "TradeDir2To1"  []]
  toContents TradeDir1To2     = [mkElemC "TradeDir1To2"  []]
  toContents (TradeDirPTo1 p) = [mkElemC "TradeDirPTo1" (toText p)]
  toContents (TradeDirPTo2 p) = [mkElemC "TradeDirPTo2" (toText p)]
  toContents (TradeDir1ToP p) = [mkElemC "TradeDir1ToP" (toText p)]
  toContents (TradeDir2ToP p) = [mkElemC "TradeDir2ToP" (toText p)]
  toContents (TradeDirPToQ p q) = [mkElemC "TradeDirPToQ" (toText p ++ toText q)]
  toContents (TradeDirQToP p q) = [mkElemC "TradeDirQToP" (toText p ++ toText q)]

instance HTypeable (Blocked c) where
    toHType _ = Defined "Blocked" [] []

instance XmlContent c => XmlContent (Blocked c) where
  parseContents = do
    e@(Elem t _ _) <- element ["BlockedOnWhen", "BlockedOnAnytime"]
    commit $ interior e $ case t of
      "BlockedOnWhen"    -> liftM2 BlockedOnWhen (fmap unObsCondition parseContents)
                                                 parseContents
      "BlockedOnAnytime" -> liftM4 BlockedOnAnytime parseContents
                                                    (inElement "ChoiceId" text)
                                                    (fmap unObsCondition parseContents)
                                                    parseContents

  toContents (BlockedOnWhen obs c) =
    [mkElemC "BlockedOnWhen"  (toContents (ObsCondition obs) ++ toContents c)]
  toContents (BlockedOnAnytime val cid obs c) =
    [mkElemC "BlockedOnAnytime" (toContents val ++ [mkElemC "ChoiceId" (toText cid)] ++
                                 toContents (ObsCondition obs) ++ toContents c)]

newtype ObsCondition = ObsCondition { unObsCondition :: (Obs Bool) }

instance HTypeable ObsCondition where
    toHType _ = Defined "ObsCondition" [] []

instance XmlContent ObsCondition where
  parseContents = inElement "ObsCondition" $
                    liftM ObsCondition Obs.parseObsCond
  toContents (ObsCondition obs) = [mkElemC "ObsCondition" [Obs.printObs obs]]

instance HTypeable ThreadState where
    toHType _ = Defined "ThreadState" [] []

instance XmlContent ThreadState where
  parseContents =
    inElement "ThreadState" $
      liftM4 TSt parseContents
                 (inElement "UntilConditions" (fmap (map unObsCondition) parseContents))
                 parseContents parseContents
  toContents (TSt c obss sf dir) =
    [mkElemC "ThreadState" (toContents c ++
                            mkElemC "UntilConditions" (toContents (map ObsCondition obss)) :
                            toContents sf ++ toContents dir)]

instance HTypeable ProcessState where
    toHType _ = Defined "ProcessState" [] []

instance XmlContent ProcessState where
  parseContents =
    inElement "ProcessState" $
      liftM3 PSt parseContents
                 (inElement "BlockedThreads" parseContents)
                 (inElement "RunnableThreads" parseContents)
  toContents (PSt t blocked runnable) =
    [mkElemC "ProcessState" (toContents t ++
                           [ mkElemC "BlockedThreads" (toContents blocked)
                           , mkElemC "RunnableThreads" (toContents runnable) ])]
