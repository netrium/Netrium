-- |Netrium is Copyright Anthony Waite, Dave Hetwett, Shaun Laurens 2009-2015, and files herein are licensed
-- |under the MIT license,  the text of which can be found in license.txt
--
{-# LANGUAGE GADTs, MultiParamTypeClasses, FlexibleInstances #-}
--
-- The definition of observable expressions
module Observable (

    -- * Creating observables
    Obs(..),
    konst,
    -- ** Named observables
    VarName, primVar, primCond, var,
    -- ** Time-based observables
    Time, mkdate,
    at, before, after, between,
    -- ** Operators
    -- | Comparison, logical and numeric operators
    (%==), (%>), (%>=), (%<), (%<=),
    (%&&), (%||), (%+), (%-), (%*), (%/),
    -- ** Other observable functions
    ifthen, negate, not, max, min, abs,

    -- * Other utilities on observables
    -- ** Parsing
    parseObsCond, parseObsReal, printObs,

    -- ** Evaluating
    eval, Steps(..),
    subst,

    -- ** Analysing
    isTrue, isFalse,
    nextTrue, nextFalse,
    evermoreTrue, evermoreFalse,
    timeHorizon, earliestTimeHorizon, simplifyWithinHorizon,

  ) where

import Display
import XmlUtils

import Prelude hiding (product, not, min, max)
import qualified Prelude
import Data.Time hiding (Day)
import Data.List (minimumBy)
import Data.Ord (comparing)
import Control.Monad
import Text.Show.Functions ()
import Text.XML.HaXml.XmlContent
  (XMLParser(), Element(..), Content(), element, interior, text, toText, mkElemC)

-- * Observable type definition
-- | We use a continuous model of time.
type Time  = UTCTime

-- | Convenience function to create a time from a date.
--
mkdate :: Integer -> Int -> Int -> Time
mkdate year month day = UTCTime (fromGregorian year month day) 0

-- | A variable name
type VarName = String

-- | A simple expression language of \"observable values\".
-- An observable represents is a time-varying value (a function from
-- 'Time' to a value).
--
-- Currently there are two types of observables:
--
--  * condition observables, type @Obs Bool@
--
--  * real-valued observables, type @Obs Double@
--
data Obs a where
  Const     :: (Show a, Eq a) => a -> Obs a  --  constant value
  Var       :: VarName -> Obs Double  --  from local environment
  NamedVal  :: VarName -> Obs Double  --  from external environment
  NamedCond :: VarName -> Obs Bool    --  from external environment

  At        :: Time -> Obs Bool   -- time == t
  After     :: Time -> Obs Bool   -- time >= t
  Before    :: Time -> Obs Bool   -- time <  t

  UnOp      :: UnOp  a b   -> Obs a -> Obs b
  BinOp     :: BinOp a a b -> Obs a -> Obs a -> Obs b
  IfThen    :: Obs Bool    -> Obs a -> Obs a -> Obs a

-- Time operations are sufficiently important, and restricted that we treat
-- them specially. In particular it is important that the only time ranges
-- that we can construct are inclusive below and exclusive above.

-- Note that 'Not (At _)' is a semantic error. Remember that we work with a
-- continuous model of time. At what time would it become false?

-- | Unary operators
data UnOp a b where
  Not       :: UnOp Bool Bool
  Neg       :: UnOp Double Double
  Abs       :: UnOp Double Double
  Sqrt      :: UnOp Double Double
  Exp       :: UnOp Double Double
  Log       :: UnOp Double Double
  Sin       :: UnOp Double Double
  Cos       :: UnOp Double Double
  Asin      :: UnOp Double Double
  Atan      :: UnOp Double Double
  Acos      :: UnOp Double Double
  Sinh      :: UnOp Double Double
  Cosh      :: UnOp Double Double
  Asinh     :: UnOp Double Double
  Acosh     :: UnOp Double Double
  Atanh     :: UnOp Double Double

-- |Binary operators
data BinOp a b c where
  And       :: BinOp Bool Bool Bool
  Or        :: BinOp Bool Bool Bool

  Eq        :: BinOp Double Double Bool

  Gt        :: BinOp Double Double Bool
  Gte       :: BinOp Double Double Bool
  Lt        :: BinOp Double Double Bool
  Lte       :: BinOp Double Double Bool

  Add       :: BinOp Double Double Double
  Sub       :: BinOp Double Double Double
  Mul       :: BinOp Double Double Double
  Div       :: BinOp Double Double Double

  Min       :: BinOp Double Double Double
  Max       :: BinOp Double Double Double

-- Notice that Obs Bool have this structure:
--
-- The top level expression is logical combination of Obs Bool "atoms"
--
-- bexp ::= atom
--        | Not bexp
--        | And bexp bexp
--        | Or  bexp bexp
--
-- There are various Obs Bool "atoms"
--
-- atom ::= Const Bool
--        | NamedCond String
--        | Before/After/At Time
--        | Eq/Gt/Gte/Lt/Lte a a


-- * The basics
-- ** Variables
-- | A constant observable.
konst :: (Show a, Eq a) => a -> Obs a
konst a = Const a

-- | A named interal contract program variable.
--
-- Usually you should use 'letin' rather than this directly.
var      :: VarName -> Obs Double

-- | A named external real-valued observable
--
-- Example:
--
-- > primVar "gas-price"
primVar  :: VarName -> Obs Double

-- | A named external condition observable
primCond :: VarName -> Obs Bool

var      = Var
primVar  = NamedVal
primCond = NamedCond

-- ** Time

-- | An observable that becomes true at a single given point in time
-- and is false at all other times.
at :: Time -> Obs Bool
at     = At
-- | An observable that becomes true after a given point in time and is
-- false prior to that time.
after :: Time -> Obs Bool
after = After
-- | An observable that is true up to a given point in time and is false
-- thereafter.
before :: Time -> Obs Bool
before = Before


-- | An observable that is true between two given points in time
-- and is false at all other times.
--
-- @between t1 t2 = time >= t1 && time < t2@
between :: Time -> Time -> Obs Bool
between t1 t2 = after t1  %&&  before t2

-- ** Other syntax and functions
-- | if..then..else for observables (returns an observable)
ifthen :: Obs Bool -> Obs a -> Obs a -> Obs a
ifthen   = IfThen

-- | Negate a boolean observable
not    :: Obs Bool   -> Obs Bool
not    = UnOp Not

min    = BinOp Min
max    = BinOp Max

-- * Operators
isInfix :: BinOp a b c -> Bool
isInfix Eq  = True
isInfix Gt  = True
isInfix Gte = True
isInfix Lt  = True
isInfix Lte = True
isInfix And = True
isInfix Or  = True
isInfix Add = True
isInfix Sub = True
isInfix Mul = True
isInfix Div = True
isInfix _   = False

infixl 7 %*, %/
infixl 6 %+, %-
infix  4 %==, %>, %>=, %<, %<=
infixr 3 %&&
infixr 2 %||

(%==) :: Obs Double -> Obs Double -> Obs Bool
(%==) = BinOp Eq

(%>), (%>=), (%<), (%<=) :: Obs Double -> Obs Double -> Obs Bool
(%>)  = BinOp Gt
(%>=) = BinOp Gte
(%<)  = BinOp Lt
(%<=) = BinOp Lte

(%&&), (%||) :: Obs Bool -> Obs Bool -> Obs Bool
(%&&) = BinOp And
(%||) = BinOp Or

(%+), (%-), (%*), (%/) :: Obs Double -> Obs Double -> Obs Double
(%+)  = BinOp Add
(%-)  = BinOp Sub
(%*)  = BinOp Mul
(%/)  = BinOp Div

-- | Equality
instance Eq (Obs a) where
  Const v1     == Const v2     = v1 == v2
  Var   n1     == Var   n2     = n1 == n2
  NamedVal  n1 == NamedVal  n2 = n1 == n2
  NamedCond n1 == NamedCond n2 = n1 == n2
  At t1        == At t2        = t1 == t2
  After  t1    == After  t2    = t1 == t2
  Before t1    == Before t2    = t1 == t2
  UnOp op1 x1  == UnOp op2 x2  = case deq op1 op2 of
                                   Just Refl -> x1 == x2
                                   Nothing   -> False

  BinOp op1 x1 y1 == BinOp op2 x2 y2 = case deq op1 op2 of
                                         Just Refl -> x1 == x2 && y1 == y2
                                         Nothing   -> False

  IfThen c1 x1 y1 == IfThen c2 x2 y2 = c1 == c2 && x1 == x2 && y1 == y2

  _ == _ = False

-- | Note that you can use ordinary Num operators like '+', '-', '*' etc
-- in observable expressions.
instance Num (Obs Double) where
  (+)    = BinOp Add
  (*)    = BinOp Mul
  (-)    = BinOp Sub
  negate = UnOp Neg
  abs    = UnOp Abs
  signum = error "Obs: signum not yet implemented"
  fromInteger = konst . fromInteger

-- | Double operations
instance Fractional (Obs Double) where
  (/)    = BinOp Div
  fromRational = konst . fromRational

-- | Floating operations
instance Floating (Obs Double) where
  pi    = Const pi
  sqrt  = UnOp Sqrt
  exp   = UnOp Exp
  log   = UnOp Log
  sin   = UnOp Sin
  cos   = UnOp Cos
  asin  = UnOp Asin
  atan  = UnOp Atan
  acos  = UnOp Acos
  sinh  = UnOp Sinh
  cosh  = UnOp Cosh
  asinh = UnOp Asinh
  acosh = UnOp Acosh
  atanh = UnOp Atanh

-- * Observable operations
data Steps a = NeedNamedVal  Time VarName (Double -> Steps a)
             | NeedNamedCond Time VarName (Bool   -> Steps a)
             | Result a
  deriving Show

-- | Evaluate an observable at a given time
eval :: Time -> Obs a -> Steps a
eval time = flip eval' Result
  where
    eval' :: Obs b -> (b -> Steps a) -> Steps a
    eval' (Const     v)        k = k v
    eval' (Var       name)     _ = error ("unbound var " ++ name)
    eval' (NamedVal  name)     k = NeedNamedVal  time name k
    eval' (NamedCond name)     k = NeedNamedCond time name k

    eval' (At t)               k = k (time == t)
    eval' (After t)            k = k (time >= t)
    eval' (Before t)           k = k (time <  t)

    eval' (UnOp  op obs1)      k = eval' obs1 $ \v  -> k (evalUnOp op v)
    eval' (BinOp op obs1 obs2) k = eval' obs1 $ \v1 ->
                                   eval' obs2 $ \v2 -> k (evalBinOp op v1 v2)
    eval' (IfThen obsc obs1 obs2) k = eval' obsc $ \vc -> if vc
                                                            then eval' obs1 k
                                                            else eval' obs2 k

evalUnOp :: UnOp a b -> a -> b
evalUnOp Not   = Prelude.not
evalUnOp Neg   = Prelude.negate
evalUnOp Abs   = Prelude.abs
evalUnOp Sqrt  = sqrt
evalUnOp Exp   = exp
evalUnOp Log   = log
evalUnOp Sin   = sin
evalUnOp Cos   = cos
evalUnOp Asin  = asin
evalUnOp Atan  = atan
evalUnOp Acos  = acos
evalUnOp Sinh  = sinh
evalUnOp Cosh  = cosh
evalUnOp Asinh = asinh
evalUnOp Acosh = acosh
evalUnOp Atanh = atanh

evalBinOp :: BinOp a a b -> a -> a -> b
evalBinOp Eq  = (==)
evalBinOp Gt  = (>)
evalBinOp Gte = (>=)
evalBinOp Lt  = (<)
evalBinOp Lte = (<=)

evalBinOp And = (&&)
evalBinOp Or  = (||)

evalBinOp Add = (+)
evalBinOp Sub = (-)
evalBinOp Mul = (*)
evalBinOp Div = (/)
evalBinOp Min = (Prelude.min)
evalBinOp Max = (Prelude.max)

-- | The time horizon of an condition observable is earliest time that it
-- guaranteed to become true (or @Nothing@ if there is no such time)
timeHorizon :: Time -> Obs Bool -> Maybe Time
timeHorizon = nextTrue

-- | Return the earliest time horizon of a set of observables and the associate
-- tag of the observable that has the earliest time horizon (or @Nothing@ if
-- none of the observables have a time horizon)
earliestTimeHorizon :: Time -> [(Obs Bool, a)] -> Maybe (Time, a)
earliestTimeHorizon time os =
    maybeMinimumBy (comparing fst)
      [ (t, x)| (o, x) <- os, Just t <- [timeHorizon time o] ]
  where
    maybeMinimumBy _   [] = Nothing
    maybeMinimumBy cmp xs = Just (minimumBy cmp xs)

-- | Check if an observable is known to be true at a given point in time,
-- independent of knowledge of any external observables
isTrue :: Time -> Obs Bool -> Bool
isTrue time obs  = case eval time obs of
                     Result True -> True
                     _           -> False

-- | Check if an observable is known to be false at a given point in time,
-- independent of knowledge of any external observables
isFalse :: Time -> Obs Bool -> Bool
isFalse time obs = case eval time obs of
                     Result False -> True
                     _            -> False

-- | The next time that an observable is guaranteed to become true
nextTrue :: Time -> Obs Bool -> Maybe Time
nextTrue time obs = case obs of
  Const     True     -> Just time
  Const     False    -> Nothing
  NamedCond _        -> Nothing

  --  theTime == t
  --  -----------|--------------
  --   ^         ^         ^
  --   time      time      time
  At t
    | time <= t -> Just t
    | otherwise -> Nothing

  --             t <= theTime
  --  -----------[++++++++++++++
  --   ^         ^         ^
  --   time      time      time
  After t
    | time <= t -> Just t
    | otherwise -> Just time

  --   theTime < t
  --  +++++++++++)--------------
  --   ^         ^         ^
  --   time      time      time
  Before t
    | time < t  -> Just time
    | otherwise -> Nothing

  UnOp  Not obs1     -> nextFalse time obs1

  BinOp And obs1 obs2 ->
    case (nextTrue time obs1, nextTrue time obs2) of
      (Just t1, _      ) | isTrue t1 obs2 -> Just t1
      (_      , Just t2) | isTrue t2 obs1 -> Just t2
      (_      , _      )                  -> Nothing

  BinOp Or obs1 obs2 ->
    case (nextTrue time obs1, nextTrue time obs2) of
      (v1,      Nothing) -> v1
      (Nothing, v2)      -> v2
      (Just t1, Just t2) -> Just (Prelude.min t1 t2)

  BinOp Eq  _ _ -> Nothing
  BinOp Gt  _ _ -> Nothing
  BinOp Gte _ _ -> Nothing
  BinOp Lt  _ _ -> Nothing
  BinOp Lte _ _ -> Nothing
  IfThen _  _ _ -> Nothing

-- | The next time that an observable is guaranteed to become false
nextFalse :: Time -> Obs Bool -> Maybe Time
nextFalse time obs = case obs of
  Const     True     -> Nothing
  Const     False    -> Just time
  NamedCond _        -> Nothing

  At _               -> error "The observable 'not (at t)' is not valid"

  --             t <= theTime
  --  -----------[++++++++++++++
  --   ^         ^         ^
  --   time      time      time
  After t
    | time < t  -> Just time
    | otherwise -> Nothing

  --   theTime < t
  --  +++++++++++)--------------
  --   ^         ^         ^
  --   time      time      time
  Before t
    | time <= t -> Just t
    | otherwise -> Just time

  UnOp  Not obs1     -> nextTrue time obs1

  BinOp And obs1 obs2 ->
    case (nextFalse time obs1, nextFalse time obs2) of
      (v1,      Nothing) -> v1
      (Nothing, v2)      -> v2
      (Just t1, Just t2) -> Just (Prelude.min t1 t2)

  BinOp Or obs1 obs2 ->
    case (nextFalse time obs1, nextFalse time obs2) of
      (Just t1, _      ) | isFalse t1 obs2 -> Just t1
      (_      , Just t2) | isFalse t2 obs1 -> Just t2
      (_      , _      )                   -> Nothing

  BinOp Eq  _ _ -> Nothing
  BinOp Gt  _ _ -> Nothing
  BinOp Gte _ _ -> Nothing
  BinOp Lt  _ _ -> Nothing
  BinOp Lte _ _ -> Nothing
  IfThen _  _ _ -> Nothing


simplifyWithinHorizon :: Time -> Time -> Obs Bool -> Obs Bool
simplifyWithinHorizon time horizon = simplify
  where
    simplify :: Obs a -> Obs a
    simplify obs = case obs of

      --             t <= theTime
      --  -----------[++++++++++++++
      --   ^         ^         ^
      --   time      time      time
      After t
        | horizon < t -> Const False
        | t <= time   -> Const True

      --   theTime < t
      --  +++++++++++)--------------
      --   ^         ^         ^
      --   time      time      time
      Before t
        | horizon < t -> Const True
        | t <= time   -> Const False

      BinOp And obs1 obs2 ->
        case (simplify obs1, simplify obs2) of
          (Const True, obs2'     ) -> obs2'
          (obs1'     , Const True) -> obs1'
          (obs1'     , obs2'     ) -> BinOp And obs1' obs2'

      BinOp Or  obs1 obs2 ->
        case (simplify obs1, simplify obs2) of
          (Const True, _         ) -> Const True
          (_         , Const True) -> Const True
          (obs1'     , obs2'     ) -> BinOp Or obs1' obs2'

      BinOp op obs1 obs2 -> BinOp op (simplify obs1) (simplify obs2)
      UnOp  op obs1      -> UnOp op (simplify obs1)

      _ -> obs

evermoreTrue, evermoreFalse :: Time -> Obs Bool -> Bool

evermoreTrue time obs = case obs of
  Const     True      -> True
  Const     False     -> False
  NamedCond _         -> False
  At _                -> False
  After t             -> time >= t
  Before _            -> False
  UnOp  Not obs1      -> evermoreFalse time obs1
  BinOp And obs1 obs2 -> evermoreTrue time obs1 && evermoreTrue time obs2
  BinOp Or  obs1 obs2 -> evermoreTrue time obs1 || evermoreTrue time obs2

  -- these cannot depend on the time, so we can reuse isTrue
  BinOp Eq  _ _ -> isTrue time obs
  BinOp Gt  _ _ -> isTrue time obs
  BinOp Gte _ _ -> isTrue time obs
  BinOp Lt  _ _ -> isTrue time obs
  BinOp Lte _ _ -> isTrue time obs
  IfThen _  _ _ -> isTrue time obs

evermoreFalse time obs = case obs of
  Const     True      -> False
  Const     False     -> True
  NamedCond _         -> False
  At t                -> time > t
  After _             -> False
  Before t            -> time >= t
  UnOp  Not obs1      -> evermoreTrue time obs1
  BinOp And obs1 obs2 -> evermoreFalse time obs1 || evermoreFalse time obs2
  BinOp Or  obs1 obs2 -> evermoreFalse time obs1 && evermoreFalse time obs2

  -- these cannot depend on the time, so we can reuse isTrue
  BinOp Eq  _ _ -> isFalse time obs
  BinOp Gt  _ _ -> isFalse time obs
  BinOp Gte _ _ -> isFalse time obs
  BinOp Lt  _ _ -> isFalse time obs
  BinOp Lte _ _ -> isFalse time obs
  IfThen _  _ _ -> isFalse time obs

subst :: VarName -> Double -> Obs a -> Obs a
subst n v = subst'
  where
    subst' :: Obs a -> Obs a
    subst' (Var n') | n == n'   = Const v

    subst' (UnOp  op obs1)      = UnOp  op (subst' obs1)
    subst' (BinOp op obs1 obs2) = BinOp op (subst' obs1) (subst' obs2)
    subst' (IfThen obsc obs1 obs2) = IfThen  (subst' obsc) (subst' obs1) (subst' obs2)

    subst' o = o


{-
-- | Construct a decision procedure for this set of high level Obs
-- expressions (and their associated continuation info)
--
mkDecisionProcedure :: [(Obs Bool, a)] -> ObsDecisionProcedure a

-- | In the current state, if no new events arrive, does there exist a
-- time horizon where one one observable will become true anyway
-- (simply due to time passing)
--
timeHorizon :: ObsDecisionProcedure -> Maybe (Time, a)

-- | An event has arrived before the time horizon, update the decision
-- procedure state machine with this event. Either an observable will
-- become true, or we just get a new decision procedure state.
--
nextEvent :: Time
          -> ObsAtomEvent
          -> ObsDecisionProcedure a
          -> Either (Time, a)
                    (ObsDecisionProcedure a)
-}

-- * Eq instance helpers
-- GADT fun

data Rep a where
  Double   :: Rep Double
  Bool     :: Rep Bool

data TEq a b where
  Refl     :: TEq a a

class DEq a b where
  deq :: a -> b -> Maybe (TEq a b)

instance DEq (Rep a1) (Rep a2) where
  deq Double Double = Just Refl
  deq Bool   Bool   = Just Refl
  deq _      _      = Nothing

instance DEq (UnOp a1 b1) (UnOp a2 b2) where
  deq Not Not     = Just Refl
  deq Neg Neg     = Just Refl
  deq Abs Abs     = Just Refl
  deq Sqrt Sqrt   = Just Refl
  deq Exp Exp     = Just Refl
  deq Log Log     = Just Refl
  deq Cos Cos     = Just Refl
  deq Sin Sin     = Just Refl
  deq Asin Asin   = Just Refl
  deq Acos Acos   = Just Refl
  deq Atan Atan   = Just Refl
  deq Sinh Sinh   = Just Refl
  deq Cosh Cosh   = Just Refl
  deq Asinh Asinh = Just Refl
  deq Acosh Acosh = Just Refl
  deq Atanh Atanh = Just Refl
  deq _   _       = Nothing

instance DEq (BinOp a1 b1 c1) (BinOp a2 b2 c2) where
  deq And And = Just Refl
  deq Or  Or  = Just Refl
  deq Eq  Eq  = Just Refl
  deq Gt  Gt  = Just Refl
  deq Gte Gte = Just Refl
  deq Lt  Lt  = Just Refl
  deq Lte Lte = Just Refl
  deq Add Add = Just Refl
  deq Sub Sub = Just Refl
  deq Mul Mul = Just Refl
  deq Div Div = Just Refl
  deq Min Min = Just Refl
  deq Max Max = Just Refl
  deq _   _   = Nothing


-- | Display tree instances
instance Show (Obs a) where
  show (Const v)       = show v
  show (Var   n)       = n
  show (NamedVal  n)   = n
  show (NamedCond n)   = n
  show (At     t)      = "(time == " ++ show t ++ ")"
  show (After  t)      = "(time >= " ++ show t ++ ")"
  show (Before t)      = "(time < "  ++ show t ++ ")"
  show (UnOp  op obs1) = "(" ++ show op ++ " " ++ show obs1 ++ ")"

  show (BinOp op obs1 obs2)
    | isInfix op       = "(" ++ show obs1 ++ " " ++ show op
                           ++ " " ++ show obs2 ++ ")"
    | otherwise        = "(" ++ show op ++ " " ++ show obs1
                                        ++ " " ++ show obs2 ++ ")"
  show (IfThen obsc obs1 obs2) = "if " ++ show obsc
                           ++ "\nthen " ++ show obs1
                           ++ "\nelse " ++ show obs2

instance Show (UnOp a b) where
  show Not   = "not"
  show Neg   = "-"
  show Abs   = "abs"
  show Sqrt  = "sqrt"
  show Exp   = "exp"
  show Log   = "log"
  show Sin   = "sin"
  show Cos   = "cos"
  show Asin  = "asin"
  show Acos  = "acos"
  show Atan  = "atan"
  show Sinh  = "sinh"
  show Cosh  = "cosh"
  show Asinh = "asinh"
  show Atanh = "atanh"
  show Acosh = "acosh"

instance Show (BinOp a b c) where
  show Eq  = "=="
  show Gt  = ">"
  show Gte = ">="
  show Lt  = "<"
  show Lte = "<="
  show And = "&&"
  show Or  = "||"
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Min = "min"
  show Max = "max"


instance Show a => Display (Obs a) where
  toTree obs  = Node (show obs) []


-- * XML instances
-- | XML parser for real-valued observables
parseObsReal :: XMLParser (Obs Double)
parseObsReal = do
  e@(Elem t _ _) <- element ["Const","Var","NamedVal","IfThen"
                            ,"Neg","Abs","Sqrt","Exp","Log","Sin","Cos","Asin","Acos","Atan","Sinh","Cosh","Asinh","Acosh","Atanh","Add","Sub","Mul","Div","Min","Max"]

  case t of
    "Const"    -> interior e $ liftM Const readText
    "Var"      -> interior e $ liftM Var text
    "NamedVal" -> interior e $ liftM NamedVal text

    "Neg"   -> interior e $ liftM (UnOp Neg) parseObsReal
    "Abs"   -> interior e $ liftM (UnOp Abs) parseObsReal
    "Sqrt"  -> interior e $ liftM (UnOp Sqrt) parseObsReal
    "Exp"   -> interior e $ liftM (UnOp Exp) parseObsReal
    "Log"   -> interior e $ liftM (UnOp Log) parseObsReal
    "Sin"   -> interior e $ liftM (UnOp Sin) parseObsReal
    "Cos"   -> interior e $ liftM (UnOp Cos) parseObsReal
    "Asin"  -> interior e $ liftM (UnOp Asin) parseObsReal
    "Acos"  -> interior e $ liftM (UnOp Acos) parseObsReal
    "Atan"  -> interior e $ liftM (UnOp Atan) parseObsReal
    "Sinh"  -> interior e $ liftM (UnOp Sinh) parseObsReal
    "Cosh"  -> interior e $ liftM (UnOp Cosh) parseObsReal
    "Asinh" -> interior e $ liftM (UnOp Asinh) parseObsReal
    "Acosh" -> interior e $ liftM (UnOp Acosh) parseObsReal
    "Atanh" -> interior e $ liftM (UnOp Atanh) parseObsReal

    "Add" -> interior e $ liftM2 (BinOp Add) parseObsReal parseObsReal
    "Sub" -> interior e $ liftM2 (BinOp Sub) parseObsReal parseObsReal
    "Mul" -> interior e $ liftM2 (BinOp Mul) parseObsReal parseObsReal
    "Div" -> interior e $ liftM2 (BinOp Div) parseObsReal parseObsReal
    "Min" -> interior e $ liftM2 (BinOp Min) parseObsReal parseObsReal
    "Max" -> interior e $ liftM2 (BinOp Max) parseObsReal parseObsReal

    "IfThen" -> interior e $ liftM3 IfThen parseObsCond parseObsReal parseObsReal

-- | XML parser for condition observables
parseObsCond :: XMLParser (Obs Bool)
parseObsCond = do
  e@(Elem t _ _) <- element ["Const","NamedCond","At", "After","Before","IfThen"
                            ,"Not","And","Or","Eq","Gt","Gte","Lt","Lte"]

  case t of
    "Const"     -> interior e $ liftM Const readText
    "NamedCond" -> interior e $ liftM NamedCond text

    "At"     -> interior e $ liftM At     readText
    "After"  -> interior e $ liftM After  readText
    "Before" -> interior e $ liftM Before readText

    "Not" -> interior e $ liftM  (UnOp Not)  parseObsCond
    "And" -> interior e $ liftM2 (BinOp And) parseObsCond parseObsCond
    "Or"  -> interior e $ liftM2 (BinOp Or)  parseObsCond parseObsCond

    "Gt"  -> interior e $ liftM2 (BinOp Gt)  parseObsReal parseObsReal
    "Gte" -> interior e $ liftM2 (BinOp Gte) parseObsReal parseObsReal
    "Lt"  -> interior e $ liftM2 (BinOp Lt)  parseObsReal parseObsReal
    "Lte" -> interior e $ liftM2 (BinOp Lte) parseObsReal parseObsReal

    "IfThen" -> interior e $ liftM3 IfThen parseObsCond parseObsCond parseObsCond

-- | Create XML tags
printObs :: Obs a -> Content ()
printObs (Const v)     = mkElemC "Const"     (toText (show v))
printObs (Var n)       = mkElemC "Var"       (toText n)
printObs (NamedVal  n) = mkElemC "NamedVal"  (toText n)
printObs (NamedCond n) = mkElemC "NamedCond" (toText n)

printObs (At     t) = mkElemC "At"     (toText (show t))
printObs (After  t) = mkElemC "After"  (toText (show t))
printObs (Before t) = mkElemC "Before" (toText (show t))

printObs (IfThen oc o1 o2) = mkElemC "IfThen" [printObs oc, printObs o1, printObs o2]

printObs (UnOp Not o1)   = mkElemC "Not" [printObs o1]
printObs (UnOp Neg o1)   = mkElemC "Neg" [printObs o1]
printObs (UnOp Abs o1)   = mkElemC "Abs" [printObs o1]
printObs (UnOp Sqrt o1)  = mkElemC "Sqrt" [printObs o1]
printObs (UnOp Exp o1)   = mkElemC "Exp" [printObs o1]
printObs (UnOp Log o1)   = mkElemC "Log" [printObs o1]
printObs (UnOp Sin o1)   = mkElemC "Sin" [printObs o1]
printObs (UnOp Cos o1)   = mkElemC "Cos" [printObs o1]
printObs (UnOp Asin o1)  = mkElemC "Asin" [printObs o1]
printObs (UnOp Atan o1)  = mkElemC "Atan" [printObs o1]
printObs (UnOp Acos o1)  = mkElemC "Acos" [printObs o1]
printObs (UnOp Sinh o1)  = mkElemC "Asinh" [printObs o1]
printObs (UnOp Cosh o1)  = mkElemC "Cosh" [printObs o1]
printObs (UnOp Asinh o1) = mkElemC "Asinh" [printObs o1]
printObs (UnOp Acosh o1) = mkElemC "Acosh" [printObs o1]
printObs (UnOp Atanh o1) = mkElemC "Atanh" [printObs o1]

printObs (BinOp And o1 o2) = mkElemC "And" [printObs o1, printObs o2]
printObs (BinOp Or  o1 o2) = mkElemC "Or"  [printObs o1, printObs o2]
printObs (BinOp Eq  o1 o2) = mkElemC "Eq"  [printObs o1, printObs o2]
printObs (BinOp Gt  o1 o2) = mkElemC "Gt"  [printObs o1, printObs o2]
printObs (BinOp Gte o1 o2) = mkElemC "Gte" [printObs o1, printObs o2]
printObs (BinOp Lt  o1 o2) = mkElemC "Lt"  [printObs o1, printObs o2]
printObs (BinOp Lte o1 o2) = mkElemC "Lte" [printObs o1, printObs o2]
printObs (BinOp Add o1 o2) = mkElemC "Add" [printObs o1, printObs o2]
printObs (BinOp Sub o1 o2) = mkElemC "Sub" [printObs o1, printObs o2]
printObs (BinOp Mul o1 o2) = mkElemC "Mul" [printObs o1, printObs o2]
printObs (BinOp Div o1 o2) = mkElemC "Div" [printObs o1, printObs o2]
printObs (BinOp Min o1 o2) = mkElemC "Min" [printObs o1, printObs o2]
printObs (BinOp Max o1 o2) = mkElemC "Max" [printObs o1, printObs o2]
