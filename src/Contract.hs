-- |Netrium is Copyright Anthony Waite, Dave Hetwett, Shaun Laurens 2009-2015, and files herein are licensed
-- |under the MIT license,  the text of which can be found in license.txt
--
-- The definition of the basic contract language
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Contract (
    -- * Contracts
    -- ** The contract type and primitives
    Contract(..),
    zero, one,
    and, give, party,
    or, cond,
    scale, ScaleFactor,
    when, anytime, until,
    read, letin,

    -- ** Tradable items
    Tradeable(..),
    Commodity(..), Unit(..), Location(..), Duration(..),
    Currency(..), CashFlowType(..), Portfolio(..),

    -- ** Choice identifiers
    ChoiceId, PartyName,

    -- * Observables
    Obs,
    konst, var, primVar, primCond,
    Time,
    at, before, after, between,
    ifthen, negate, max, min, abs,
    (%==),
    (%>), (%>=), (%<), (%<=),
    (%&&), (%||),
    (%+), (%-), (%*), (%/),
  ) where

import Observable
         ( Time, mkdate
         , Obs, konst, var, primVar, primCond, at, before, after, between
         , (%==), (%>), (%>=), (%<), (%<=)
         , (%&&), (%||), (%+), (%-), (%*), (%/)
         , ifthen, negate, not, max, min, abs
         , parseObsCond, parseObsReal, printObs )
import Display
import XmlUtils

import Prelude hiding (product, read, until, and, or, min, max, abs, not, negate)
import Control.Monad hiding (when)
import Text.XML.HaXml.XmlContent

-- * Contract type definition
-- | A canonical tradeable element, physical or financial
data Tradeable = Physical  Commodity Unit Location (Maybe Duration) (Maybe Portfolio)
               | Financial Currency CashFlowType (Maybe Portfolio)
  deriving (Eq, Show)

-- | A duration is a span of time, measured in seconds.
--
newtype Duration  = Duration  Int {- in sec -} deriving (Eq, Show, Num)
-- | Commodity, e.g. Gas, Electricity
newtype Commodity = Commodity String           deriving (Eq, Show)
-- | Unit, e.g. tonnes, MWh
newtype Unit      = Unit      String           deriving (Eq, Show)
-- | Location, e.g. UK, EU
newtype Location  = Location  String           deriving (Eq, Show)
-- | Currency, e.g. EUR, USD, GBP
newtype Currency  = Currency  String           deriving (Eq, Show)
-- | Cashflow type, e.g. cash, premium
newtype CashFlowType = CashFlowType String     deriving (Eq, Show)
-- | Portfolio name
newtype Portfolio = Portfolio String           deriving (Eq, Show)

-- | Scaling factor (used to scale the 'One' contract)
type ScaleFactor  = Double

-- | Choice label, used for options
type ChoiceId = String

-- | Name of a third party mentioned in a contract
type PartyName = String

-- | The main contract data type
--
data Contract
   = Zero
   | One  Tradeable

   | Give Contract
   | Party PartyName Contract
   | And  Contract Contract

   | Or      ChoiceId     Contract Contract
   | Cond    (Obs Bool)   Contract Contract

   | Scale    (Obs Double) Contract
   | Read Var (Obs Double) Contract

   | When             (Obs Bool)   Contract
   | Anytime ChoiceId (Obs Bool)   Contract
   | Until            (Obs Bool)   Contract
  deriving (Eq, Show)

-- | A variable
type Var = String

-- | The @zero@ contract has no rights and no obligations.
zero :: Contract
zero = Zero

-- | If you acquire @one t@ you immediately recieve one unit of the
-- 'Tradeable' @t@.
one :: Tradeable -> Contract
one = One

-- | Swap the rights and obligations of the party and counterparty.
give :: Contract -> Contract
give = Give

-- | Make a contract with a named 3rd party as the counterparty.
party :: PartyName -> Contract -> Contract
party = Party

-- | If you acquire @c1 `and` c2@ you immediately acquire /both/ @c1@ and @c2@.
and :: Contract -> Contract -> Contract
and = And

-- | If you acquire @c1 `or` c2@ you immediately acquire your choice of
-- /either/ @c1@ or @c2@.
or :: ChoiceId -> Contract -> Contract -> Contract
or = Or
--TODO: document the ChoiceId

-- | If you acquire @cond obs c1 c2@ then you acquire @c1@ if the observable
-- @obs@ is true /at the moment of acquistion/, and @c2@ otherwise.
cond :: Obs Bool -> Contract -> Contract -> Contract
cond = Cond

-- | If you acquire @scale obs c@, then you acquire @c@ at the same moment
-- except that all the subsequent trades of @c@ are multiplied by the value
-- of the observable @obs@ /at the moment of acquistion/.
scale :: Obs ScaleFactor -> Contract -> Contract
scale = Scale

read :: Var -> Obs Double -> Contract -> Contract
read = Read
{-# DEPRECATED read "Use 'letin' instead." #-}

-- | If you acquire @when obs c@, you must acquire @c@ as soon as observable
-- @obs@ subsequently becomes true.
when :: Obs Bool -> Contract -> Contract
when = When

-- | Once you acquire @anytime obs c@, you /may/ acquire @c@ at any time the
-- observable @obs@ is true.
anytime :: ChoiceId -> Obs Bool -> Contract -> Contract
anytime = Anytime

-- | Once acquired, @until obs c@ is exactly like @c@ except that it /must be
-- abandoned/ when observable @obs@ becomes true.
until :: Obs Bool -> Contract -> Contract
until = Until


-- | Observe the value of an observable now and save its value to use later.
--
-- Currently this requires a unique variable name.
--
-- Example:
--
-- > letin "count" (count-1) $ \count' ->
-- >   ...
--
letin :: String                    -- ^ A unique variable name
      -> Obs Double                -- ^ The observable to observe now
      -> (Obs Double -> Contract)  -- ^ The contract using the observed value
      -> Contract
letin vname obs c = read vname obs (c (var vname))


-- Display tree instances
instance Display Contract where
  toTree Zero           = Node "zero"         []
  toTree (One  t)       = Node "one"          [Node (show t) []]
  toTree (Give c)       = Node "give"         [toTree c]
  toTree (Party p c)    = Node ("party " ++ p)[toTree c]
  toTree (And c1 c2)    = Node "and"          [toTree c1, toTree c2]
  toTree (Or cid c1 c2) = Node ("or " ++ cid) [toTree c1, toTree c2]
  toTree (Cond o c1 c2) = Node "cond"         [toTree o, toTree c1, toTree c2]
  toTree (Scale       o c)  = Node "scale"            [toTree o, toTree c]
  toTree (Read      n o c)  = Node ("read " ++  n)    [toTree o, toTree c]
  toTree (When        o c)  = Node "when"             [toTree o, toTree c]
  toTree (Anytime cid o c)  = Node ("anytime" ++ cid) [toTree o, toTree c]
  toTree (Until       o c)  = Node "until"            [toTree o, toTree c]

-- XML instances
instance HTypeable Tradeable where
    toHType _ = Defined "Tradeable" [] []

instance XmlContent Tradeable where
  parseContents = do
    e@(Elem t _ _) <- element ["Physical","Financial"]
    commit $ interior e $ case t of
      "Physical"  -> liftM5 Physical  parseContents parseContents
                                      parseContents parseContents
                                      parseContents
      "Financial" -> liftM3 Financial parseContents parseContents
                                      parseContents

  toContents (Physical c u l d p) =
    [mkElemC "Physical"  (toContents c ++ toContents u
                       ++ toContents l ++ toContents d
                       ++ toContents p)]
  toContents (Financial c t p) =
    [mkElemC "Financial" (toContents c ++ toContents t
                       ++ toContents p)]

instance HTypeable Duration where
    toHType _ = Defined "Duration" [] []

instance XmlContent Duration where
  parseContents = inElement "Duration" (liftM Duration readText)
  toContents (Duration sec) = [mkElemC "Duration" (toText (show sec))]

instance HTypeable Commodity where
    toHType _ = Defined "Commodity" [] []

instance XmlContent Commodity where
  parseContents = inElement "Commodity" (liftM Commodity text)
  toContents (Commodity name) = [mkElemC "Commodity" (toText name)]

instance HTypeable Unit where
    toHType _ = Defined "Unit" [] []

instance XmlContent Unit where
  parseContents = inElement "Unit" (liftM Unit text)
  toContents (Unit name) = [mkElemC "Unit" (toText name)]

instance HTypeable Location where
    toHType _ = Defined "Location" [] []

instance XmlContent Location where
  parseContents = inElement "Location" (liftM Location text)
  toContents (Location name) = [mkElemC "Location" (toText name)]

instance HTypeable Currency where
    toHType _ = Defined "Currency" [] []

instance XmlContent Currency where
  parseContents = inElement "Currency" (liftM Currency text)
  toContents (Currency name) = [mkElemC "Currency" (toText name)]

instance HTypeable CashFlowType where
    toHType _ = Defined "CashFlowType" [] []

instance XmlContent CashFlowType where
  parseContents = inElement "CashFlowType" (liftM CashFlowType text)
  toContents (CashFlowType name) = [mkElemC "CashFlowType" (toText name)]

instance HTypeable Portfolio where
    toHType _ = Defined "Portfolio" [] []

instance XmlContent Portfolio where
  parseContents = inElement "Portfolio" (liftM Portfolio text)
  toContents (Portfolio name) = [mkElemC "Portfolio" (toText name)]

instance HTypeable Contract where
  toHType _ = Defined "Contract" [] []

instance XmlContent Contract where
  parseContents = do
    e@(Elem t _ _) <- element ["Zero","When","Until","Scale","Read"
                              ,"Or","One","Give","Party","Cond","Anytime","And"]
    commit $ interior e $ case t of
      "Zero"    -> return Zero
      "One"     -> liftM  One     parseContents
      "Give"    -> liftM  Give    parseContents
      "Party"   -> liftM2 Party   (attrStr "name" e) parseContents
      "And"     -> liftM2 And     parseContents parseContents
      "Or"      -> liftM3 Or      (attrStr "choiceid" e) parseContents parseContents
      "Cond"    -> liftM3 Cond    parseObsCond  parseContents parseContents
      "Scale"   -> liftM2 Scale   parseObsReal  parseContents
      "Read"    -> liftM3 Read    (attrStr "var" e) parseObsReal  parseContents
      "When"    -> liftM2 When    parseObsCond  parseContents
      "Anytime" -> liftM3 Anytime (attrStr "choiceid" e) parseObsCond  parseContents
      "Until"   -> liftM2 Until   parseObsCond  parseContents

  toContents Zero           = [mkElemC  "Zero" []]
  toContents (One t)        = [mkElemC  "One"  (toContents t)]
  toContents (Give c)       = [mkElemC  "Give" (toContents c)]
  toContents (Party p c)    = [mkElemAC "Party" [("name", str2attr p)]
                                                (toContents c)]
  toContents (And    c1 c2) = [mkElemC  "And"  (toContents c1 ++ toContents c2)]
  toContents (Or cid c1 c2) = [mkElemAC "Or"   [("choiceid", str2attr cid)]
                                               (toContents c1 ++ toContents c2)]
  toContents (Cond o c1 c2) = [mkElemC  "Cond" (printObs o : toContents c1 ++ toContents c2)]
  toContents (Scale  o c)      = [mkElemC  "Scale"   (printObs o : toContents c)]
  toContents (Read n o c)      = [mkElemAC "Read"    [("var", str2attr n)]
                                                     (printObs o : toContents c)]
  toContents (When   o c)      = [mkElemC  "When"    (printObs o : toContents c)]
  toContents (Anytime cid o c) = [mkElemAC "Anytime" [("choiceid", str2attr cid)]
                                                     (printObs o : toContents c)]
  toContents (Until  o c)      = [mkElemC  "Until"   (printObs o : toContents c)]
