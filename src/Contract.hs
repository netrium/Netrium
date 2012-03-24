-- |Netrium is Copyright Netrium Consulting Ltd, 2009-2012, and files herein are licensed
-- |under the Affero General Public License version 3, the text of which can
-- |be found in agpl.txt, or any later version of the AGPL, unless otherwise
-- |noted. 
--
-- Module for contract
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Contract (
    module Contract,

    Time,

    Obs,
    konst, var, primVar, primCond, at, before, after, between,
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
import Control.Monad
import Text.XML.HaXml.XmlContent

-- *Contract type definition
-- |A canonical tradeable element, physical or financial
data Tradeable = Physical  Commodity Unit Location (Maybe Duration)
               | Financial Currency CashFlowType
  deriving (Eq, Show)

-- |A duration is a span of time, measured in seconds.
newtype Duration  = Duration  Int {- in sec -} deriving (Eq, Show, Num)
-- |Commodity, e.g. Gas, Electricity
newtype Commodity = Commodity String           deriving (Eq, Show)
-- |Unit, e.g. tonnes, MWh
newtype Unit      = Unit      String           deriving (Eq, Show)
-- |Location, e.g. UK, EU
newtype Location  = Location  String           deriving (Eq, Show)
-- |Currency, e.g. EUR, USD, GBP
newtype Currency  = Currency  String           deriving (Eq, Show)
-- |Cashflow type, e.g. cash, premium
newtype CashFlowType = CashFlowType String     deriving (Eq, Show)

-- |Scaling factor (used to scale the 'One' contract)
type ScaleFactor  = Double

-- |Choice label, used for options
type ChoiceId = String

-- |Contract
data Contract
   = Zero                                         -- ^Deliver nothing now
   | One  Tradeable                               -- ^Deliver one tradeable element now

   | Give Contract                                -- ^Give a contract
   | And  Contract Contract                       -- ^Append 2 contracts

   | Or      ChoiceId     Contract Contract       -- ^Offer a choice between 2 contracts
   | Cond    (Obs Bool)   Contract Contract       -- ^The contract returned depend on a boolean condition

   | Scale    (Obs Double) Contract               -- ^Scale a contract using a scaling factor
   | Read Var (Obs Double) Contract               -- ^Read a variable into a contract

   | When             (Obs Bool)   Contract       -- ^Return a contract only at a given time
   | Anytime ChoiceId (Obs Bool)   Contract       -- ^Return a contract anytime a condition is met
   | Until            (Obs Bool)   Contract       -- ^Return a contract until a condition is met
  deriving (Eq, Show)

-- |A variable
type Var = String

zero = Zero
one  = One
give = Give
and  = And
or   = Or
cond = Cond
scale   = Scale
read    = Read
when    = When
anytime = Anytime
until   = Until


-- Display tree instances
instance Display Contract where
  toTree Zero           = Node "zero"         []
  toTree (One  t)       = Node "one"          [Node (show t) []]
  toTree (Give c)       = Node "give"         [toTree c]
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
      "Physical"  -> liftM4 Physical   parseContents parseContents
                                       parseContents parseContents
      "Financial" -> liftM2  Financial parseContents parseContents

  toContents (Physical c u l d) = [mkElemC "Physical"  (toContents c
                                                     ++ toContents u
                                                     ++ toContents l
                                                     ++ toContents d)]
  toContents (Financial c t)    = [mkElemC "Financial" (toContents c
                                                     ++ toContents t)]

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

instance HTypeable Contract where
  toHType _ = Defined "Contract" [] []

instance XmlContent Contract where
  parseContents = do
    e@(Elem t _ _) <- element ["Zero","When","Until","Scale","Read"
                              ,"Or","One","Give","Cond","Anytime","And"]
    commit $ interior e $ case t of
      "Zero"    -> return Zero
      "One"     -> liftM  One     parseContents
      "Give"    -> liftM  Give    parseContents
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
