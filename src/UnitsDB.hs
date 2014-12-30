-- |Netrium is Copyright Anthony Waite, Dave Hetwett, Shaun Laurens 2009-2015, and files herein are licensed
-- |under the MIT license,  the text of which can be found in license.txt
--
module UnitsDB where

import Control.Monad              (liftM, liftM2)
import Text.XML.HaXml.XmlContent

import XmlUtils

newtype UnitsDB = UnitsDB { unUnitsDB :: [UnitDecl] }
  deriving (Show, Read)

data UnitDecl = CommodityDecl    String
              | UnitDecl         String
              | LocationDecl     String
              | CurrencyDecl     String
              | CashFlowTypeDecl String
  deriving (Show, Read)


instance HTypeable UnitsDB where
  toHType _ = Defined "UnitsDB" [] []

instance XmlContent UnitsDB where
  parseContents = inElement "UnitsDB" (liftM UnitsDB parseContents)
  toContents (UnitsDB ds) = [mkElemC "UnitsDB" (toContents ds)]


instance HTypeable UnitDecl where
  toHType _ = Defined "UnitDecl" [] []

instance XmlContent UnitDecl where
  parseContents = do
    e@(Elem t _ _) <- element ["CommodityDecl", "CashFlowTypeDecl",
                               "UnitDecl",
                               "LocationDecl", "CurrencyDecl"]
    commit $ interior e $ case t of
      "CashFlowTypeDecl"  -> liftM CashFlowTypeDecl  (attrStr "name" e)
      "CommodityDecl"     -> liftM CommodityDecl     (attrStr "name" e)
      "UnitDecl"          -> liftM UnitDecl          (attrStr "name" e)
      "LocationDecl"      -> liftM LocationDecl      (attrStr "name" e)
      "CurrencyDecl"      -> liftM CurrencyDecl      (attrStr "name" e)

  toContents (CommodityDecl n ) =
    [mkElemAC "CommodityDecl" [("name", str2attr n)] []]
  toContents (CashFlowTypeDecl n ) =
    [mkElemAC "CashFlowTypeDecl" [("name", str2attr n)] []]
  toContents (UnitDecl n ) =
    [mkElemAC "UnitDecl" [("name", str2attr n)] []]
  toContents (LocationDecl n ) =
    [mkElemAC "LocationDecl" [("name", str2attr n)] []]
  toContents (CurrencyDecl n ) =
    [mkElemAC "CurrencyDecl" [("name", str2attr n)] []]


compileUnitsDB :: UnitsDB -> String
compileUnitsDB = unlines . map compileUnit . unUnitsDB
  where
    compileUnit (CashFlowTypeDecl n) =
      n ++ " :: CashFlowType\n" ++
      n ++ " = CashFlowType " ++ show n
    compileUnit (CommodityDecl n) =
      n ++ " :: Commodity\n" ++
      n ++ " = Commodity " ++ show n
    compileUnit (UnitDecl n) =
      n ++ " :: Unit\n" ++
      n ++ " = Unit " ++ show n
    compileUnit (LocationDecl n) =
      n ++ " :: Location\n" ++
      n ++ " = Location " ++ show n
    compileUnit (CurrencyDecl n) =
      n ++ " :: Currency\n" ++
      n ++ " = Currency " ++ show n
