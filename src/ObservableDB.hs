-- |Netrium is Copyright Netrium Consulting Ltd, 2009-2012, and files herein are licensed
-- |under the Affero General Public License version 3, the text of which can
-- |be found in agpl.txt, or any later version of the AGPL, unless otherwise
-- |noted. 
module ObservableDB where

import Control.Monad              (liftM, liftM2)
import Text.XML.HaXml.XmlContent

import XmlUtils

newtype ObservableDB = ObservableDB { unObservableDB :: [ObservableDecl] }
  deriving (Show, Read)
data ObservableDecl = ObservableDecl String ObservableType
  deriving (Show, Read)
data ObservableType = Double | Bool
  deriving (Show, Read)

instance HTypeable ObservableDB where
  toHType _ = Defined "ObservableDB" [] []

instance XmlContent ObservableDB where
  parseContents = inElement "ObservableDB" $
                    liftM ObservableDB parseContents

  toContents (ObservableDB ds) =
    [mkElemC "ObservableDB" (toContents ds)]

instance HTypeable ObservableDecl where
  toHType _ = Defined "ObservableDecl" [] []

instance XmlContent ObservableDecl where
  parseContents = do
    e@(Elem t _ _) <- element ["ObservableDecl"]
    commit $ interior e $ case t of
      "ObservableDecl" -> liftM2 ObservableDecl (attrStr "name" e) parseContents

  toContents (ObservableDecl n t) =
    [mkElemAC "ObservableDecl" [("name", str2attr n)] (toContents t)]

instance HTypeable ObservableType where
  toHType _ = Defined "ObservableType" [] []

instance XmlContent ObservableType where
  parseContents = do
    e@(Elem t _ _) <- element ["Double", "Bool"]
    commit $ interior e $ case t of
      "Double" -> return Double
      "Bool"   -> return Bool

  toContents Double = [mkElemC "Double" []]
  toContents Bool   = [mkElemC "Bool"   []]

compileObservableDB :: ObservableDB -> String
compileObservableDB = unlines . map compileObservable . unObservableDB
  where
    compileObservable (ObservableDecl n t) =
      n ++ " :: Obs " ++ ct ++ "\n" ++
      n ++ " = " ++ ce ++ " " ++ show n
      where
        ct = case t of
               Double -> "Double"
               Bool   -> "Bool"
        ce = case t of
               Double -> "primVar"
               Bool   -> "primCond"
