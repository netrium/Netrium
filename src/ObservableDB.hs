-- |Netrium is Copyright Anthony Waite, Dave Hetwett, Shaun Laurens 2009-2015, and files herein are licensed
-- |under the MIT license,  the text of which can be found in license.txt
--
module ObservableDB where

import Control.Monad              (liftM, liftM2)
import Text.XML.HaXml.Namespaces  (localName)
import Text.XML.HaXml.Types       (QName(..))
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
    commit $ interior e $ case localName t of
      "ObservableDecl" -> liftM2 ObservableDecl (attrStr (N "name") e) parseContents
      x -> fail $ "cannot parse " ++ x

  toContents (ObservableDecl n t) =
    [mkElemAC (N "ObservableDecl") [(N "name", str2attr n)] (toContents t)]

instance HTypeable ObservableType where
  toHType _ = Defined "ObservableType" [] []

instance XmlContent ObservableType where
  parseContents = do
    e@(Elem t _ _) <- element ["Double", "Bool"]
    commit $ interior e $ case localName t of
      "Double" -> return Double
      "Bool"   -> return Bool
      x        -> fail $ "cannot parse " ++ x

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
