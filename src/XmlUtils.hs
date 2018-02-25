-- |Netrium is Copyright Anthony Waite, Dave Hetwett, Shaun Laurens 2009-2015, and files herein are licensed
-- |under the MIT license,  the text of which can be found in license.txt
--
{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module XmlUtils where

import Text.XML.HaXml.Namespaces (localName)
import Text.XML.HaXml.Types (QName(..))
import Text.XML.HaXml.XmlContent
import Data.Time

attrStr :: Monad m => QName -> Element t -> m String
attrStr n (Elem _ as _) =
    case lookup n as of
      Nothing -> fail ("expected attribute " ++ localName n)
      Just av -> return (attr2str av)

attrRead :: (Read b, Monad m) => QName -> Element t -> m b
attrRead n e = do
    str <- attrStr n e
    case reads str of
      [(v,_)] -> return v
      _       -> fail $ "cannot parse attribute " ++ localName n ++ ": " ++ str

mkElemAC :: QName -> [Attribute] -> [Content ()] -> Content ()
mkElemAC x as cs = CElem (Elem x as cs) ()

readText :: Read a => XMLParser a
readText = do
  t <- text
  case reads t of
    [(v,_)] -> return v
    _       -> fail $ "cannot parse " ++ t


instance XmlContent Bool where
  parseContents = do
    e@(Elem t _ _) <- element ["True", "False"]
    commit $ interior e $ case localName t of
      "True"  -> return True
      "False" -> return False
      x       -> fail $ "cannot parse " ++ x

  toContents True  = [mkElemC "True"  []]
  toContents False = [mkElemC "False" []]

instance XmlContent Double where
  parseContents = inElement "Double" readText
  toContents t  = [mkElemC "Double" (toText (show t))]

instance HTypeable UTCTime where
  toHType _ = Defined "Time" [] []

instance XmlContent UTCTime where
  parseContents = inElement "Time" readText
  toContents t  = [mkElemC "Time" (toText (show t))]
