-- |Netrium is Copyright Netrium Consulting Ltd, 2009-2012, and files herein are licensed
-- |under the Affero General Public License version 3, the text of which can
-- |be found in agpl.txt, or any later version of the AGPL, unless otherwise
-- |noted. 
--
module XmlUtils where

import Text.XML.HaXml.XmlContent

attrStr n (Elem _ as _) =
    case lookup n as of
      Nothing -> fail ("expected attribute " ++ n)
      Just av -> return (attr2str av)

attrRead n e = do
    str <- attrStr n e
    case reads str of
      [(v,_)] -> return v
      _       -> fail $ "cannot parse attribute " ++ n ++ ": " ++ str

mkElemAC x as cs = CElem (Elem x as cs) ()

readText :: Read a => XMLParser a
readText = do
  t <- text
  case reads t of
    [(v,_)] -> return v
    _       -> fail $ "cannot parse " ++ t

