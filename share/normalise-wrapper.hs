-- |Netrium is Copyright Netrium Consulting Ltd, 2009-2012, and files herein are licensed
-- |under the Affero General Public License version 3, the text of which can
-- |be found in agpl.txt, or any later version of the AGPL, unless otherwise
-- |noted. 
module Main where

import Prelude hiding (and, or, min, max, abs, negate, not, read, until)
import Contract
import Common

import qualified Text.XML.HaXml.XmlContent as XML
import qualified Text.XML.HaXml.Pretty     as XML.PP
import qualified Text.PrettyPrint          as PP
