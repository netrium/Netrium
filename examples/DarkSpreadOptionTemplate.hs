-- |Netrium is Copyright Anthony Waite, Dave Hewett, Shaun Laurens & Contributors 2009-2018, and files herein are licensed
-- |under the MIT license,  the text of which can be found in license.txt
module Main where

import Prelude hiding (and, or, min, max, abs, negate, not, read, until)

import Contract
import Common
import Options
import Calendar

import qualified Text.XML.HaXml.XmlContent as XML
import qualified Text.XML.HaXml.Pretty     as XML.PP
import qualified Text.PrettyPrint          as PP


-------------------------------------------------------------------------------
-- Template program main
--

main = do
   
   -- read the contract template parameters from the program stdin
   -- one paramater per line, in Haskell 'Read' syntax
   -- For this example the following input file would do:
   --
   -- 7
   -- 3

   -- the contract xml is written on program stdout
   -- invoke as so:
   -- ./DarkSpreadOptionTemplate < templateInputs.txt  > contract.xml

   powerPrice <- readLn
   premium    <- readLn
   
   let contract = contractTemplate (konst powerPrice) (konst premium)
   
   putStr (renderXml contract)

renderXml = PP.renderStyle PP.style { PP.mode = PP.OneLineMode }
          . XML.PP.document
          . XML.toXml False

-------------------------------------------------------------------------------
-- Contrac template function itself
--

-- Daily exercise at 15:00 on the day preceding the supply day
-- (this is supposed to be on every fourth EEX business day but for now it does every fourth calendar day - to be updated when we have a proper calendar)

contractTemplate powerPrice premium
  = commoditySpreadOption "choice" legs
             (workingDaysEarlier calendar 4 <> atTimeOfDay 15 00)
             (workingDaysLater   calendar 5)
             CallOption
             strikePrice (Currency "GBP") (CashFlowType "initialMargin")
             premium (Currency "GBP")
  where strikePrice = powerPrice * powerVol
        calendar = getCalendar "EEX Power"

legs = [coalLeg, carbonLeg, electricityLeg]

-- API2 coal: monthly delivery (assumption is 1st of the month)
coalLeg =
    ( Market (Commodity "Coal") (Unit "MWh") (Location "ARA"), coalVol
    , coalPrice, (Currency "USD"), (CashFlowType "initialMargin")
    , [ [datetime 2011 m 1 0 0 ] | m <- [1..12] ]
    , exchangeFee  50
    )

-- Carbon: yearly certificates split into monthly deliveries at 12:00
carbonLeg =
    ( Market (Commodity "Carbon") (Unit "t") (Location "EU"), carbonVol
    , carbonPrice, (Currency "GBP"), (CashFlowType "initialMargin")
    , [ [datetime 2011 m 1 12 0 ] | m <- [1..12] ]
    , exchangeFee  75)

-- CE Power: delivery every 15 minutes
electricityLeg =
    ( Market (Commodity "Electricity") (Unit "MWh") (Location "Amprion HVG"), powerVol
    , powerPrice, (Currency "EUR"), (CashFlowType "initialMargin")
    , [ [datetime 2011 m d h i | d <- [1..31], h <- [0..23], i <- [0,15,30,45] ] | m <- [1] ]
    , exchangeFee 100 )


-------------------------------------------------------------------------------
-- Generated 
--

-- This part would be generated automatically by a variation of the normalise
-- program. For ordinary (non-template) contracts the normalise program
-- generate these automatically. We will eventually want something similar for
-- templates.

coalVol :: Obs Double
coalVol = primVar "coalVol"
coalPrice :: Obs Double
coalPrice = primVar "coalPrice"
carbonVol :: Obs Double
carbonVol = primVar "carbonVol"
carbonPrice :: Obs Double
carbonPrice = primVar "carbonPrice"
powerVol :: Obs Double
powerVol = primVar "powerVol"
powerPrice :: Obs Double
powerPrice = primVar "powerPrice"
