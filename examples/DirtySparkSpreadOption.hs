module Main where

import Prelude hiding (and, or, min, max, abs, negate, not, read, until)

import DemoContractAST
import Contract (Obs, primVar)
import Common (atTimeOfDay, date, datetime, exchangeFee, (<>))
-- import Options (OptionDirection(CallOption))
import Calendar

import qualified Text.XML.HaXml.XmlContent as XML
import qualified Text.XML.HaXml.Pretty     as XML.PP
import qualified Text.PrettyPrint          as PP

-------------------------------------------------------------------------------
-- Contract template function itself
--

-- Daily exercise at 15:00 on the day preceding the supply day
-- (this is supposed to be on every fourth EEX business day but for now it does every fourth calendar day - to be updated when we have a proper calendar)

contract
  = namedContract "Commodity Spread Option" $ commoditySpreadOption legs
             (calendarDaysEarlier calendar 4 <> atTimeOfDay 07 00)
             (calendarDaysEarlier calendar 4 <> atTimeOfDay 15 00)
             CallOption
             strike
             (CashFlowType "initialMargin")
             premium
  where calendar = getBusinessDayCalendar "EEX Power"
        strike = (strikePrice, Currency "GBP")
        strikePrice = powerPrice * powerVol
        premium = (2, Currency "GBP")

coalVol :: Double
coalVol = 5000
coalPrice :: Double
coalPrice = 12
carbonVol :: Double
carbonVol = 100
carbonPrice :: Double
carbonPrice = 70
powerVol :: Double
powerVol = 50
powerPrice :: Double
powerPrice = 16

legs = [coalLeg, carbonLeg, electricityLeg]

-- API2 coal: monthly delivery (assumption is 1st of the month)
coalLeg =
    ( Market (Commodity "Coal") (Unit "MWh") (Location "ARA"), coalVol
    , coalPrice, (Currency "USD"), (CashFlowType "initialMargin")
    , [ schedule ]
    , exchangeFee  50
    )
 where
  schedule = deliverySchedule (date 2011 1 1) (date 2011 12 31)
             calendar deliverAtMidnight
  calendar = newCalendar "carbonCalendar" [(date 2011 m 1, BusinessDay) | m <- [1..12]]

-- Carbon: yearly certificates split into monthly deliveries at 12:00
carbonLeg =
    ( Market (Commodity "Carbon") (Unit "t") (Location "EU"), carbonVol
    , carbonPrice, (Currency "EUR"), (CashFlowType "initialMargin")
    , [ schedule ]
    , exchangeFee  75)
 where
  schedule = deliverySchedule (date 2011 1 1) (date 2011 12 31) 
             calendar deliverAtMidnight
  calendar = newCalendar "carbonCalendar" [(date 2011 m 1, BusinessDay) | m <- [1..12]]

-- CE Power: delivery every 15 minutes
electricityLeg =
    ( Market (Commodity "Electricity") (Unit "MWh") (Location "Amprion HVG"), powerVol
    , powerPrice, (Currency "GBP"), (CashFlowType "initialMargin")
    , [ schedule ]
    , exchangeFee 100 )
 where
  calendar = getBusinessDayCalendar "EEX Power"
  schedule = deliverySchedule (date 2011 1 1) (date 2011 12 31) 
             calendar shape
  shape = complexDeliveryShape [deliverAtTimeOfDay h m | h <- [0..23], m <- [0,15,30,45]]


-------------------------------------------------------------------------------
-- Template program main
--

main = do
  putStr (renderXml contract)
 where
  -- renderXml = show
  renderXml = PP.renderStyle PP.style { PP.mode = PP.OneLineMode }
            . XML.PP.document
            . toXml False
