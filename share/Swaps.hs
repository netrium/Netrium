-- |Netrium is Copyright Anthony Waite, Dave Hewett, Shaun Laurens & Contributors 2009-2018, and files herein are licensed
-- |under the MIT license,  the text of which can be found in license.txt
--
-- Module for swap contracts
module Swaps where

import Prelude hiding (and, or, min, max, abs, negate, not, read, until)
import Contract
import Common
import Calendar
import Credit

import Data.List (transpose)
import Data.Time
import Data.Monoid

-- | Basic commodity swap: simultaneous swap of commodity A for commodity B for
-- a nominal cost over a delivery schedule (same cost for each delivery period)
commoditySwap :: (Market, Market)
              -> (Volume, Volume)
              -> Price -> Currency -> CashFlowType
              -> Schedule
              -> Contract
commoditySwap (market1, market2) (vol1, vol2) pr cur cft sch =
  scheduled (physical vol1 market1 `and` give (physical vol2 market2 `and` (financial pr cur cft))) sch

-- | Variance swap
--
-- A variance swap is an instrument which allows investors to trade future
-- realized (or historical) volatility against current implied volatility.
-- Only variance —the squared volatility— can be replicated with a static
-- hedge.
varianceSwap :: Price                    -- ^Notional amount
             -> Obs Double               -- ^Vega amount
             -> Obs Double               -- ^Variance amount
             -> Schedule                 -- ^Observation schedule
             -> Index                    -- ^Placeholder for index, currently not used
             -> [Price]                  -- ^Official closing prices of the underlying over the observation period
             -> Currency                 -- ^Payment currency
             -> CashFlowType             -- ^Payment cashflow type
             -> DiffDateTime             -- ^Settlement offset
             -> Calendar                 -- ^Calendar to use
             -> Contract
varianceSwap strikePrice vegaAmount varianceAmount sch underlyingIndex priceCurve cur cft sDiffTime cal =
    when (at settlementTime) $
          -- If the Equity Amount is negative the Variance Buyer will
          -- pay the Variance Seller an amount equal to the absolute
          --value of the Equity Amount
          cond (finalEquityAmount %<= konst 0)
             (give (financial (abs(finalEquityAmount)) cur cft))
          -- If the Equity Amount is positive the Variance Seller will pay
          -- the Variance Buyer the Equity Amount
             (financial finalEquityAmount cur cft)

    where
       settlementTime = last(sch) `adjustDateTime` sDiffTime
       finalEquityAmount = varianceAmount * (square(finalRealisedVolatility) - square(strikePrice))
       finalRealisedVolatility :: Price
       finalRealisedVolatility =
           100 * sqrt ((252 * sumLnPt priceCurve) / fromIntegral numBusinessDays)
         where
           numBusinessDays = length $ calendarDaysInPeriod cal (head sch, last sch)

       -- Function needed to calculate the final realised volatility
       sumLnPt :: [Price] -> Price
       sumLnPt [] = 0
       sumLnPt [x] = 0
       sumLnPt (x1:x2:xs) = square(logBase (exp 1) (x2 / x1)) + sumLnPt (x2:xs)

-- | Index Amortising Swap
--
-- Swap Buyer receives the following coupons:
-- Buyer rate paid e.g. quarterly (according to specified day count convention)
-- on the principal amount, amortised according to the values of the buyer rate
--
-- Swap Seller receives seller rate on same coupon dates.
indexAmortisingSwap :: Price                         -- ^Notional amount
                    -> Schedule                      -- ^Coupon dates
                    -> (Currency, Currency)          -- ^Currencies respectively for swap buyer and seller
                    -> (CashFlowType, CashFlowType)  -- ^Cashflow types respectively for cash buyer and seller
                    -> AmortisationTable             -- ^Amortisation table
                    -> DayCountConvention            -- ^Day count convention, e.g. 90/360
                    -> Index                         -- ^Buyer rate
                    -> Index                         -- ^Seller rate
                    -> Contract
indexAmortisingSwap notional cSch (cur1, cur2) (cft1, cft2) amTable dcc bRate sRate =
     foldr cAmt zero cSch

 where
   -- need to ensure fixing date is two days before coupon date
   cAmt cDate next = when (at cDate) $ allOf [cCalc, next]
     where
       cCalc = allOf[
                      financial (getAmRate bRate amTable %* notional %* konst dcc) cur1 cft1,
                      give $ financial (sRate %* notional %* konst dcc) cur2 cft2
                    ]

-- |Credit Default Swap
--
-- CDS buyer pays an agreed rate on an agreed basis on the principal amount
--
-- CDS seller makes capital payments if and when credit events happen
creditDefaultSwap :: [CreditEvent]                   -- ^List of credit events
                  -> Price                           -- ^Notional amount
                  -> Schedule                        -- ^Payment schedule
                  -> Currency                        -- ^Payment currency
                  -> Index                           -- ^Payment rate
                  -> CashFlowType                    -- ^Payment cashflow type
                  -> DayCountConvention              -- ^Day count convention, e.g. 90/360
                  -> Underlying                      -- ^Underlying asset
                  -> Contract
creditDefaultSwap cEvents notional cSch cur pRate cft dcc underlying =
  and
-- CDS buyer pays an agreed rate on an agreed basis on the principal amount
-- First, check if any credit events that would cause a termination of the contract has happened
    (cond(terminContract cEvents)
       zero
       (foldr cAmt zero cSch))

-- CDS seller makes capital payments if and when credit events happen
    (foldr capPay zero cEvents)

 where
   cAmt cDate next = when (at cDate) $ allOf [(financial (pRate %* notional %* konst dcc) cur cft), next]
   capPay (_, cTermin, sType, capPay, cCur, cHasHappened) next = when (cHasHappened) $ allOf[
            (if (sType == 'C')
             then (give (financial capPay cCur (CashFlowType "cash")))
             else (give underlying))
            , next]
