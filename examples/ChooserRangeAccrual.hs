module ChooserRangeAccrual where

import Prelude hiding (and, or, until, read, all, any, max, min, negate, abs)
import Common
import Contract
import DecisionTree
import Interpreter
import Data.Time
import List hiding (and)

import Display

type Price  = Double

financial = financial' . konst
financial' o cur = Scale o (One (Financial cur))

m = (*1000000)
		
dailySch1	= [date 2011 1 d | d <- [1..2]]
dailySch2       = [date 2011 2 d | d <- [1..2]]
dailySch3       = [date 2011 3 d | d <- [1..2]]

chooserLeg :: Int		-- range for observation period
	   -> Int		-- index strike for observation period
	   -> Time		-- coupon settlement date
	   -> Schedule		-- dates for observation period
	   -> Contract
 
chooserLeg range strike setD sch =

    foldr daily settlementDate sch (konst 0)

  where
    daily (day) next daysWithinRange =
      when (at day) $
        read "daysWithinRange"
          (daysWithinRange %+ indexInRange strike range)
          (next (var "daysWithinRange"))
        
    settlementDate couponAmount = when (at setD) $ financial' paymentDue (Currency "eur")
      where
	-- bit of a cheat here. should be a count of actual business days, not total number of days
        paymentDue = primVar "LIBOR.EUR.6M" %* var "daysWithinRange" %/ konst (fromIntegral(length sch))

        -- If the 6M Euro libor rate is within strike +/- range then increase days within range
	-- otherwise dont increase days within range
    indexInRange strike range =

        ifthen (primVar "LIBOR.EUR.6M" %<= konst (fromIntegral(strike + range))
               %&& primVar "LIBOR.EUR.6M" %> konst (fromIntegral(strike - range)))
	       (konst 1) (konst 0)


ex1 = chooserLeg 80 500 (last dailySch1) dailySch1

chooserNote :: [Int]
            -> [Int]
            -> [Schedule]
            -> Contract
chooserNote rangeList strikeList schedules = 
    allOf
      [ chooserLeg range strike (last sch) sch
      | (range, strike, sch) <- zip3 rangeList strikeList schedules ]



ex3 :: Contract
ex3 = chooserNote 
        [80,45,50] 
        [500,450, 550]
        [dailySch1,dailySch2,dailySch3]
