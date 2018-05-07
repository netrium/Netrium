-- |Netrium is Copyright Anthony Waite, Dave Hewett, Shaun Laurens & Contributors 2009-2018, and files herein are licensed
-- |under the MIT license,  the text of which can be found in license.txt
module Common where

import Prelude hiding (and, or, min, max, abs, negate, not, read, all, until)
import Contract
import Data.Time

--
-- Common types
--

type Price  = Obs Double
type Volume = Obs Double

data Market  = Market Commodity Unit Location
data Product = Product Market Schedule

type Index  = Obs Double

type Schedule = [Time]


--
-- Dates and times
--

-- | Midnight on a given day
date :: Integer -> Int -> Int -> Time
date year month day =
   UTCTime (fromGregorian year month day) 0

-- | A point in time on a given day
datetime :: Integer -> Int -> Int
         -> Int -> Int
         -> Time
datetime year month day hours minutes =
   UTCTime (fromGregorian year month day)
           (timeOfDayToTime (TimeOfDay hours minutes 0))


hours, minutes, seconds :: Int -> Duration
seconds s = Duration s
minutes m = seconds (m * 60)
hours   h = minutes (h * 60)


data DateOffset = DateOffset Int Int Int  -- years, months, days
  deriving (Eq, Show)

instance Num DateOffset where
  DateOffset y1 m1 d1 + DateOffset y2 m2 d2 =
    DateOffset (y1+y2) (m1+m2) (d1+d2)

  DateOffset y1 m1 d1 - DateOffset y2 m2 d2 =
    DateOffset (y1-y2) (m1-m2) (d1-d2)

  (*) = error "Multiplying date offsets makes no sense"
  
  negate (DateOffset y1 m1 d1) =
    DateOffset (negate y1) (negate m1) (negate d1)

  abs (DateOffset y1 m1 d1) =
    DateOffset (abs y1) (abs m1) (abs d1)

  signum = error "signum is not defines for date offsets"
  fromInteger n = DateOffset 0 0 (fromIntegral n)

offsetDate :: Time -> DateOffset -> Time
offsetDate (UTCTime day tod) (DateOffset y m d) = UTCTime (adj day) tod
  where
    adj = addDays                (fromIntegral d)
        . addGregorianMonthsClip (fromIntegral m)
        . addGregorianYearsClip  (fromIntegral y)

days, months, quarters, years, seasons :: Int -> DateOffset

days     n = DateOffset 0 0 n
months   n = DateOffset 0 n 0
quarters n = months (3*n)
years    n = DateOffset n 0 0
seasons  n = error "TODO: definition for seasons"


--
-- Extra combinators
--

allOf :: [Contract] -> Contract
allOf [] = zero
allOf xs = foldr1 and xs

--anyOneOf :: [(ChoiceLabel, Contract)] -> Contract
--anyOneOf


--
-- Simple trades
--

physical :: Volume -> Market -> Contract
physical vol (Market comod unit loc) =
    scale vol (one (Physical comod unit loc Nothing))

physicalWithDuration :: Volume -> Market -> Duration -> Contract
physicalWithDuration vol (Market comod unit loc) dur =
    scale vol (one (Physical comod unit loc (Just dur)))

financial :: Price -> Currency -> Contract
financial pr cur =
    scale pr (one (Financial cur))

fixedPrice :: Double -> Price
fixedPrice pr = konst pr

floatingPrice :: Obs Double -> Price
floatingPrice pr = pr


--
-- More complex trades
--

zcb :: Time -> Price -> Currency -> Contract
zcb t pr cur = when (at t) (financial pr cur)

forward :: FeeCalc
        -> Market
        -> Volume
        -> Price -> Currency
        -> Schedule
        -> Contract
forward fee market vol pr cur sch =
        give (calcFee fee vol pr cur sch)
  `and`
        allOf
          [ when (at t) $    physical vol market
                       `and` give (financial (vol * pr) cur)
          | t <- sch ]

-- Basic commodity swap: simultaneous swap of commodity A for commodity B for
-- a nominal cost over a delivery schedule (same cost for each delivery period)
commoditySwap :: (Market, Market)
              -> (Volume, Volume)
              -> Price -> Currency
              -> Schedule
              -> Contract
commoditySwap (market1, market2) (vol1, vol2) pr cur sch =
  allOf
    [ when (at t) $ physical vol1 market1
                    `and` give (physical vol2 market2
                               `and` (financial pr cur))
    | t <- sch ]


commoditSwing :: FeeCalc
              -> Market
              -> Price -> Currency
              -> (Volume, Volume, Volume)  -- ^ (low, normal, high) delivery volumes
              -> Int                       -- ^ number of exercise times
              -> DateOffset                -- ^ how far the option is in advance
              -> Schedule
              -> Contract
commoditSwing fee market pr cur (lowVol,normalVol, highVol)
              exerciseCount optTimeDiff sch =
    allOf
      [ give (calcFee fee normalVol pr cur sch)
      , read "count" (konst (fromIntegral exerciseCount)) $
          foldr leg zero sch
      ]
  where
    leg delTime remainder =
        when (at optTime) $
          cond (var "count" %<= konst 0)
               normal
               (or "normal" normal
                            (or "low-high" low high))
      where
        optTime = offsetDate delTime (abs optTimeDiff)
        normal  = when (at delTime) $
                    allOf [ delivery normalVol
                          , remainder
                        ]
        low     = when (at delTime) $
                    allOf [ delivery lowVol
                          , read "count" (var "count" %- konst 1) remainder
                          ]
        high    = when (at delTime) $
                    allOf [ delivery highVol
                          , read "count" (var "count" %- konst 1) remainder
                          ]

    delivery vol = and (physical vol market)
                       (give (financial (vol * pr) cur))


--
-- Trade metadata
--

newtype CounterParty = CounterParty String  deriving (Show, Eq)
newtype Trader       = Trader       String  deriving (Show, Eq)
newtype Book         = Book         String  deriving (Show, Eq)


--
-- Types for margining, fixing, netting, settlement
--

type MarginingSchedule  = Schedule
type FixingSchedule     = Schedule

type SettlementSchedule = Schedule
type SettlementVolumeVariance = Obs Double
data SettlementAgreement = SettlementAgreement Market SettlementSchedule

data MarketNettingAgreement = MarketNettingAgreement CounterParty Market
data CrossMarketNettingAgreement = CrossMarketNettingAgreement CounterParty


--
-- Fee calculations
--

newtype FeeCalc = FeeCalc (Volume -> Price -> Schedule -> Price)

calcFee :: FeeCalc
        -> Volume -> Price -> Currency -> Schedule
        -> Contract
calcFee (FeeCalc fc) vol pr cur sch = financial (fc vol pr sch) cur

zeroFee :: FeeCalc
zeroFee = FeeCalc $ \_ _ _ -> 0

initialMarginFee :: FeeCalc
initialMarginFee =
    FeeCalc $ \vol pr sch ->
      let numdays = fromIntegral (length sch)
       in vol * pr * konst numdays
--TODO: this assumes the schedule has just one entry
--      per-day which may not be true.

computedFee :: Price -> FeeCalc
computedFee pr = FeeCalc $ \_ _ _ -> pr

fixedFee :: Double -> FeeCalc
fixedFee = computedFee . konst

exchangeFee :: Price -> FeeCalc
exchangeFee = computedFee

andFee :: FeeCalc -> FeeCalc -> FeeCalc
andFee (FeeCalc fc1) (FeeCalc fc2) =
    FeeCalc $ \vol pr sch -> fc1 vol pr sch
                           + fc2 vol pr sch


--
-- Options
--

european :: Time -> Contract -> Contract
european exTime c = when (at exTime) $ or "choice" c zero 


--
-- Unknown
--

type TradedDate = Obs Time

data Direction = Give | Take

