-- |Netrium is Copyright Netrium Consulting Ltd, 2009-2012, and files herein are licensed
-- |under the Affero General Public License version 3, the text of which can
-- |be found in agpl.txt, or any later version of the AGPL, unless otherwise
-- |noted. 
--
-- Module for common types and functions
module Common where

import Prelude hiding (and, or, min, max, abs, negate, not, read, until)
import Contract

import Data.List (transpose)
import Data.Time
import Data.Monoid
import Data.Maybe

-- *Common types
-- |A price is modelled as an observable double
type Price  = Obs Double
-- |A volume is modelled as an observable double
type Volume = Obs Double
-- |A price curve is a list of prices
type PriceCurve = [Price]

-- |Markets, e.g. UK Power (Electricity MWh UK), NBP Gas (Gas TH UK)
data Market  = Market Commodity Unit Location
-- |A product is defined as a market and a delivery schedule
data Product = Product Market Schedule

-- |Index, e.g. SPX
type Index  = Obs Double

-- |Basic schedule, defined as a ist of date times
type Schedule = [DateTime]
-- |Advanced schedule, defined as a list of time ranges
type SegmentedSchedule = [[DateTime]]
-- |Amortisation table, defined as a list of amortisation rates and observation rates
type AmortisationTable = [(Double, Double)]

-- |Day count conventions, e.g. 90/360
type DayCountConvention = Double

-- |Underlying assets
type Underlying = Contract

-- *Dates and times
-- |Use the UTC format
type DateTime = UTCTime

-- |Deconstruct a DateTime
toGregorian' :: DateTime -> (Integer, Int, Int)
toGregorian' dt = (y, m, d)
    where (y, m, d) = toGregorian (utctDay dt)

-- |Midnight on a given day
date :: Integer -> Int -> Int -> DateTime
date year month day =
   UTCTime (fromGregorian year month day) 0

-- |A point in time on a given day
datetime :: Integer -> Int -> Int
         -> Int -> Int
         -> DateTime
datetime year month day hours minutes =
   UTCTime (fromGregorian year month day)
           (timeOfDayToTime (TimeOfDay hours minutes 0))

-- |The difference between 2 date times
newtype DiffDateTime = DiffDateTime (DateTime -> DateTime)

infixr 5 <>

-- |Used to append hours and minutes to a date, i.e. define the time within a day
(<>) :: Monoid a => a -> a -> a
a <> b = mappend a b

instance Monoid DiffDateTime where
  mempty = DiffDateTime id
  mappend (DiffDateTime a) (DiffDateTime b) = DiffDateTime (b . a)

-- |Adjust a date time by a given date time difference
adjustDateTime :: DateTime -> DiffDateTime -> DateTime
adjustDateTime d (DiffDateTime adj) = adj d

adjustDate adj = DiffDateTime (\(UTCTime day tod) -> UTCTime (adj day) tod)
adjustTime adj = DiffDateTime (\(UTCTime day tod) -> UTCTime day (adj tod))

-- |Return a positive time difference of a given number of days
daysLater :: Int -> DiffDateTime
daysLater     n = adjustDate (addDays (fromIntegral n))

-- |Return a negative time difference of a given number of days
daysEarlier :: Int -> DiffDateTime
daysEarlier     = daysLater . negate

-- |Return a positive time difference of a given number of months
monthsEarlier :: Int -> DiffDateTime
monthsEarlier n = adjustDate (addGregorianMonthsClip (fromIntegral n))

-- |Return a negative time difference of a given number of months
monthsLater :: Int -> DiffDateTime
monthsLater     = monthsEarlier . negate

-- |Return a negative time difference of a given number of quarters
quartersLater :: Int -> DiffDateTime
quartersLater n = monthsEarlier (n*3)

-- |Return a positive time difference of a given number of quarters
quartersEarlier :: Int -> DiffDateTime
quartersEarlier = quartersEarlier . negate

-- |Return a positive time difference of a given number of years
yearsEarlier :: Int -> DiffDateTime
yearsEarlier n  = adjustDate (addGregorianYearsClip (fromIntegral n))

-- |Return a negative time difference of a given number of years
yearsLater :: Int -> DiffDateTime
yearsLater      = yearsEarlier . negate

-- |Return a time difference of a given number of seasons
--
-- Not implemented yet
seasons :: Int -> DiffDateTime
seasons  n = error "TODO: definition for seasons"

-- |Return the time difference between midnight and a given time
atTimeOfDay :: Int            -- ^Hours
            -> Int            -- ^Minutes
            -> DiffDateTime
atTimeOfDay h m = adjustTime $ \_ -> timeOfDayToTime (TimeOfDay h m 0)

-- |Return a time difference of a given number of days
onDayOfMonth :: Int -> DiffDateTime
onDayOfMonth d =
    adjustDate $ \day ->
      let (y, m, _) = toGregorian day
       in fromGregorian y m d

-- |Return a time difference of a given number of months
inMonth :: Int -> DiffDateTime
inMonth m =
    adjustDate $ \day ->
      let (y, _, d) = toGregorian day
       in fromGregorian y m d

-- |Define a time duration, in terms of hours, minutes and seconds.
duration :: Int        -- ^Hours
         -> Int        -- ^Minutes
         -> Int        -- ^Seconds
         -> Duration
duration h m s = Duration ((h * 60 + m) * 60 + s)

-- |Double conversion - e.g. FX, unit conversion
convertDouble :: Obs Double  -- ^Double to convert
              -> Obs Double  -- ^Conversion factor
              -> Obs Double
convertDouble toConvert factor = toConvert * factor

-- *Extra combinators
-- |Combine a list of contract into a contract
allOf :: [Contract] -> Contract
allOf [] = zero
allOf xs = foldr1 and xs

--anyOneOf :: [(ChoiceLabel, Contract)] -> Contract
--anyOneOf

-- |Choice between a contract and a zero contract (used for options)
orZero :: ChoiceId -> Contract -> Contract
orZero cid c = or cid c zero

-- *Simple contract templates
-- |Basic physical leg
physical :: Volume -> Market -> Contract
physical vol (Market comod unit loc) =
    scale vol (one (Physical comod unit loc Nothing))

-- |A physical leg with a duration
physicalWithDuration :: Volume -> Market -> Duration -> Contract
physicalWithDuration vol (Market comod unit loc) dur =
    scale vol (one (Physical comod unit loc (Just dur)))

-- A physical contract with a delivery/settlement schedule
--physicalWithSchedule :: Volume -> Market -> Maybe Schedule -> Maybe Duration -> Contract
--physicalWithSchedule vol m sch dur =
--    if (isNothing sch) then (physicalWithDuration vol m dur) else (allOf [when (at t) $ (physicalWithDuration vol m dur) | t <- (fromJust sch)])

-- |Basic financial leg
financial :: Price -> Currency -> CashFlowType -> Contract
financial pr cur cft =
    scale pr (one (Financial cur cft))

-- A financial contract with a delivery/settlement schedule
--financialWithSchedule :: Price -> Currency -> CashFlowType -> Maybe Schedule -> Contract
--financialWithSchedule pr cur cft sch =
--    if (isNothing sch) then (financial pr cur cft) else (allOf [when (at t) $ (financial pr cur cft) | t <- (fromJust sch)])

-- |A fixed price: transform a double into an observable
fixedPrice :: Double -> Price
fixedPrice pr = konst pr

-- |A floating price
floatingPrice :: Obs Double -> Price
floatingPrice pr = pr

-- |Distribute the contract over a schedule
-- 
-- Used to apply a settlement/delivery schedule
scheduledContract :: Contract -> Schedule -> Contract
scheduledContract c sch = 
        allOf [ when (at t) $ c | t <- sch ]

-- *More complex trades
-- |A zero coupon bond: delivery of a notional amount at a given time
zcb :: DateTime      -- ^Delivery date time
    -> Price         -- ^Notional amount
    -> Currency      -- ^Payment currency
    -> CashFlowType  -- ^Cashflow type
    -> Contract
zcb t pr cur cft = when (at t) (financial pr cur cft)

-- More 'advanced' bonds
-- |Chooser Range
--
-- The note pays a coupon based on the number of days that e.g. 6-month LIBOR sits within an e.g. [80bps] range.
-- The range is chosen by the buyer at the beginning of each coupon period.
chooserLeg :: Int		    -- ^Range for observation period
	       -> Int		    -- ^Index strike for observation period
	       -> Time		    -- ^Coupon settlement date
	       -> Schedule		-- ^Dates for observation period
           -> Currency      -- ^Payment currency
           -> Index         -- ^Coupon rate
	       -> CashFlowType  -- ^Cashflow type
           -> Contract
 
chooserLeg range strike setD sch cur cRate cft =

    foldr daily settlementDate sch (konst 0)

  where
    daily (day) next daysWithinRange =
      when (at day) $
        read "daysWithinRange"
          (daysWithinRange %+ indexInRange strike range)
          (next (var "daysWithinRange"))
        
    settlementDate couponAmount = scheduledContract (financial paymentDue cur cft) [setD]
      where
	-- bit of a cheat here. should be a count of actual business days, not total number of days
        paymentDue = cRate %* var "daysWithinRange" %/ konst (fromIntegral(length sch))

    -- If the coupon rate is within strike +/- range then increase days within range
	-- otherwise don't increase days within range
    indexInRange strike range =

        ifthen (cRate %<= konst (fromIntegral(strike + range))
               %&& cRate %> konst (fromIntegral(strike - range)))
	       (konst 1) (konst 0)

-- |A chooser note has a set of 'chooserLeg'
chooserNote :: [Contract] -> Contract
chooserNote c = allOf c

-- |Forward: agreement to buy or sell an asset at a pre-agreed future point
forward :: FeeCalc                            -- ^Fees
        -> Market                             -- ^Market, e.g. NBP Gas
        -> Volume                             -- ^Notional volume
        -> Price                              -- ^Price
        -> Currency                           -- ^Payment currency
        -> CashFlowType                       -- ^Cashflow type
        -> Schedule                           -- ^Schedule for physical settlement
        -> Schedule                           -- ^Schedule for financial settlement
        -> Contract
forward fee market vol pr cur cft pSch fSch =
        give (calcFee fee vol pr cur fSch)
  `and`
        (scheduledContract (physical vol market) pSch) `and` (scheduledContract (give (financial (vol * pr) cur cft)) fSch)

-- |Commodity swing: a contract guaranteeing one of two parties periodic delivery of a certain amount of commodity (the nominated amount)
-- on certain dates in the future, within a given delivery period, at a stipulated constant price (the strike price).
--
-- The party can also change ("swing") the amount delivered from the nominated amount to a new amount on a short notice, for a limited number of times
commoditySwing :: FeeCalc                   -- ^Fees
               -> Market                    -- ^Market, e.g. NBP Gas
               -> Price                     -- ^Price
               -> Currency                  -- ^Payment currency
               -> CashFlowType              -- ^Cashflow type
               -> (Volume, Volume, Volume)  -- ^(low, normal, high) delivery volumes
               -> Int                       -- ^Number of exercise times
               -> DiffDateTime              -- ^How far the option is in advance
               -> SegmentedSchedule         -- ^Delivery schedule
               -> Contract
commoditySwing fee market pr cur cft (lowVol,normalVol, highVol)
              exerciseCount optionDiffTime sched =
    allOf
      [ give (calcFee fee normalVol pr cur sched)
      , read "count" (konst (fromIntegral exerciseCount)) $
          foldr leg zero sched
      ]
  where
    leg deliverySegment remainder =
        when (at optionTime) $
          cond (var "count" %<= konst 0)
               normal
               (or "normal" normal
                            (or "low-high" low high))
      where
        optionTime        = firstDeliveryTime `adjustDateTime` optionDiffTime
        firstDeliveryTime = head deliverySegment

        normal  = and (delivery deliverySegment normalVol) remainder
        low     = and (delivery deliverySegment lowVol)
                      (read "count" (var "count" %- konst 1) remainder)
        high    = and (delivery deliverySegment highVol)
                      (read "count" (var "count" %- konst 1) remainder)

    delivery sch vol = scheduledContract (and (physical vol market) (give (financial (vol * pr) cur cft))) sch

-- *Trade metadata
-- |Counterparty
newtype CounterParty = CounterParty String  deriving (Show, Eq)
-- |Trader
newtype Trader       = Trader       String  deriving (Show, Eq)
-- |Book
newtype Book         = Book         String  deriving (Show, Eq)


-- *Types for margining, fixing, netting, settlement
-- |Margining schedule
type MarginingSchedule  = Schedule
-- |Fixing schedule
type FixingSchedule     = Schedule

-- |Settlement schedule
type SettlementSchedule = Schedule
-- |Settlement volume variance (for actuals)
type SettlementVolumeVariance = Obs Double
-- |Settlement agreement
data SettlementAgreement = SettlementAgreement Market SettlementSchedule

-- |Market netting agreement
data MarketNettingAgreement = MarketNettingAgreement CounterParty Market
-- |Cross-market netting agreement
data CrossMarketNettingAgreement = CrossMarketNettingAgreement CounterParty


-- *Fee calculations
-- |Type for fee calculations
newtype FeeCalc = FeeCalc (Volume -> Price -> Currency -> Int -> Contract)

instance Monoid FeeCalc where
  mempty  = zeroFee
  mappend = andFee

-- |Function to calculate fees
calcFee :: FeeCalc     -- ^Function to apply to calculate the fees
        -> Volume      -- ^Contract notional volume
        -> Price       -- ^Contract price
        -> Currency    -- ^Contract currency
        -> [a]         -- ^The schedule
        -> Contract
calcFee (FeeCalc fc) vol pr cur sch = fc vol pr cur (length sch)

-- |No fee
zeroFee :: FeeCalc
zeroFee = FeeCalc $ \_ _ _ _ -> zero

-- |Fees for initial margin
initialMarginFee :: FeeCalc
initialMarginFee =
    FeeCalc $ \vol pr cur numDeliveries ->
      financial (vol * pr * konst (fromIntegral (numDeliveries))) cur cft
  where
    cft = CashFlowType "initialMargin"

-- |Basic calculated fees
computedFee :: CashFlowType -> Price -> FeeCalc
computedFee cft pr = FeeCalc $ \_ _ cur _ -> financial pr cur cft

-- |Fixed fees
fixedFee :: CashFlowType -> Double -> FeeCalc
fixedFee cft = computedFee cft . konst

-- |Exchange fees
exchangeFee :: Price -> FeeCalc
exchangeFee = computedFee cft
  where
    cft = CashFlowType "exchange"

-- |Append 2 fee calculations
andFee :: FeeCalc -> FeeCalc -> FeeCalc
andFee (FeeCalc fc1) (FeeCalc fc2) =
    FeeCalc $ \vol pr cur sch -> fc1 vol pr cur sch
                           `and` fc2 vol pr cur sch

-- Not used yet
-- |Traded date
type TradedDate = Obs DateTime
-- |Contract direction (give or take)
data Direction = DGive | DTake

-- *Utilities
-- |Function to determine amortisation rate from an amortisation table and an observable rate
getAmRate :: Obs Double -> AmortisationTable -> Obs Double
getAmRate rate [] = konst 0
getAmRate rate ((x1,x2):xs) = ifthen (rate %< konst x1)
                                 (konst x2) $
                              getAmRate rate xs

-- *List helper functions
-- |Extract n last elements from a list
lastn ::  Int -> [a] -> [a]
lastn n l = reverse (take n (reverse l))

-- *Basic math functions
-- |Square a double
square :: Num n => n -> n
square x = x * x