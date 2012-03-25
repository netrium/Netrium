-- |Netrium is Copyright Netrium Consulting Ltd, 2009-2012, and files herein are licensed
-- |under the Affero General Public License version 3, the text of which can
-- |be found in agpl.txt, or any later version of the AGPL, unless otherwise
-- |noted. 
--
-- Module for options
--
-- Options are built up as follows:
--
-- In general terms, an option is a contract that gives you the right, under
-- certain conditions, to buy (call) or sell (put) some underlying asset.
--
-- There are two separate aspects to an \"underlying\":
--
--  * There are the rights (ie a contract) you get when you choose to exercise
--    the option (e.g. the right to buy some physical or financial asset)
--
--  * An observable, usually related to the spot price of the physical or
--    financial asset that the option involves.
--
module Options where

import Prelude hiding (and, or, min, max, abs, negate, not, read, until)
import Contract
import Common
import Data.List (transpose)
import Data.Monoid

-- * Types
-- | Add an exercise condition to a contract, e.g. to add a barrier condition
type ExerciseCondition   = Contract -> Contract
-- | Define a time window when a condition applies as a set of time ranges
type ConditionWindow     = [(DateTime, DateTime)]
-- | An expiration condition
type ExpirationCondition = Contract -> Contract

-- | The spot price of the underlying asset
type UnderlyingPrice    = Price

-- | The rights to acquire the underlying asset. This must include
-- any payments (which may depend on the option strike price).
--
type UnderlyingContract = StrikePrice -> Contract

-- | The price the option owner pays to acquire the underlying
type StrikePrice = Price

-- | The direction of the option (put or call)
data OptionDirection  = CallOption  -- ^ Option to buy underlying
                      | PutOption   -- ^ Option to sell underlying

-- | Option contracts differ in the detail of when and at what price the
-- option can be exercised.
--
newtype ExerciseDetails
      = ExerciseDetails (ChoiceId -> (StrikePrice -> Contract) -> Contract)

-- * Option template
-- | Basic option template. Flexibility is achieved through 'ExerciseDetails'.
option :: ChoiceId                  -- ^Choice label
       -> ExerciseDetails           -- ^Details of when and at what price the option can be exercised
       -> OptionDirection           -- ^Direction of the option (call or put)
       -> OptionAttrs               -- ^Extra option attributes / features
       -> UnderlyingContract        -- ^Underlying asset
       -> Contract
option cid (ExerciseDetails exerciseDetails)
       optionDirection (OptionAttrs attrs) underlyingContract

  = attrs $ exerciseDetails cid $ \strikePrice ->

      case optionDirection of
              CallOption -> underlyingContract strikePrice
              PutOption  -> give (underlyingContract strikePrice)


-- ** Option atributes
-- | Additional optional attributes of an 'option'.
--
newtype OptionAttrs = OptionAttrs (Contract -> Contract)

instance Monoid OptionAttrs where
  mempty = OptionAttrs id
  mappend (OptionAttrs a) (OptionAttrs b) = OptionAttrs (b . a)

emptyOptionAttrs :: OptionAttrs
emptyOptionAttrs = mempty

-- | Pay the given premium on aquiring the option.
--
withPremium :: Price -> Currency -> OptionAttrs
withPremium premium cur =
    OptionAttrs (and payPremium)
  where
    payPremium = give $ financial premium cur (CashFlowType "premium")

-- | Pay the given premium multiple times acording to the payment schedule.
--
withPremiumSchedule :: Price -> Currency -> Schedule -> OptionAttrs
withPremiumSchedule premium cur schedule =
    OptionAttrs (and payPremium)
  where
    payPremium     = scheduled payInstallment schedule
    payInstallment = give $ financial premium cur (CashFlowType "premium")


-- * Templates for option parameters
-- ** Exercise time
-- | European exercise: option may be exercised only at the expiry date of the
-- option, i.e. at a single pre-defined point in time.
europeanExercise :: DateTime -> StrikePrice -> ExerciseDetails
europeanExercise exTime strikePrice =
    ExerciseDetails $ \cid c ->
      when (at exTime) (orZero cid (c strikePrice))

-- | American exercise: option may be exercised at any time before the
-- expiry date.
americanExercise :: (DateTime, DateTime) -> StrikePrice -> ExerciseDetails
americanExercise (t1, t2) strikePrice =
    ExerciseDetails $ \cid c ->
      anytime cid (after t1 %&& before t2) (c strikePrice)

-- | Bermudan exercise: option may be exercised at a set (always discretely
-- spaced) number of times.
bermudanExercise :: [(DateTime, DateTime)] -> StrikePrice -> ExerciseDetails
bermudanExercise exerciseWindows strikePrice =
    ExerciseDetails $ \cid c ->
      allOf
        [ anytime cid (after t1 %&& before t2) (c strikePrice)
        | (t1, t2) <- exerciseWindows ]

-- ** Payoff
-- | Asian exercise: option where the payoff is not determined by the underlying
-- price at maturity
-- but by the average underlying price over some pre-set period of time.
asianExercise :: UnderlyingPrice -> Schedule -> ExerciseDetails
asianExercise underlyingPrice sch =
    ExerciseDetails $ \cid c ->
      foldr sample (final cid c) sch 0
  where
    sample t remainder sum =
      when (at t) $
        letin "sum" (sum + underlyingPrice) $ \sum ->
          remainder sum

    final cid c sum = orZero cid (c strikePrice)
      where
        strikePrice = sum / fromIntegral (length sch)

-- ** Exercise conditions
-- | Barrier knock-in: options that start their lives worthless and only become
-- active in the event a predetermined knock-in barrier price is breached
-- Barrier options become activated or, on the contrary, null and void only if
-- the underlier reaches a predetermined level (barrier).
barrierKnockIn :: Obs Bool -> ExerciseDetails -> ExerciseDetails
barrierKnockIn condition (ExerciseDetails exerciseDetails) =
    ExerciseDetails $ \cid c ->
      when condition (exerciseDetails cid c)

-- | Barrier up-and-in: spot price starts below the barrier level and has to
-- move up for the option to become activated
barrierUpAndIn :: Index -> Price -> ExerciseDetails -> ExerciseDetails
barrierUpAndIn index ceiling = barrierKnockIn (index %>= ceiling)

-- | Barrier down-and-in: spot price starts above the barrier level and has to
-- move down for the option to become activated.
barrierDownAndIn :: Index -> Price -> ExerciseDetails -> ExerciseDetails
barrierDownAndIn index floor = barrierKnockIn (index %<= floor)

-- * More advanced option templates
-- | Commodity Spread Option: a strip of options with a spread underlying
-- (e.g. with x legs)
--
-- Exercise is determined by an offset to the earliest delivery date of the
-- underlying for each given option
--
-- Generic so:
--
--       * Options can be daily, monthly or any grain/combination of grains
--
--       * Underlying can have any number of legs (the grain of the leg does
--         not have to be the same)
commoditySpreadOption :: ChoiceId                     -- ^Choice label
               -> [( Market
                   , Volume
                   , Price, Currency, CashFlowType
                   , SegmentedSchedule
                   , FeeCalc )]                       -- ^List of underlying legs
               -> DiffDateTime                        -- ^Exercise date offset (relative to leg)
               -> DiffDateTime                        -- ^Payment date offset (relative to exercise)
               -> OptionDirection                     -- ^Option direction (put or call)
               -> StrikePrice                         -- ^Strike price of the option
               -> Currency                            -- ^Currency of the strike price
               -> CashFlowType                        -- ^Cashflow type of the strike price
               -> Obs Double                          -- ^Premium
               -> Currency                            -- ^Currency for the premium
               -> Contract
commoditySpreadOption cid legs exerciseDiffTime paymentDiffTime opDir strikePrice currency cftype premium pCur =
   allOf
     [ legOption groupedLeg $
         allOf [ forward fee m pr cur cft vol seg seg
               | (m, pr, cur, cft, vol, seg, fee) <- groupedLeg ]
     | groupedLeg <- groupedLegs ]

 where
   groupedLegs =
     transpose
       [ [ (m, pr, cur, cft, vol, seg, fee) | seg <- sch ]
       | (m, pr, cur, cft, vol, sch, fee) <- legs ]

   legOption groupedLeg underlying =

       option cid exerciseDetails opDir optionPremium $ \strikePrice ->
         give (financial strikePrice currency cftype)
         `and` underlying

     where
       exerciseDetails = europeanExercise exerciseTime strikePrice
       exerciseTime    = adjustDateTime earliestDeliveryTime exerciseDiffTime
         where
           earliestDeliveryTime =
             minimum [ t | (_, _, _, _, _, (t:_), _) <- groupedLeg ]

       optionPremium  = withPremiumSchedule premium pCur [premiumTime]
       premiumTime    = adjustDateTime exerciseTime paymentDiffTime
