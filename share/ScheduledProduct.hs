-- |Netrium is Copyright Anthony Waite, Dave Hewett, Shaun Laurens & Contributors 2009-2018, and files herein are licensed
-- |under the MIT license,  the text of which can be found in license.txt
--
-- Scheduled products: products delivered according to a schedule.
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ScheduledProduct (

    -- * Scheduled products
    ScheduledProduct,
    scheduledProduct,
    
    -- ** Defining scheduled products
    defineScheduledProduct,
    DeliverySchedule,
    deliverySchedule,
    
    -- ** Delivery shape
    DeliveryShape,
    deliverAtMidnight,
    deliverAtTimeOfDay,
    complexDeliveryShape,
    
    -- * Relative scheduled product type
    ScheduledProductRelative,
    scheduledProductRelative,
    setScheduledProductDate,
    defineScheduledProductRelative,
    DeliveryScheduleRelative,
    relativeDeliverySchedule,

  ) where

import Contract
import Common
import Calendar

import Data.Monoid


-- | A scheduled product is a contract to repeatedly acquire a quantity of an
-- underlying product according to a delivery schedule.
--
-- Use 'productQuantity' to acquire a scheduled product and specify the
-- quantity of the underlying product to recieve each time.
--
data ScheduledProduct q = ScheduledProduct (q -> Contract) DeliverySchedule

-- | A relative product is a product using a relative delivery schedule.
-- Use 'setProductDate' to fix the delivery schedule.
--
data ScheduledProductRelative q = ScheduledProductRelative (q -> Contract) DeliveryScheduleRelative

-- | A delivery schedule for some product.
--
-- This is an absolute schedule (e.g. June 2011, which delivers
-- from 01/06/2011 to 30/06/2011 according to a given calendar).
--
newtype DeliverySchedule
      = DeliverySchedule
          [([DateTime]      -- delivery days
           , DateTime       -- start date
           , DateTime       -- end date
           ,DeliveryShape
           )]
  deriving Monoid

-- | A relative delivery schedule for some product.
--
-- This is a relative schedule (e.g. day ahead, month + 1, balance of month...
-- - once again according to a given calendar).
--
newtype DeliveryScheduleRelative
      = DeliveryScheduleRelative
          [(DiffDateTime  -- start date/time relative to acquire
           ,DiffDateTime  -- end date/time relative to acquire
           ,Calendar      -- delivery days
           ,DeliveryShape
           )]
  deriving Monoid

-- | The daily delivery shape says how often and when during the day the
-- product is acquired.
--
-- The simple cases are once daily at midnight using 'deliverAtMidnight', or
-- once daily at a particular time of day using 'deliverAtTimeOfDay'.
--
-- Complex delivery shapes can be constructed by combining multiple
-- deliveries using the '(<>)' operator, or by using 'complexDeliveryShape'.
--
-- For exmple, two daily deliveries, 6am and 6pm:
--
-- > deliverAtTimeOfDay 6 0 <> deliverAtTimeOfDay 18 0
--
-- Or, half-hourly delivery between 7am and 7pm:
--
-- > complexDeliveryShape [ deliverAtTimeOfDay hr ms | hr <- [7..18], ms <- [0,30] ]
--
newtype DeliveryShape = DeliveryShape [DiffDateTime]
  deriving Monoid


-- | A contract to acquire a scheduled product using a given quantity of the
-- underlying product.
-- 
scheduledProduct :: q -> ScheduledProduct q -> Contract
scheduledProduct quantity (ScheduledProduct underlying (DeliverySchedule blocks))
  = allOf
      [ when (at t) (underlying quantity)
      | (days, _, _, DeliveryShape deliverySegments) <- blocks
      , day    <- days
      , offset <- deliverySegments
      , let t = adjustDateTime day offset ]

-- | A contract to acquire a relative scheduled product at a given date using a
-- given quantity of the underlying product.
-- 
scheduledProductRelative :: DateTime -> q -> ScheduledProductRelative q -> Contract
scheduledProductRelative acquireDate quantity product =
  scheduledProduct quantity (setScheduledProductDate acquireDate product)

-- | Turn a product with a relative schedule into a product with an
-- absolute schedule.
--
setScheduledProductDate :: DateTime -> ScheduledProductRelative q -> ScheduledProduct q
setScheduledProductDate acquireDate
    (ScheduledProductRelative p (DeliveryScheduleRelative relShedule))
  = ScheduledProduct p $ DeliverySchedule
      [ (days, startDate, endDate, shape)
      | (startOffset, endOffset, cal, shape) <- relShedule
      , let startDate = adjustDateTime acquireDate startOffset
            endDate   = adjustDateTime acquireDate endOffset
            days      = calendarDaysInPeriod cal (startDate, endDate)
      ]


-- | Define a scheduled product based on an underlying contract to acquire a
-- given quantity of a product.
--
defineScheduledProduct :: (q -> Contract) -> DeliverySchedule -> ScheduledProduct q
defineScheduledProduct = ScheduledProduct

-- | Define a scheduled product with a schedule that is relative to some date.
--
defineScheduledProductRelative :: (q -> Contract) -> DeliveryScheduleRelative -> ScheduledProductRelative q
defineScheduledProductRelative = ScheduledProductRelative


-- | Define a delivery schedule using absolute dates.
--
-- A delivery schedule with multiple different blocks can be defined by
-- combining schedules by using the '(<>)' operator.
--
deliverySchedule :: DateTime -> DateTime
                 -> Calendar -- ^ What days to deliver on
                 -> DeliveryShape
                 -> DeliverySchedule
deliverySchedule start end cal shape =
    DeliverySchedule [(days, start, end, shape)]
  where
    days = calendarDaysInPeriod cal (start, end)

-- | Define a delivery schedule using dates relative to the acquisition date.
--
-- A delivery schedule with multiple different blocks can be defined by
-- combining schedules by using the '(<>)' operator.
--
relativeDeliverySchedule :: DiffDateTime -> DiffDateTime
                         -> Calendar -- ^ What days to deliver on
                         -> DeliveryShape
                         -> DeliveryScheduleRelative
relativeDeliverySchedule start end cal shape =
    DeliveryScheduleRelative [(start, end, cal, shape)]


-- | Single delivery at midnight.
--
deliverAtMidnight  :: DeliveryShape
deliverAtMidnight = deliverAtTimeOfDay 0 0

-- | Single delivery at a particular time of day.
--
deliverAtTimeOfDay :: Int -> Int -> DeliveryShape
deliverAtTimeOfDay hs ms = DeliveryShape [atTimeOfDay hs ms]

-- | Defines a complex intra-day delivery shape as a sequence of deliveries.
--
-- For exmple, half-hourly delivery between 7am and 7pm:
--
-- > complexDeliveryShape [ deliverAtTimeOfDay hr ms | hr <- [7..18], ms <- [0,30] ]
--
complexDeliveryShape :: [DeliveryShape] -> DeliveryShape
complexDeliveryShape = mconcat
