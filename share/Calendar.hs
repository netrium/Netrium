-- |Netrium is Copyright Netrium Consulting Ltd, 2009-2012, and files herein are licensed
-- |under the Affero General Public License version 3, the text of which can
-- |be found in agpl.txt, or any later version of the AGPL, unless otherwise
-- |noted. 
--
-- Date functions that are sensitive to various kinds of calendars.
--
-- A 'Calendar' type is provided which allows us to define various kinds of
-- calendars including business days, working days, delivery schedules etc.
--
{-# LANGUAGE BangPatterns, Rank2Types #-}
module Calendar (

    -- * Calendar type
    Calendar,

    -- ** Date adjustment functions
    calendarDayFirst,
    calendarDayNext,
    calendarDayPrevious,
    calendarDaysEarlier,
    calendarDaysLater,

    -- ** Other utilities
    dayInCalendar,
    calendarDaysInPeriod,
    calendarFirstDay,
    calendarLastDay,

    -- ** Constructing calendars
    getBusinessDayCalendar,
    newCalendar,
    DayType(..),

    -- ** Deprecated
    firstWorkingDay,
    nextWorkingDay,
    previousWorkingDay,
    workingDaysEarlier,
    workingDaysLater,
    isWorkingDay,
    workingDaysInPeriod,
    getCalendar,
  ) where

import Common

import Data.Time
import Data.Array.Unboxed

-- * Calendar types

-- | A calendar is a set of days.
--
-- For example a working days calendar is a set of working days. This lets
-- us do date calculations that are sensitive to holidays etc.
--
-- We can also define or calculate calendars for special purposes, like
-- all the delivery days in a contract delivery schedule.
--
-- Note that calendars are not contigious sets of days, that is they are not
-- date ranges, they usually do contain gaps. Calendars are finite, that is
-- there is a first and a last day that are listed in the calendar.
--
data Calendar = Calendar !String             -- calendar name (for error messages)
                         !(UArray Day Bool)  -- bitmap of day -> workday
                                             -- very compact: 46 bytes per year

-- In future we might want infinte calendars to allow for weekday or weekend,
-- or monthly calendars. These would be useful in calculations, if not directly
-- on their own. But then these would not be suitable for use as delivery
-- schedules. We might need to distinguish finite range calendars. Or just make
-- it a runtime error.

calendarRange :: Calendar -> (Day, Day)
calendarRange (Calendar _ daybitmap) = bounds daybitmap

-- | The first day that is listed in the calendar.
--
calendarFirstDay :: Calendar -> DateTime
calendarFirstDay cal = UTCTime first 0 where (first, _) = calendarRange cal

-- | The last day that is listed in the calendar.
--
calendarLastDay :: Calendar -> DateTime
calendarLastDay cal = UTCTime first 0 where (first, _) = calendarRange cal

-- | Is the given date listed in the given calendar.
--
dayInCalendar :: Calendar -> DateTime -> Bool
dayInCalendar cal@(Calendar _ daybitmap) (UTCTime day _)
  | inRange (calendarRange cal) day = daybitmap ! day
  | otherwise                       = False

-- | The days within the given time period that are listed in the calendar.
--
calendarDaysInPeriod :: Calendar
                     -> (DateTime, DateTime)  -- ^ @(start, end)@ datetimes
                     -> [DateTime]
calendarDaysInPeriod (Calendar _ daybitmap)
                     (UTCTime rangeStart _, UTCTime rangeEnd _) =
    [ UTCTime day 0 | day <- [start..end], daybitmap ! day ]
  where
    start = max calStart rangeStart
    end   = min calEnd   rangeEnd
    (calStart, calEnd) = bounds daybitmap

-- | A date change: a number of calendar days later. Days that are not listed
-- in the calendar are skipped.
--
-- For example, for a working day calendar, @calendarDaysLater cal 2@ is the
-- 2nd working day, not including the current day and taking into account
-- weekends and holidays. This works because a working day calendar lists only
-- working days, and omits the weekends and holidays.
--
calendarDaysLater :: Calendar -> Int -> DiffDateTime
calendarDaysLater _   n0 | n0 < 0 = error "calendarDaysLater: negative days are invalid"
calendarDaysLater cal n0 =
    modifyDate $ \day0 ->
      let dayError = dateError ("calendarDaysLater" ++ show (day0,n0)) cal
       in succCalendarDays dayError cal n0 day0

-- | A date change: a number of calendar days previous. Days that are not listed
-- in the calendar are skipped.
--
-- For example, for a working day calendar, @calendarDaysEarlier cal 1@ is the
-- previous working day, taking into account weekends and holidays (as
-- specified by the given calendar).
--
calendarDaysEarlier :: Calendar -> Int -> DiffDateTime
calendarDaysEarlier _   n0 | n0 < 0 = error "calendarDaysEarlier: negative days are invalid"
calendarDaysEarlier cal n0 =
    modifyDate $ \day0 ->
      let dayError = dateError ("calendarDaysEarlier" ++ show (day0,n0)) cal
       in predCalendarDays dayError cal n0 day0

-- | A date change: the first of the subsequent days that is listed in the
-- calendar. That is, the current day is not included.
--
-- This is the same as @calendarDaysLater cal 1@
--
calendarDayNext :: Calendar -> DiffDateTime
calendarDayNext cal =
    modifyDate $ \day0 ->
      let dayError = dateError ("calendarDayNext(" ++ show day0 ++ ")") cal
       in succCalendarDays dayError cal 1 day0

-- | A date change: the first of the current or subsequent days that is a
-- listed in the calendar day. This is the current day if the current day is
-- listed in the calendar.
--
-- This is the same as @calendarDaysLater cal 0@
--
-- Example (for a workday calendar): first working day of the year:
--
-- > inMonth 1 <> onDayOfMonth 1 <> calendarDayFirst cal
--
-- Example (for a workday calendar): first working day in a month's time:
--
-- > monthsLater 1 <> calendarDayFirst cal
--
calendarDayFirst :: Calendar -> DiffDateTime
calendarDayFirst cal =
    modifyDate $ \day0 ->
      let dayError = dateError ("calendarDayFirst(" ++ show day0 ++ ")") cal
       in succCalendarDays dayError cal 0 day0

-- | A date change: the first of the previous days that is listed in the
-- calendar.
--
-- That is, the current day is not included.
--
-- This is the same as @calendarDaysEarlier cal 1@
--
calendarDayPrevious :: Calendar -> DiffDateTime
calendarDayPrevious cal =
    modifyDate $ \day0 ->
      let dayError = dateError ("calendarDayPrevious(" ++ show day0 ++ ")") cal
       in predCalendarDays dayError cal 1 day0

-- internal worker & helper functions:

badDay :: Calendar -> Day -> Bool
badDay (Calendar _ daybitmap) = not . inRange (bounds daybitmap)

dateError :: String -> Calendar -> Day -> a
dateError fname (Calendar name daybitmap) day =
  error $ fname ++ ": " ++ "the date " ++ show day
       ++ " is out of range for the calendar " ++ name
       ++ " " ++ show (bounds daybitmap)

succCalendarDays :: (forall a. Day -> a) -> Calendar -> Int -> Day -> Day
succCalendarDays dayError cal@(Calendar _ daybitmap) = later
  where
    later  _ !day | badDay cal day  = dayError day
    later !0 !day | daybitmap ! day = day
                  | otherwise       = succCalendarDay day
    later !1 !day = succCalendarDay day
    later !n !day = later (n-1) (succCalendarDay day)

    succCalendarDay = go . succ
      where
        go !day | badDay cal day  = dayError day
                | daybitmap ! day = day
                | otherwise       = go (succ day)

predCalendarDays :: (forall a. Day -> a) -> Calendar -> Int -> Day -> Day
predCalendarDays dayError cal@(Calendar _ daybitmap) = earlier
  where
    earlier  _ !day | badDay cal day  = dayError day
    earlier !0 !day | daybitmap ! day = day
                    | otherwise       = predCalendarDay day
    earlier !1 !day = predCalendarDay day
    earlier !n !day = earlier (n-1) (predCalendarDay day)

    predCalendarDay = go . pred
      where
        go !day | badDay cal day  = dayError day
                | daybitmap ! day = day
                | otherwise       = go (pred day)

---------------
-- Deprecated
--

{-# DEPRECATED firstWorkingDay     "Use calendarDayFirst instead"     #-}
{-# DEPRECATED nextWorkingDay      "Use calendarDayNext instead"      #-}
{-# DEPRECATED previousWorkingDay  "Use calendarDayPrevious instead"  #-}
{-# DEPRECATED workingDaysEarlier  "Use calendarDaysEarlier instead"  #-}
{-# DEPRECATED workingDaysLater    "Use calendarDaysLater instead"  #-}

{-# DEPRECATED isWorkingDay        "Use dayInCalendar instead"        #-}
{-# DEPRECATED workingDaysInPeriod "Use calendarDaysInPeriod instead" #-}
{-# DEPRECATED getCalendar         "Use getBusinessDayCalendar instead" #-}

firstWorkingDay     = calendarDayFirst
nextWorkingDay      = calendarDayNext
previousWorkingDay  = calendarDayPrevious
workingDaysEarlier  = calendarDaysEarlier
workingDaysLater    = calendarDaysLater
isWorkingDay        = dayInCalendar
workingDaysInPeriod = calendarDaysInPeriod
getCalendar         = getBusinessDayCalendar

---------------------------
-- Constructing calendars
--

newCalendar :: String -> [(DateTime, DayType)] -> Calendar
newCalendar name days = Calendar name daybitmap
  where
    daybitmap :: UArray Day Bool
    daybitmap = array (lbound, ubound)
                      [ (day, isBusinessDay daytype)
                      | (UTCTime day _, daytype) <- days ]
    UTCTime lbound _ = minimum [ day | (day,_) <- days]
    UTCTime ubound _ = maximum [ day | (day,_) <- days]


-- | Day types
data DayType = BusinessDay  -- ^ a normal working weekday
             | Holiday      -- ^ a weekday that is a non-working day, e.g. holiday
             | Weekend      -- ^ a weekend
  deriving (Show, Eq)

isBusinessDay :: DayType -> Bool
isBusinessDay BusinessDay = True
isBusinessDay _           = False

-- * Calendar instances
-- | Get a given business day calendar. For now, calendars are manually
-- populated here. Ideally this will done using a service and parsed here.
getBusinessDayCalendar :: String -> Calendar
getBusinessDayCalendar "EEX Power" = eexPower
getBusinessDayCalendar name        = error $ "getCalendar: unknown calendar " ++ show name



eexPower :: Calendar
eexPower = newCalendar "EEX Power"
  [(date 2010 12 25, Weekend)
  ,(date 2010 12 26, Weekend)
  ,(date 2010 12 27, BusinessDay)
  ,(date 2010 12 28, BusinessDay)
  ,(date 2010 12 29, BusinessDay)
  ,(date 2010 12 30, BusinessDay)
  ,(date 2010 12 31, BusinessDay)
  ,(date 2011 01 01, Weekend)
  ,(date 2011 01 02, Weekend)
  ,(date 2011 01 03, BusinessDay)
  ,(date 2011 01 04, BusinessDay)
  ,(date 2011 01 05, BusinessDay)
  ,(date 2011 01 06, BusinessDay)
  ,(date 2011 01 07, BusinessDay)
  ,(date 2011 01 08, Weekend)
  ,(date 2011 01 09, Weekend)
  ,(date 2011 01 10, BusinessDay)
  ,(date 2011 01 11, BusinessDay)
  ,(date 2011 01 12, BusinessDay)
  ,(date 2011 01 13, BusinessDay)
  ,(date 2011 01 14, BusinessDay)
  ,(date 2011 01 15, Weekend)
  ,(date 2011 01 16, Weekend)
  ,(date 2011 01 17, BusinessDay)
  ,(date 2011 01 18, BusinessDay)
  ,(date 2011 01 19, BusinessDay)
  ,(date 2011 01 20, BusinessDay)
  ,(date 2011 01 21, BusinessDay)
  ,(date 2011 01 22, Weekend)
  ,(date 2011 01 23, Weekend)
  ,(date 2011 01 24, BusinessDay)
  ,(date 2011 01 25, BusinessDay)
  ,(date 2011 01 26, BusinessDay)
  ,(date 2011 01 27, BusinessDay)
  ,(date 2011 01 28, BusinessDay)
  ,(date 2011 01 29, Weekend)
  ,(date 2011 01 30, Weekend)
  ,(date 2011 01 31, BusinessDay)
  ,(date 2011 02 01, BusinessDay)
  ,(date 2011 02 02, BusinessDay)
  ,(date 2011 02 03, BusinessDay)
  ,(date 2011 02 04, BusinessDay)
  ,(date 2011 02 05, Weekend)
  ,(date 2011 02 06, Weekend)
  ,(date 2011 02 07, BusinessDay)
  ,(date 2011 02 08, BusinessDay)
  ,(date 2011 02 09, BusinessDay)
  ,(date 2011 02 10, BusinessDay)
  ,(date 2011 02 11, BusinessDay)
  ,(date 2011 02 12, Weekend)
  ,(date 2011 02 13, Weekend)
  ,(date 2011 02 14, BusinessDay)
  ,(date 2011 02 15, BusinessDay)
  ,(date 2011 02 16, BusinessDay)
  ,(date 2011 02 17, BusinessDay)
  ,(date 2011 02 18, BusinessDay)
  ,(date 2011 02 19, Weekend)
  ,(date 2011 02 20, Weekend)
  ,(date 2011 02 21, BusinessDay)
  ,(date 2011 02 22, BusinessDay)
  ,(date 2011 02 23, BusinessDay)
  ,(date 2011 02 24, BusinessDay)
  ,(date 2011 02 25, BusinessDay)
  ,(date 2011 02 26, Weekend)
  ,(date 2011 02 27, Weekend)
  ,(date 2011 02 28, BusinessDay)
  ,(date 2011 03 01, BusinessDay)
  ,(date 2011 03 02, BusinessDay)
  ,(date 2011 03 03, BusinessDay)
  ,(date 2011 03 04, BusinessDay)
  ,(date 2011 03 05, Weekend)
  ,(date 2011 03 06, Weekend)
  ,(date 2011 03 07, BusinessDay)
  ,(date 2011 03 08, BusinessDay)
  ,(date 2011 03 09, BusinessDay)
  ,(date 2011 03 10, BusinessDay)
  ,(date 2011 03 11, BusinessDay)
  ,(date 2011 03 12, Weekend)
  ,(date 2011 03 13, Weekend)
  ,(date 2011 03 14, BusinessDay)
  ,(date 2011 03 15, BusinessDay)
  ,(date 2011 03 16, BusinessDay)
  ,(date 2011 03 17, BusinessDay)
  ,(date 2011 03 18, BusinessDay)
  ,(date 2011 03 19, Weekend)
  ,(date 2011 03 20, Weekend)
  ,(date 2011 03 21, BusinessDay)
  ,(date 2011 03 22, BusinessDay)
  ,(date 2011 03 23, BusinessDay)
  ,(date 2011 03 24, BusinessDay)
  ,(date 2011 03 25, BusinessDay)
  ,(date 2011 03 26, Weekend)
  ,(date 2011 03 27, Weekend)
  ,(date 2011 03 28, BusinessDay)
  ,(date 2011 03 29, BusinessDay)
  ,(date 2011 03 30, BusinessDay)
  ,(date 2011 03 31, BusinessDay)
  ,(date 2011 04 01, BusinessDay)
  ,(date 2011 04 02, Weekend)
  ,(date 2011 04 03, Weekend)
  ,(date 2011 04 04, BusinessDay)
  ,(date 2011 04 05, BusinessDay)
  ,(date 2011 04 06, BusinessDay)
  ,(date 2011 04 07, BusinessDay)
  ,(date 2011 04 08, BusinessDay)
  ,(date 2011 04 09, Weekend)
  ,(date 2011 04 10, Weekend)
  ,(date 2011 04 11, BusinessDay)
  ,(date 2011 04 12, BusinessDay)
  ,(date 2011 04 13, BusinessDay)
  ,(date 2011 04 14, BusinessDay)
  ,(date 2011 04 15, BusinessDay)
  ,(date 2011 04 16, Weekend)
  ,(date 2011 04 17, Weekend)
  ,(date 2011 04 18, BusinessDay)
  ,(date 2011 04 19, BusinessDay)
  ,(date 2011 04 20, BusinessDay)
  ,(date 2011 04 21, BusinessDay)
  ,(date 2011 04 22, Holiday)
  ,(date 2011 04 23, Weekend)
  ,(date 2011 04 24, Weekend)
  ,(date 2011 04 25, Holiday)
  ,(date 2011 04 26, BusinessDay)
  ,(date 2011 04 27, BusinessDay)
  ,(date 2011 04 28, BusinessDay)
  ,(date 2011 04 29, BusinessDay)
  ,(date 2011 04 30, Weekend)
  ,(date 2011 05 01, Weekend)
  ,(date 2011 05 02, BusinessDay)
  ,(date 2011 05 03, BusinessDay)
  ,(date 2011 05 04, BusinessDay)
  ,(date 2011 05 05, BusinessDay)
  ,(date 2011 05 06, BusinessDay)
  ,(date 2011 05 07, Weekend)
  ,(date 2011 05 08, Weekend)
  ,(date 2011 05 09, BusinessDay)
  ,(date 2011 05 10, BusinessDay)
  ,(date 2011 05 11, BusinessDay)
  ,(date 2011 05 12, BusinessDay)
  ,(date 2011 05 13, BusinessDay)
  ,(date 2011 05 14, Weekend)
  ,(date 2011 05 15, Weekend)
  ,(date 2011 05 16, BusinessDay)
  ,(date 2011 05 17, BusinessDay)
  ,(date 2011 05 18, BusinessDay)
  ,(date 2011 05 19, BusinessDay)
  ,(date 2011 05 20, BusinessDay)
  ,(date 2011 05 21, Weekend)
  ,(date 2011 05 22, Weekend)
  ,(date 2011 05 23, BusinessDay)
  ,(date 2011 05 24, BusinessDay)
  ,(date 2011 05 25, BusinessDay)
  ,(date 2011 05 26, BusinessDay)
  ,(date 2011 05 27, BusinessDay)
  ,(date 2011 05 28, Weekend)
  ,(date 2011 05 29, Weekend)
  ,(date 2011 05 30, BusinessDay)
  ,(date 2011 05 31, BusinessDay)
  ,(date 2011 06 01, BusinessDay)
  ,(date 2011 06 02, Holiday)
  ,(date 2011 06 03, BusinessDay)
  ,(date 2011 06 04, Weekend)
  ,(date 2011 06 05, Weekend)
  ,(date 2011 06 06, BusinessDay)
  ,(date 2011 06 07, BusinessDay)
  ,(date 2011 06 08, BusinessDay)
  ,(date 2011 06 09, BusinessDay)
  ,(date 2011 06 10, BusinessDay)
  ,(date 2011 06 11, Weekend)
  ,(date 2011 06 12, Weekend)
  ,(date 2011 06 13, Holiday)
  ,(date 2011 06 14, BusinessDay)
  ,(date 2011 06 15, BusinessDay)
  ,(date 2011 06 16, BusinessDay)
  ,(date 2011 06 17, BusinessDay)
  ,(date 2011 06 18, Weekend)
  ,(date 2011 06 19, Weekend)
  ,(date 2011 06 20, BusinessDay)
  ,(date 2011 06 21, BusinessDay)
  ,(date 2011 06 22, BusinessDay)
  ,(date 2011 06 23, BusinessDay)
  ,(date 2011 06 24, BusinessDay)
  ,(date 2011 06 25, Weekend)
  ,(date 2011 06 26, Weekend)
  ,(date 2011 06 27, BusinessDay)
  ,(date 2011 06 28, BusinessDay)
  ,(date 2011 06 29, BusinessDay)
  ,(date 2011 06 30, BusinessDay)
  ,(date 2011 07 01, BusinessDay)
  ,(date 2011 07 02, Weekend)
  ,(date 2011 07 03, Weekend)
  ,(date 2011 07 04, BusinessDay)
  ,(date 2011 07 05, BusinessDay)
  ,(date 2011 07 06, BusinessDay)
  ,(date 2011 07 07, BusinessDay)
  ,(date 2011 07 08, BusinessDay)
  ,(date 2011 07 09, Weekend)
  ,(date 2011 07 10, Weekend)
  ,(date 2011 07 11, BusinessDay)
  ,(date 2011 07 12, BusinessDay)
  ,(date 2011 07 13, BusinessDay)
  ,(date 2011 07 14, BusinessDay)
  ,(date 2011 07 15, BusinessDay)
  ,(date 2011 07 16, Weekend)
  ,(date 2011 07 17, Weekend)
  ,(date 2011 07 18, BusinessDay)
  ,(date 2011 07 19, BusinessDay)
  ,(date 2011 07 20, BusinessDay)
  ,(date 2011 07 21, BusinessDay)
  ,(date 2011 07 22, BusinessDay)
  ,(date 2011 07 23, Weekend)
  ,(date 2011 07 24, Weekend)
  ,(date 2011 07 25, BusinessDay)
  ,(date 2011 07 26, BusinessDay)
  ,(date 2011 07 27, BusinessDay)
  ,(date 2011 07 28, BusinessDay)
  ,(date 2011 07 29, BusinessDay)
  ,(date 2011 07 30, Weekend)
  ,(date 2011 07 31, Weekend)
  ,(date 2011 08 01, BusinessDay)
  ,(date 2011 08 02, BusinessDay)
  ,(date 2011 08 03, BusinessDay)
  ,(date 2011 08 04, BusinessDay)
  ,(date 2011 08 05, BusinessDay)
  ,(date 2011 08 06, Weekend)
  ,(date 2011 08 07, Weekend)
  ,(date 2011 08 08, BusinessDay)
  ,(date 2011 08 09, BusinessDay)
  ,(date 2011 08 10, BusinessDay)
  ,(date 2011 08 11, BusinessDay)
  ,(date 2011 08 12, BusinessDay)
  ,(date 2011 08 13, Weekend)
  ,(date 2011 08 14, Weekend)
  ,(date 2011 08 15, BusinessDay)
  ,(date 2011 08 16, BusinessDay)
  ,(date 2011 08 17, BusinessDay)
  ,(date 2011 08 18, BusinessDay)
  ,(date 2011 08 19, BusinessDay)
  ,(date 2011 08 20, Weekend)
  ,(date 2011 08 21, Weekend)
  ,(date 2011 08 22, BusinessDay)
  ,(date 2011 08 23, BusinessDay)
  ,(date 2011 08 24, BusinessDay)
  ,(date 2011 08 25, BusinessDay)
  ,(date 2011 08 26, BusinessDay)
  ,(date 2011 08 27, Weekend)
  ,(date 2011 08 28, Weekend)
  ,(date 2011 08 29, BusinessDay)
  ,(date 2011 08 30, BusinessDay)
  ,(date 2011 08 31, BusinessDay)
  ,(date 2011 09 01, BusinessDay)
  ,(date 2011 09 02, BusinessDay)
  ,(date 2011 09 03, Weekend)
  ,(date 2011 09 04, Weekend)
  ,(date 2011 09 05, BusinessDay)
  ,(date 2011 09 06, BusinessDay)
  ,(date 2011 09 07, BusinessDay)
  ,(date 2011 09 08, BusinessDay)
  ,(date 2011 09 09, BusinessDay)
  ,(date 2011 09 10, Weekend)
  ,(date 2011 09 11, Weekend)
  ,(date 2011 09 12, BusinessDay)
  ,(date 2011 09 13, BusinessDay)
  ,(date 2011 09 14, BusinessDay)
  ,(date 2011 09 15, BusinessDay)
  ,(date 2011 09 16, BusinessDay)
  ,(date 2011 09 17, Weekend)
  ,(date 2011 09 18, Weekend)
  ,(date 2011 09 19, BusinessDay)
  ,(date 2011 09 20, BusinessDay)
  ,(date 2011 09 21, BusinessDay)
  ,(date 2011 09 22, BusinessDay)
  ,(date 2011 09 23, BusinessDay)
  ,(date 2011 09 24, Weekend)
  ,(date 2011 09 25, Weekend)
  ,(date 2011 09 26, BusinessDay)
  ,(date 2011 09 27, BusinessDay)
  ,(date 2011 09 28, BusinessDay)
  ,(date 2011 09 29, BusinessDay)
  ,(date 2011 09 30, BusinessDay)
  ,(date 2011 10 01, Weekend)
  ,(date 2011 10 02, Weekend)
  ,(date 2011 10 03, Holiday)
  ,(date 2011 10 04, BusinessDay)
  ,(date 2011 10 05, BusinessDay)
  ,(date 2011 10 06, BusinessDay)
  ,(date 2011 10 07, BusinessDay)
  ,(date 2011 10 08, Weekend)
  ,(date 2011 10 09, Weekend)
  ,(date 2011 10 10, BusinessDay)
  ,(date 2011 10 11, BusinessDay)
  ,(date 2011 10 12, BusinessDay)
  ,(date 2011 10 13, BusinessDay)
  ,(date 2011 10 14, BusinessDay)
  ,(date 2011 10 15, Weekend)
  ,(date 2011 10 16, Weekend)
  ,(date 2011 10 17, BusinessDay)
  ,(date 2011 10 18, BusinessDay)
  ,(date 2011 10 19, BusinessDay)
  ,(date 2011 10 20, BusinessDay)
  ,(date 2011 10 21, BusinessDay)
  ,(date 2011 10 22, Weekend)
  ,(date 2011 10 23, Weekend)
  ,(date 2011 10 24, BusinessDay)
  ,(date 2011 10 25, BusinessDay)
  ,(date 2011 10 26, BusinessDay)
  ,(date 2011 10 27, BusinessDay)
  ,(date 2011 10 28, BusinessDay)
  ,(date 2011 10 29, Weekend)
  ,(date 2011 10 30, Weekend)
  ,(date 2011 10 31, BusinessDay)
  ,(date 2011 11 01, BusinessDay)
  ,(date 2011 11 02, BusinessDay)
  ,(date 2011 11 03, BusinessDay)
  ,(date 2011 11 04, BusinessDay)
  ,(date 2011 11 05, Weekend)
  ,(date 2011 11 06, Weekend)
  ,(date 2011 11 07, BusinessDay)
  ,(date 2011 11 08, BusinessDay)
  ,(date 2011 11 09, BusinessDay)
  ,(date 2011 11 10, BusinessDay)
  ,(date 2011 11 11, BusinessDay)
  ,(date 2011 11 12, Weekend)
  ,(date 2011 11 13, Weekend)
  ,(date 2011 11 14, BusinessDay)
  ,(date 2011 11 15, BusinessDay)
  ,(date 2011 11 16, BusinessDay)
  ,(date 2011 11 17, BusinessDay)
  ,(date 2011 11 18, BusinessDay)
  ,(date 2011 11 19, Weekend)
  ,(date 2011 11 20, Weekend)
  ,(date 2011 11 21, BusinessDay)
  ,(date 2011 11 22, BusinessDay)
  ,(date 2011 11 23, BusinessDay)
  ,(date 2011 11 24, BusinessDay)
  ,(date 2011 11 25, BusinessDay)
  ,(date 2011 11 26, Weekend)
  ,(date 2011 11 27, Weekend)
  ,(date 2011 11 28, BusinessDay)
  ,(date 2011 11 29, BusinessDay)
  ,(date 2011 11 30, BusinessDay)
  ,(date 2011 12 01, BusinessDay)
  ,(date 2011 12 02, BusinessDay)
  ,(date 2011 12 03, Weekend)
  ,(date 2011 12 04, Weekend)
  ,(date 2011 12 05, BusinessDay)
  ,(date 2011 12 06, BusinessDay)
  ,(date 2011 12 07, BusinessDay)
  ,(date 2011 12 08, BusinessDay)
  ,(date 2011 12 09, BusinessDay)
  ,(date 2011 12 10, Weekend)
  ,(date 2011 12 11, Weekend)
  ,(date 2011 12 12, BusinessDay)
  ,(date 2011 12 13, BusinessDay)
  ,(date 2011 12 14, BusinessDay)
  ,(date 2011 12 15, BusinessDay)
  ,(date 2011 12 16, BusinessDay)
  ,(date 2011 12 17, Weekend)
  ,(date 2011 12 18, Weekend)
  ,(date 2011 12 19, BusinessDay)
  ,(date 2011 12 20, BusinessDay)
  ,(date 2011 12 21, BusinessDay)
  ,(date 2011 12 22, BusinessDay)
  ,(date 2011 12 23, BusinessDay)
  ,(date 2011 12 24, Weekend)
  ,(date 2011 12 25, Weekend)
  ,(date 2011 12 26, Holiday)
  ,(date 2011 12 27, BusinessDay)
  ,(date 2011 12 28, BusinessDay)
  ,(date 2011 12 29, BusinessDay)
  ,(date 2011 12 30, BusinessDay)
  ,(date 2011 12 31, Weekend)
  ]
