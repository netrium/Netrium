-- |Netrium is Copyright Netrium Consulting Ltd, 2009-2012, and files herein are licensed
-- |under the Affero General Public License version 3, the text of which can
-- |be found in agpl.txt, or any later version of the AGPL, unless otherwise
-- |noted. 
--
-- Module for calendars
module Calendar where

import Prelude hiding (and, or, min, max, abs, negate, not, read, until)
import Contract
import Common

import Data.List
import Data.Time
import Data.Monoid
import Data.Maybe

-- *Calendar types
-- |Day types
type DayType = String

-- |A calendar period
type CalendarPeriod = (DateTime,       -- start datetime
                       DateTime,       -- end datetime
                       DayType)        -- D for week day, E for week-end, combined with B for business day, N for non-working day

-- |Define a calendar as a set of calendar days and a name
type Calendar = (String,            -- name of the calendar, e.g. EEX calendar
                 [CalendarPeriod])

-- |Define a calendar offset time as an offset to apply according to a given calendar at a given time
--
-- For instance (\"EEX Power\", -3, \"DB\", (15, 00)) will apply an offset of -3 business days according to the EEX Power calendar and return this date at 3PM
type CalendarOffsetTime = (String,           -- name of the calendar
                           Int,              -- offset to apply
                           DayType,          -- day type for the offset
                           (Int, Int))       -- time of day

-- *Helper functions
-- |Returns the number of days of a given type (e.g. business days) between 2 dates for a given calendar
numTypeDays :: Calendar     -- ^Reference calendar
            -> DateTime     -- ^Start datetime for search range
            -> DateTime     -- ^End datetime for search range
            -> String       -- ^Type of day
            -> Int
numTypeDays (calName, []) startDateTime endDateTime dayType = 0
numTypeDays (calName, ((sd, ed, dt):xs)) startDateTime endDateTime dayType =
  if ((dayInRange (sd, ed, dt) startDateTime endDateTime) && (dt == dayType))
      then (1 + (numTypeDays(calName, xs) startDateTime endDateTime dayType))
      else (numTypeDays(calName, xs) startDateTime endDateTime dayType)

-- not needed
--getStartDate :: CalendarPeriod -> DateTime
--getStartDate (sd, _, _) = sd

-- |Returns true if a given calendar period is within a given date range
dayInRange :: CalendarPeriod
           -> DateTime        -- ^Start datetime for search range
           -> DateTime        -- ^End datetime for search range
           -> Bool
dayInRange (sd, ed, dt) startDateTime endDateTime = (sd >= startDateTime) && (ed < endDateTime)

-- |Returns a date which is a given number of days of a given type earlier according to a given calendar
-- or Nothing if there is no such date
calDaysOffset :: Calendar
              -> DateTime
              -> String          -- ^Type of day
              -> Int             -- ^Number of days to offset
              -> DateTime
calDaysOffset cal iDate dayType numDays = rsd
   where
     --(rsd, _, _) = fromJust (find p cd)
     --p (sd, _, _) = (sd == iDate)
     (rsd, _, _) = if (null cd) then (error "No date matching criteria") else (if (direction == 'L') then (head (lastn (abs numDays) cd)) else (last (take numDays cd)))
     (calName, cd) = getSubCalendarByDayType (getSubCalendarByDate cal iDate direction "") dayType ""
     direction = if (numDays == 0) then 'E' else (case (numDays < 0) of
                                                        True -> 'L'
                                                        False -> 'G')

-- |Get a sub-calendar from a calendar and a given day type (e.g. EEX business day calendar)
getSubCalendarByDayType :: Calendar
                        -> String     -- ^Type of day
                        -> String     -- ^Name of sub-calendar
                        -> Calendar
getSubCalendarByDayType (_, cp) dayType subCalName = ("", (filter p cp))
   where p (sd, ed, dt) = (dt == dayType)

-- |Get a sub-calendar from a calendar and a given reference date (e.g. all days prior to 30/04/2011)
getSubCalendarByDate :: Calendar
                        -> DateTime   -- ^Reference date
                        -> Char       -- ^G for greater than, L for lower than, E for equal
                        -> String     -- ^Name of sub-calendar
                        -> Calendar
getSubCalendarByDate (_, cp) refdt op subCalName = ("", (filter p cp))
   where p (sd, ed, dt) = case op of
                               'G' -> (sd > refdt)
                               'L' -> (sd < refdt)
                               'E' -> (sd == refdt)

-- |Returns a date n business days prior to a given date according to a given calendar
businessDaysOffset :: String
                    -> DateTime
                    -> Int
                    -> DateTime
businessDaysOffset calName dt n = calDaysOffset (getCalendar calName) dt "DB" n

-- |Returns a date n calendar days prior to a given date according to a given calendar
--
-- TODO: make this work!
calendarDaysOffset :: String
                    -> DateTime
                    -> Int
                    -> DateTime
calendarDaysOffset calName dt n = calDaysOffset (getCalendar calName) dt "" n

-- *Calendar instances
-- |Get a given calendar. For now, calendars are manually populated here.
-- Ideally this will done using a service and parsed here.
getCalendar :: String -> Calendar
getCalendar "EEX Power" = ("EEX Power", [(datetime 2010 12 25 00 00, datetime 2011 1 1 23 59, "EN"), 
 (datetime 2010 12 26 00 00, datetime 2011 1 1 23 59, "EN"), 
 (datetime 2010 12 27 00 00, datetime 2011 1 1 23 59, "DB"), 
 (datetime 2010 12 28 00 00, datetime 2011 1 1 23 59, "DB"), 
 (datetime 2010 12 29 00 00, datetime 2011 1 1 23 59, "DB"), 
 (datetime 2010 12 30 00 00, datetime 2011 1 1 23 59, "DB"), 
 (datetime 2010 12 31 00 00, datetime 2011 1 1 23 59, "DB"), 
 (datetime 2011 1 1 00 00, datetime 2011 1 1 23 59, "EN"), 
 (datetime 2011 1 2 00 00, datetime 2011 1 2 23 59, "EN"), 
 (datetime 2011 1 3 00 00, datetime 2011 1 3 23 59, "DB"), 
 (datetime 2011 1 4 00 00, datetime 2011 1 4 23 59, "DB"), 
 (datetime 2011 1 5 00 00, datetime 2011 1 5 23 59, "DB"), 
 (datetime 2011 1 6 00 00, datetime 2011 1 6 23 59, "DB"), 
 (datetime 2011 1 7 00 00, datetime 2011 1 7 23 59, "DB"), 
 (datetime 2011 1 8 00 00, datetime 2011 1 8 23 59, "EN"), 
 (datetime 2011 1 9 00 00, datetime 2011 1 9 23 59, "EN"), 
 (datetime 2011 1 10 00 00, datetime 2011 1 10 23 59, "DB"), 
 (datetime 2011 1 11 00 00, datetime 2011 1 11 23 59, "DB"), 
 (datetime 2011 1 12 00 00, datetime 2011 1 12 23 59, "DB"), 
 (datetime 2011 1 13 00 00, datetime 2011 1 13 23 59, "DB"), 
 (datetime 2011 1 14 00 00, datetime 2011 1 14 23 59, "DB"), 
 (datetime 2011 1 15 00 00, datetime 2011 1 15 23 59, "EN"), 
 (datetime 2011 1 16 00 00, datetime 2011 1 16 23 59, "EN"), 
 (datetime 2011 1 17 00 00, datetime 2011 1 17 23 59, "DB"), 
 (datetime 2011 1 18 00 00, datetime 2011 1 18 23 59, "DB"), 
 (datetime 2011 1 19 00 00, datetime 2011 1 19 23 59, "DB"), 
 (datetime 2011 1 20 00 00, datetime 2011 1 20 23 59, "DB"), 
 (datetime 2011 1 21 00 00, datetime 2011 1 21 23 59, "DB"), 
 (datetime 2011 1 22 00 00, datetime 2011 1 22 23 59, "EN"), 
 (datetime 2011 1 23 00 00, datetime 2011 1 23 23 59, "EN"), 
 (datetime 2011 1 24 00 00, datetime 2011 1 24 23 59, "DB"), 
 (datetime 2011 1 25 00 00, datetime 2011 1 25 23 59, "DB"), 
 (datetime 2011 1 26 00 00, datetime 2011 1 26 23 59, "DB"), 
 (datetime 2011 1 27 00 00, datetime 2011 1 27 23 59, "DB"), 
 (datetime 2011 1 28 00 00, datetime 2011 1 28 23 59, "DB"), 
 (datetime 2011 1 29 00 00, datetime 2011 1 29 23 59, "EN"), 
 (datetime 2011 1 30 00 00, datetime 2011 1 30 23 59, "EN"), 
 (datetime 2011 1 31 00 00, datetime 2011 1 31 23 59, "DB"), 
 (datetime 2011 2 1 00 00, datetime 2011 2 1 23 59, "DB"), 
 (datetime 2011 2 2 00 00, datetime 2011 2 2 23 59, "DB"), 
 (datetime 2011 2 3 00 00, datetime 2011 2 3 23 59, "DB"), 
 (datetime 2011 2 4 00 00, datetime 2011 2 4 23 59, "DB"), 
 (datetime 2011 2 5 00 00, datetime 2011 2 5 23 59, "EN"), 
 (datetime 2011 2 6 00 00, datetime 2011 2 6 23 59, "EN"), 
 (datetime 2011 2 7 00 00, datetime 2011 2 7 23 59, "DB"), 
 (datetime 2011 2 8 00 00, datetime 2011 2 8 23 59, "DB"), 
 (datetime 2011 2 9 00 00, datetime 2011 2 9 23 59, "DB"), 
 (datetime 2011 2 10 00 00, datetime 2011 2 10 23 59, "DB"), 
 (datetime 2011 2 11 00 00, datetime 2011 2 11 23 59, "DB"), 
 (datetime 2011 2 12 00 00, datetime 2011 2 12 23 59, "EN"), 
 (datetime 2011 2 13 00 00, datetime 2011 2 13 23 59, "EN"), 
 (datetime 2011 2 14 00 00, datetime 2011 2 14 23 59, "DB"), 
 (datetime 2011 2 15 00 00, datetime 2011 2 15 23 59, "DB"), 
 (datetime 2011 2 16 00 00, datetime 2011 2 16 23 59, "DB"), 
 (datetime 2011 2 17 00 00, datetime 2011 2 17 23 59, "DB"), 
 (datetime 2011 2 18 00 00, datetime 2011 2 18 23 59, "DB"), 
 (datetime 2011 2 19 00 00, datetime 2011 2 19 23 59, "EN"), 
 (datetime 2011 2 20 00 00, datetime 2011 2 20 23 59, "EN"), 
 (datetime 2011 2 21 00 00, datetime 2011 2 21 23 59, "DB"), 
 (datetime 2011 2 22 00 00, datetime 2011 2 22 23 59, "DB"), 
 (datetime 2011 2 23 00 00, datetime 2011 2 23 23 59, "DB"), 
 (datetime 2011 2 24 00 00, datetime 2011 2 24 23 59, "DB"), 
 (datetime 2011 2 25 00 00, datetime 2011 2 25 23 59, "DB"), 
 (datetime 2011 2 26 00 00, datetime 2011 2 26 23 59, "EN"), 
 (datetime 2011 2 27 00 00, datetime 2011 2 27 23 59, "EN"), 
 (datetime 2011 2 28 00 00, datetime 2011 2 28 23 59, "DB"), 
 (datetime 2011 3 1 00 00, datetime 2011 3 1 23 59, "DB"), 
 (datetime 2011 3 2 00 00, datetime 2011 3 2 23 59, "DB"), 
 (datetime 2011 3 3 00 00, datetime 2011 3 3 23 59, "DB"), 
 (datetime 2011 3 4 00 00, datetime 2011 3 4 23 59, "DB"), 
 (datetime 2011 3 5 00 00, datetime 2011 3 5 23 59, "EN"), 
 (datetime 2011 3 6 00 00, datetime 2011 3 6 23 59, "EN"), 
 (datetime 2011 3 7 00 00, datetime 2011 3 7 23 59, "DB"), 
 (datetime 2011 3 8 00 00, datetime 2011 3 8 23 59, "DB"), 
 (datetime 2011 3 9 00 00, datetime 2011 3 9 23 59, "DB"), 
 (datetime 2011 3 10 00 00, datetime 2011 3 10 23 59, "DB"), 
 (datetime 2011 3 11 00 00, datetime 2011 3 11 23 59, "DB"), 
 (datetime 2011 3 12 00 00, datetime 2011 3 12 23 59, "EN"), 
 (datetime 2011 3 13 00 00, datetime 2011 3 13 23 59, "EN"), 
 (datetime 2011 3 14 00 00, datetime 2011 3 14 23 59, "DB"), 
 (datetime 2011 3 15 00 00, datetime 2011 3 15 23 59, "DB"), 
 (datetime 2011 3 16 00 00, datetime 2011 3 16 23 59, "DB"), 
 (datetime 2011 3 17 00 00, datetime 2011 3 17 23 59, "DB"), 
 (datetime 2011 3 18 00 00, datetime 2011 3 18 23 59, "DB"), 
 (datetime 2011 3 19 00 00, datetime 2011 3 19 23 59, "EN"), 
 (datetime 2011 3 20 00 00, datetime 2011 3 20 23 59, "EN"), 
 (datetime 2011 3 21 00 00, datetime 2011 3 21 23 59, "DB"), 
 (datetime 2011 3 22 00 00, datetime 2011 3 22 23 59, "DB"), 
 (datetime 2011 3 23 00 00, datetime 2011 3 23 23 59, "DB"), 
 (datetime 2011 3 24 00 00, datetime 2011 3 24 23 59, "DB"), 
 (datetime 2011 3 25 00 00, datetime 2011 3 25 23 59, "DB"), 
 (datetime 2011 3 26 00 00, datetime 2011 3 26 23 59, "EN"), 
 (datetime 2011 3 27 00 00, datetime 2011 3 27 23 59, "EN"), 
 (datetime 2011 3 28 00 00, datetime 2011 3 28 23 59, "DB"), 
 (datetime 2011 3 29 00 00, datetime 2011 3 29 23 59, "DB"), 
 (datetime 2011 3 30 00 00, datetime 2011 3 30 23 59, "DB"), 
 (datetime 2011 3 31 00 00, datetime 2011 3 31 23 59, "DB"), 
 (datetime 2011 4 1 00 00, datetime 2011 4 1 23 59, "DB"), 
 (datetime 2011 4 2 00 00, datetime 2011 4 2 23 59, "EN"), 
 (datetime 2011 4 3 00 00, datetime 2011 4 3 23 59, "EN"), 
 (datetime 2011 4 4 00 00, datetime 2011 4 4 23 59, "DB"), 
 (datetime 2011 4 5 00 00, datetime 2011 4 5 23 59, "DB"), 
 (datetime 2011 4 6 00 00, datetime 2011 4 6 23 59, "DB"), 
 (datetime 2011 4 7 00 00, datetime 2011 4 7 23 59, "DB"), 
 (datetime 2011 4 8 00 00, datetime 2011 4 8 23 59, "DB"), 
 (datetime 2011 4 9 00 00, datetime 2011 4 9 23 59, "EN"), 
 (datetime 2011 4 10 00 00, datetime 2011 4 10 23 59, "EN"), 
 (datetime 2011 4 11 00 00, datetime 2011 4 11 23 59, "DB"), 
 (datetime 2011 4 12 00 00, datetime 2011 4 12 23 59, "DB"), 
 (datetime 2011 4 13 00 00, datetime 2011 4 13 23 59, "DB"), 
 (datetime 2011 4 14 00 00, datetime 2011 4 14 23 59, "DB"), 
 (datetime 2011 4 15 00 00, datetime 2011 4 15 23 59, "DB"), 
 (datetime 2011 4 16 00 00, datetime 2011 4 16 23 59, "EN"), 
 (datetime 2011 4 17 00 00, datetime 2011 4 17 23 59, "EN"), 
 (datetime 2011 4 18 00 00, datetime 2011 4 18 23 59, "DB"), 
 (datetime 2011 4 19 00 00, datetime 2011 4 19 23 59, "DB"), 
 (datetime 2011 4 20 00 00, datetime 2011 4 20 23 59, "DB"), 
 (datetime 2011 4 21 00 00, datetime 2011 4 21 23 59, "DB"), 
 (datetime 2011 4 22 00 00, datetime 2011 4 22 23 59, "DN"), 
 (datetime 2011 4 23 00 00, datetime 2011 4 23 23 59, "EN"), 
 (datetime 2011 4 24 00 00, datetime 2011 4 24 23 59, "EN"), 
 (datetime 2011 4 25 00 00, datetime 2011 4 25 23 59, "DN"), 
 (datetime 2011 4 26 00 00, datetime 2011 4 26 23 59, "DB"), 
 (datetime 2011 4 27 00 00, datetime 2011 4 27 23 59, "DB"), 
 (datetime 2011 4 28 00 00, datetime 2011 4 28 23 59, "DB"), 
 (datetime 2011 4 29 00 00, datetime 2011 4 29 23 59, "DB"), 
 (datetime 2011 4 30 00 00, datetime 2011 4 30 23 59, "EN"), 
 (datetime 2011 5 1 00 00, datetime 2011 5 1 23 59, "EN"), 
 (datetime 2011 5 2 00 00, datetime 2011 5 2 23 59, "DB"), 
 (datetime 2011 5 3 00 00, datetime 2011 5 3 23 59, "DB"), 
 (datetime 2011 5 4 00 00, datetime 2011 5 4 23 59, "DB"), 
 (datetime 2011 5 5 00 00, datetime 2011 5 5 23 59, "DB"), 
 (datetime 2011 5 6 00 00, datetime 2011 5 6 23 59, "DB"), 
 (datetime 2011 5 7 00 00, datetime 2011 5 7 23 59, "EN"), 
 (datetime 2011 5 8 00 00, datetime 2011 5 8 23 59, "EN"), 
 (datetime 2011 5 9 00 00, datetime 2011 5 9 23 59, "DB"), 
 (datetime 2011 5 10 00 00, datetime 2011 5 10 23 59, "DB"), 
 (datetime 2011 5 11 00 00, datetime 2011 5 11 23 59, "DB"), 
 (datetime 2011 5 12 00 00, datetime 2011 5 12 23 59, "DB"), 
 (datetime 2011 5 13 00 00, datetime 2011 5 13 23 59, "DB"), 
 (datetime 2011 5 14 00 00, datetime 2011 5 14 23 59, "EN"), 
 (datetime 2011 5 15 00 00, datetime 2011 5 15 23 59, "EN"), 
 (datetime 2011 5 16 00 00, datetime 2011 5 16 23 59, "DB"), 
 (datetime 2011 5 17 00 00, datetime 2011 5 17 23 59, "DB"), 
 (datetime 2011 5 18 00 00, datetime 2011 5 18 23 59, "DB"), 
 (datetime 2011 5 19 00 00, datetime 2011 5 19 23 59, "DB"), 
 (datetime 2011 5 20 00 00, datetime 2011 5 20 23 59, "DB"), 
 (datetime 2011 5 21 00 00, datetime 2011 5 21 23 59, "EN"), 
 (datetime 2011 5 22 00 00, datetime 2011 5 22 23 59, "EN"), 
 (datetime 2011 5 23 00 00, datetime 2011 5 23 23 59, "DB"), 
 (datetime 2011 5 24 00 00, datetime 2011 5 24 23 59, "DB"), 
 (datetime 2011 5 25 00 00, datetime 2011 5 25 23 59, "DB"), 
 (datetime 2011 5 26 00 00, datetime 2011 5 26 23 59, "DB"), 
 (datetime 2011 5 27 00 00, datetime 2011 5 27 23 59, "DB"), 
 (datetime 2011 5 28 00 00, datetime 2011 5 28 23 59, "EN"), 
 (datetime 2011 5 29 00 00, datetime 2011 5 29 23 59, "EN"), 
 (datetime 2011 5 30 00 00, datetime 2011 5 30 23 59, "DB"), 
 (datetime 2011 5 31 00 00, datetime 2011 5 31 23 59, "DB"), 
 (datetime 2011 6 1 00 00, datetime 2011 6 1 23 59, "DB"), 
 (datetime 2011 6 2 00 00, datetime 2011 6 2 23 59, "DN"), 
 (datetime 2011 6 3 00 00, datetime 2011 6 3 23 59, "DB"), 
 (datetime 2011 6 4 00 00, datetime 2011 6 4 23 59, "EN"), 
 (datetime 2011 6 5 00 00, datetime 2011 6 5 23 59, "EN"), 
 (datetime 2011 6 6 00 00, datetime 2011 6 6 23 59, "DB"), 
 (datetime 2011 6 7 00 00, datetime 2011 6 7 23 59, "DB"), 
 (datetime 2011 6 8 00 00, datetime 2011 6 8 23 59, "DB"), 
 (datetime 2011 6 9 00 00, datetime 2011 6 9 23 59, "DB"), 
 (datetime 2011 6 10 00 00, datetime 2011 6 10 23 59, "DB"), 
 (datetime 2011 6 11 00 00, datetime 2011 6 11 23 59, "EN"), 
 (datetime 2011 6 12 00 00, datetime 2011 6 12 23 59, "EN"), 
 (datetime 2011 6 13 00 00, datetime 2011 6 13 23 59, "DN"), 
 (datetime 2011 6 14 00 00, datetime 2011 6 14 23 59, "DB"), 
 (datetime 2011 6 15 00 00, datetime 2011 6 15 23 59, "DB"), 
 (datetime 2011 6 16 00 00, datetime 2011 6 16 23 59, "DB"), 
 (datetime 2011 6 17 00 00, datetime 2011 6 17 23 59, "DB"), 
 (datetime 2011 6 18 00 00, datetime 2011 6 18 23 59, "EN"), 
 (datetime 2011 6 19 00 00, datetime 2011 6 19 23 59, "EN"), 
 (datetime 2011 6 20 00 00, datetime 2011 6 20 23 59, "DB"), 
 (datetime 2011 6 21 00 00, datetime 2011 6 21 23 59, "DB"), 
 (datetime 2011 6 22 00 00, datetime 2011 6 22 23 59, "DB"), 
 (datetime 2011 6 23 00 00, datetime 2011 6 23 23 59, "DB"), 
 (datetime 2011 6 24 00 00, datetime 2011 6 24 23 59, "DB"), 
 (datetime 2011 6 25 00 00, datetime 2011 6 25 23 59, "EN"), 
 (datetime 2011 6 26 00 00, datetime 2011 6 26 23 59, "EN"), 
 (datetime 2011 6 27 00 00, datetime 2011 6 27 23 59, "DB"), 
 (datetime 2011 6 28 00 00, datetime 2011 6 28 23 59, "DB"), 
 (datetime 2011 6 29 00 00, datetime 2011 6 29 23 59, "DB"), 
 (datetime 2011 6 30 00 00, datetime 2011 6 30 23 59, "DB"), 
 (datetime 2011 7 1 00 00, datetime 2011 7 1 23 59, "DB"), 
 (datetime 2011 7 2 00 00, datetime 2011 7 2 23 59, "EN"), 
 (datetime 2011 7 3 00 00, datetime 2011 7 3 23 59, "EN"), 
 (datetime 2011 7 4 00 00, datetime 2011 7 4 23 59, "DB"), 
 (datetime 2011 7 5 00 00, datetime 2011 7 5 23 59, "DB"), 
 (datetime 2011 7 6 00 00, datetime 2011 7 6 23 59, "DB"), 
 (datetime 2011 7 7 00 00, datetime 2011 7 7 23 59, "DB"), 
 (datetime 2011 7 8 00 00, datetime 2011 7 8 23 59, "DB"), 
 (datetime 2011 7 9 00 00, datetime 2011 7 9 23 59, "EN"), 
 (datetime 2011 7 10 00 00, datetime 2011 7 10 23 59, "EN"), 
 (datetime 2011 7 11 00 00, datetime 2011 7 11 23 59, "DB"), 
 (datetime 2011 7 12 00 00, datetime 2011 7 12 23 59, "DB"), 
 (datetime 2011 7 13 00 00, datetime 2011 7 13 23 59, "DB"), 
 (datetime 2011 7 14 00 00, datetime 2011 7 14 23 59, "DB"), 
 (datetime 2011 7 15 00 00, datetime 2011 7 15 23 59, "DB"), 
 (datetime 2011 7 16 00 00, datetime 2011 7 16 23 59, "EN"), 
 (datetime 2011 7 17 00 00, datetime 2011 7 17 23 59, "EN"), 
 (datetime 2011 7 18 00 00, datetime 2011 7 18 23 59, "DB"), 
 (datetime 2011 7 19 00 00, datetime 2011 7 19 23 59, "DB"), 
 (datetime 2011 7 20 00 00, datetime 2011 7 20 23 59, "DB"), 
 (datetime 2011 7 21 00 00, datetime 2011 7 21 23 59, "DB"), 
 (datetime 2011 7 22 00 00, datetime 2011 7 22 23 59, "DB"), 
 (datetime 2011 7 23 00 00, datetime 2011 7 23 23 59, "EN"), 
 (datetime 2011 7 24 00 00, datetime 2011 7 24 23 59, "EN"), 
 (datetime 2011 7 25 00 00, datetime 2011 7 25 23 59, "DB"), 
 (datetime 2011 7 26 00 00, datetime 2011 7 26 23 59, "DB"), 
 (datetime 2011 7 27 00 00, datetime 2011 7 27 23 59, "DB"), 
 (datetime 2011 7 28 00 00, datetime 2011 7 28 23 59, "DB"), 
 (datetime 2011 7 29 00 00, datetime 2011 7 29 23 59, "DB"), 
 (datetime 2011 7 30 00 00, datetime 2011 7 30 23 59, "EN"), 
 (datetime 2011 7 31 00 00, datetime 2011 7 31 23 59, "EN"), 
 (datetime 2011 8 1 00 00, datetime 2011 8 1 23 59, "DB"), 
 (datetime 2011 8 2 00 00, datetime 2011 8 2 23 59, "DB"), 
 (datetime 2011 8 3 00 00, datetime 2011 8 3 23 59, "DB"), 
 (datetime 2011 8 4 00 00, datetime 2011 8 4 23 59, "DB"), 
 (datetime 2011 8 5 00 00, datetime 2011 8 5 23 59, "DB"), 
 (datetime 2011 8 6 00 00, datetime 2011 8 6 23 59, "EN"), 
 (datetime 2011 8 7 00 00, datetime 2011 8 7 23 59, "EN"), 
 (datetime 2011 8 8 00 00, datetime 2011 8 8 23 59, "DB"), 
 (datetime 2011 8 9 00 00, datetime 2011 8 9 23 59, "DB"), 
 (datetime 2011 8 10 00 00, datetime 2011 8 10 23 59, "DB"), 
 (datetime 2011 8 11 00 00, datetime 2011 8 11 23 59, "DB"), 
 (datetime 2011 8 12 00 00, datetime 2011 8 12 23 59, "DB"), 
 (datetime 2011 8 13 00 00, datetime 2011 8 13 23 59, "EN"), 
 (datetime 2011 8 14 00 00, datetime 2011 8 14 23 59, "EN"), 
 (datetime 2011 8 15 00 00, datetime 2011 8 15 23 59, "DB"), 
 (datetime 2011 8 16 00 00, datetime 2011 8 16 23 59, "DB"), 
 (datetime 2011 8 17 00 00, datetime 2011 8 17 23 59, "DB"), 
 (datetime 2011 8 18 00 00, datetime 2011 8 18 23 59, "DB"), 
 (datetime 2011 8 19 00 00, datetime 2011 8 19 23 59, "DB"), 
 (datetime 2011 8 20 00 00, datetime 2011 8 20 23 59, "EN"), 
 (datetime 2011 8 21 00 00, datetime 2011 8 21 23 59, "EN"), 
 (datetime 2011 8 22 00 00, datetime 2011 8 22 23 59, "DB"), 
 (datetime 2011 8 23 00 00, datetime 2011 8 23 23 59, "DB"), 
 (datetime 2011 8 24 00 00, datetime 2011 8 24 23 59, "DB"), 
 (datetime 2011 8 25 00 00, datetime 2011 8 25 23 59, "DB"), 
 (datetime 2011 8 26 00 00, datetime 2011 8 26 23 59, "DB"), 
 (datetime 2011 8 27 00 00, datetime 2011 8 27 23 59, "EN"), 
 (datetime 2011 8 28 00 00, datetime 2011 8 28 23 59, "EN"), 
 (datetime 2011 8 29 00 00, datetime 2011 8 29 23 59, "DB"), 
 (datetime 2011 8 30 00 00, datetime 2011 8 30 23 59, "DB"), 
 (datetime 2011 8 31 00 00, datetime 2011 8 31 23 59, "DB"), 
 (datetime 2011 9 1 00 00, datetime 2011 9 1 23 59, "DB"), 
 (datetime 2011 9 2 00 00, datetime 2011 9 2 23 59, "DB"), 
 (datetime 2011 9 3 00 00, datetime 2011 9 3 23 59, "EN"), 
 (datetime 2011 9 4 00 00, datetime 2011 9 4 23 59, "EN"), 
 (datetime 2011 9 5 00 00, datetime 2011 9 5 23 59, "DB"), 
 (datetime 2011 9 6 00 00, datetime 2011 9 6 23 59, "DB"), 
 (datetime 2011 9 7 00 00, datetime 2011 9 7 23 59, "DB"), 
 (datetime 2011 9 8 00 00, datetime 2011 9 8 23 59, "DB"), 
 (datetime 2011 9 9 00 00, datetime 2011 9 9 23 59, "DB"), 
 (datetime 2011 9 10 00 00, datetime 2011 9 10 23 59, "EN"), 
 (datetime 2011 9 11 00 00, datetime 2011 9 11 23 59, "EN"), 
 (datetime 2011 9 12 00 00, datetime 2011 9 12 23 59, "DB"), 
 (datetime 2011 9 13 00 00, datetime 2011 9 13 23 59, "DB"), 
 (datetime 2011 9 14 00 00, datetime 2011 9 14 23 59, "DB"), 
 (datetime 2011 9 15 00 00, datetime 2011 9 15 23 59, "DB"), 
 (datetime 2011 9 16 00 00, datetime 2011 9 16 23 59, "DB"), 
 (datetime 2011 9 17 00 00, datetime 2011 9 17 23 59, "EN"), 
 (datetime 2011 9 18 00 00, datetime 2011 9 18 23 59, "EN"), 
 (datetime 2011 9 19 00 00, datetime 2011 9 19 23 59, "DB"), 
 (datetime 2011 9 20 00 00, datetime 2011 9 20 23 59, "DB"), 
 (datetime 2011 9 21 00 00, datetime 2011 9 21 23 59, "DB"), 
 (datetime 2011 9 22 00 00, datetime 2011 9 22 23 59, "DB"), 
 (datetime 2011 9 23 00 00, datetime 2011 9 23 23 59, "DB"), 
 (datetime 2011 9 24 00 00, datetime 2011 9 24 23 59, "EN"), 
 (datetime 2011 9 25 00 00, datetime 2011 9 25 23 59, "EN"), 
 (datetime 2011 9 26 00 00, datetime 2011 9 26 23 59, "DB"), 
 (datetime 2011 9 27 00 00, datetime 2011 9 27 23 59, "DB"), 
 (datetime 2011 9 28 00 00, datetime 2011 9 28 23 59, "DB"), 
 (datetime 2011 9 29 00 00, datetime 2011 9 29 23 59, "DB"), 
 (datetime 2011 9 30 00 00, datetime 2011 9 30 23 59, "DB"), 
 (datetime 2011 10 1 00 00, datetime 2011 10 1 23 59, "EN"), 
 (datetime 2011 10 2 00 00, datetime 2011 10 2 23 59, "EN"), 
 (datetime 2011 10 3 00 00, datetime 2011 10 3 23 59, "DN"), 
 (datetime 2011 10 4 00 00, datetime 2011 10 4 23 59, "DB"), 
 (datetime 2011 10 5 00 00, datetime 2011 10 5 23 59, "DB"), 
 (datetime 2011 10 6 00 00, datetime 2011 10 6 23 59, "DB"), 
 (datetime 2011 10 7 00 00, datetime 2011 10 7 23 59, "DB"), 
 (datetime 2011 10 8 00 00, datetime 2011 10 8 23 59, "EN"), 
 (datetime 2011 10 9 00 00, datetime 2011 10 9 23 59, "EN"), 
 (datetime 2011 10 10 00 00, datetime 2011 10 10 23 59, "DB"), 
 (datetime 2011 10 11 00 00, datetime 2011 10 11 23 59, "DB"), 
 (datetime 2011 10 12 00 00, datetime 2011 10 12 23 59, "DB"), 
 (datetime 2011 10 13 00 00, datetime 2011 10 13 23 59, "DB"), 
 (datetime 2011 10 14 00 00, datetime 2011 10 14 23 59, "DB"), 
 (datetime 2011 10 15 00 00, datetime 2011 10 15 23 59, "EN"), 
 (datetime 2011 10 16 00 00, datetime 2011 10 16 23 59, "EN"), 
 (datetime 2011 10 17 00 00, datetime 2011 10 17 23 59, "DB"), 
 (datetime 2011 10 18 00 00, datetime 2011 10 18 23 59, "DB"), 
 (datetime 2011 10 19 00 00, datetime 2011 10 19 23 59, "DB"), 
 (datetime 2011 10 20 00 00, datetime 2011 10 20 23 59, "DB"), 
 (datetime 2011 10 21 00 00, datetime 2011 10 21 23 59, "DB"), 
 (datetime 2011 10 22 00 00, datetime 2011 10 22 23 59, "EN"), 
 (datetime 2011 10 23 00 00, datetime 2011 10 23 23 59, "EN"), 
 (datetime 2011 10 24 00 00, datetime 2011 10 24 23 59, "DB"), 
 (datetime 2011 10 25 00 00, datetime 2011 10 25 23 59, "DB"), 
 (datetime 2011 10 26 00 00, datetime 2011 10 26 23 59, "DB"), 
 (datetime 2011 10 27 00 00, datetime 2011 10 27 23 59, "DB"), 
 (datetime 2011 10 28 00 00, datetime 2011 10 28 23 59, "DB"), 
 (datetime 2011 10 29 00 00, datetime 2011 10 29 23 59, "EN"), 
 (datetime 2011 10 30 00 00, datetime 2011 10 30 23 59, "EN"), 
 (datetime 2011 10 31 00 00, datetime 2011 10 31 23 59, "DB"), 
 (datetime 2011 11 1 00 00, datetime 2011 11 1 23 59, "DB"), 
 (datetime 2011 11 2 00 00, datetime 2011 11 2 23 59, "DB"), 
 (datetime 2011 11 3 00 00, datetime 2011 11 3 23 59, "DB"), 
 (datetime 2011 11 4 00 00, datetime 2011 11 4 23 59, "DB"), 
 (datetime 2011 11 5 00 00, datetime 2011 11 5 23 59, "EN"), 
 (datetime 2011 11 6 00 00, datetime 2011 11 6 23 59, "EN"), 
 (datetime 2011 11 7 00 00, datetime 2011 11 7 23 59, "DB"), 
 (datetime 2011 11 8 00 00, datetime 2011 11 8 23 59, "DB"), 
 (datetime 2011 11 9 00 00, datetime 2011 11 9 23 59, "DB"), 
 (datetime 2011 11 10 00 00, datetime 2011 11 10 23 59, "DB"), 
 (datetime 2011 11 11 00 00, datetime 2011 11 11 23 59, "DB"), 
 (datetime 2011 11 12 00 00, datetime 2011 11 12 23 59, "EN"), 
 (datetime 2011 11 13 00 00, datetime 2011 11 13 23 59, "EN"), 
 (datetime 2011 11 14 00 00, datetime 2011 11 14 23 59, "DB"), 
 (datetime 2011 11 15 00 00, datetime 2011 11 15 23 59, "DB"), 
 (datetime 2011 11 16 00 00, datetime 2011 11 16 23 59, "DB"), 
 (datetime 2011 11 17 00 00, datetime 2011 11 17 23 59, "DB"), 
 (datetime 2011 11 18 00 00, datetime 2011 11 18 23 59, "DB"), 
 (datetime 2011 11 19 00 00, datetime 2011 11 19 23 59, "EN"), 
 (datetime 2011 11 20 00 00, datetime 2011 11 20 23 59, "EN"), 
 (datetime 2011 11 21 00 00, datetime 2011 11 21 23 59, "DB"), 
 (datetime 2011 11 22 00 00, datetime 2011 11 22 23 59, "DB"), 
 (datetime 2011 11 23 00 00, datetime 2011 11 23 23 59, "DB"), 
 (datetime 2011 11 24 00 00, datetime 2011 11 24 23 59, "DB"), 
 (datetime 2011 11 25 00 00, datetime 2011 11 25 23 59, "DB"), 
 (datetime 2011 11 26 00 00, datetime 2011 11 26 23 59, "EN"), 
 (datetime 2011 11 27 00 00, datetime 2011 11 27 23 59, "EN"), 
 (datetime 2011 11 28 00 00, datetime 2011 11 28 23 59, "DB"), 
 (datetime 2011 11 29 00 00, datetime 2011 11 29 23 59, "DB"), 
 (datetime 2011 11 30 00 00, datetime 2011 11 30 23 59, "DB"), 
 (datetime 2011 12 1 00 00, datetime 2011 12 1 23 59, "DB"), 
 (datetime 2011 12 2 00 00, datetime 2011 12 2 23 59, "DB"), 
 (datetime 2011 12 3 00 00, datetime 2011 12 3 23 59, "EN"), 
 (datetime 2011 12 4 00 00, datetime 2011 12 4 23 59, "EN"), 
 (datetime 2011 12 5 00 00, datetime 2011 12 5 23 59, "DB"), 
 (datetime 2011 12 6 00 00, datetime 2011 12 6 23 59, "DB"), 
 (datetime 2011 12 7 00 00, datetime 2011 12 7 23 59, "DB"), 
 (datetime 2011 12 8 00 00, datetime 2011 12 8 23 59, "DB"), 
 (datetime 2011 12 9 00 00, datetime 2011 12 9 23 59, "DB"), 
 (datetime 2011 12 10 00 00, datetime 2011 12 10 23 59, "EN"), 
 (datetime 2011 12 11 00 00, datetime 2011 12 11 23 59, "EN"), 
 (datetime 2011 12 12 00 00, datetime 2011 12 12 23 59, "DB"), 
 (datetime 2011 12 13 00 00, datetime 2011 12 13 23 59, "DB"), 
 (datetime 2011 12 14 00 00, datetime 2011 12 14 23 59, "DB"), 
 (datetime 2011 12 15 00 00, datetime 2011 12 15 23 59, "DB"), 
 (datetime 2011 12 16 00 00, datetime 2011 12 16 23 59, "DB"), 
 (datetime 2011 12 17 00 00, datetime 2011 12 17 23 59, "EN"), 
 (datetime 2011 12 18 00 00, datetime 2011 12 18 23 59, "EN"), 
 (datetime 2011 12 19 00 00, datetime 2011 12 19 23 59, "DB"), 
 (datetime 2011 12 20 00 00, datetime 2011 12 20 23 59, "DB"), 
 (datetime 2011 12 21 00 00, datetime 2011 12 21 23 59, "DB"), 
 (datetime 2011 12 22 00 00, datetime 2011 12 22 23 59, "DB"), 
 (datetime 2011 12 23 00 00, datetime 2011 12 23 23 59, "DB"), 
 (datetime 2011 12 24 00 00, datetime 2011 12 24 23 59, "EN"), 
 (datetime 2011 12 25 00 00, datetime 2011 12 25 23 59, "EN"), 
 (datetime 2011 12 26 00 00, datetime 2011 12 26 23 59, "DN"), 
 (datetime 2011 12 27 00 00, datetime 2011 12 27 23 59, "DB"), 
 (datetime 2011 12 28 00 00, datetime 2011 12 28 23 59, "DB"), 
 (datetime 2011 12 29 00 00, datetime 2011 12 29 23 59, "DB"), 
 (datetime 2011 12 30 00 00, datetime 2011 12 30 23 59, "DB"), 
 (datetime 2011 12 31 00 00, datetime 2011 12 31 23 59, "EN")]) 