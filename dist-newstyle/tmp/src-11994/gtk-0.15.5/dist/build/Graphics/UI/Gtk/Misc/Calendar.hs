
{-# LINE 2 "./Graphics/UI/Gtk/Misc/Calendar.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget Calendar
--
-- Author : Axel Simon
--
-- Created: 23 May 2001
--
-- Copyright (C) 1999-2005 Axel Simon
--
-- This library is free software; you can redistribute it and/or
-- modify it under the terms of the GNU Lesser General Public
-- License as published by the Free Software Foundation; either
-- version 2.1 of the License, or (at your option) any later version.
--
-- This library is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- Lesser General Public License for more details.
--
-- |
-- Maintainer : gtk2hs-users@lists.sourceforge.net
-- Stability : provisional
-- Portability : portable (depends on GHC)
--
-- Displays a calendar and allows the user to select a date
--
module Graphics.UI.Gtk.Misc.Calendar (
-- * Detail
--
-- | 'Calendar' is a widget that displays a calendar, one month at a time. It
-- can be created with 'calendarNew'.
--
-- The month and year currently displayed can be altered with
-- 'calendarSelectMonth'. The exact day can be selected from the displayed
-- month using 'calendarSelectDay'.
--
-- To place a visual marker on a particular day, use 'calendarMarkDay' and
-- to remove the marker, 'calendarUnmarkDay'. Alternative, all marks can be
-- cleared with 'calendarClearMarks'.
--
-- The way in which the calendar itself is displayed can be altered using
-- 'calendarSetDisplayOptions'.
--
-- The selected date can be retrieved from a 'Calendar' using
-- 'calendarGetDate'.

-- * Class Hierarchy
-- |
-- @
-- | 'GObject'
-- | +----'Object'
-- | +----'Widget'
-- | +----Calendar
-- @

-- * Types
  Calendar,
  CalendarClass,
  castToCalendar, gTypeCalendar,
  toCalendar,
  CalendarDisplayOptions(..),

-- * Constructors
  calendarNew,

-- * Methods
  calendarSelectMonth,
  calendarSelectDay,
  calendarMarkDay,
  calendarUnmarkDay,
  calendarClearMarks,


  calendarDisplayOptions,



  calendarSetDisplayOptions,
  calendarGetDisplayOptions,

  calendarGetDate,


  calendarFreeze,



-- * Attributes
  calendarYear,
  calendarMonth,
  calendarDay,

  calendarShowHeading,
  calendarShowDayNames,
  calendarNoMonthChange,
  calendarShowWeekNumbers,

-- calendarDisplayOptions,

-- * Signals
  onDaySelected,
  afterDaySelected,
  onDaySelectedDoubleClick,
  afterDaySelectedDoubleClick,
  onMonthChanged,
  afterMonthChanged,
  onNextMonth,
  afterNextMonth,
  onNextYear,
  afterNextYear,
  onPrevMonth,
  afterPrevMonth,
  onPrevYear,
  afterPrevYear,
  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.Flags (fromFlags, toFlags)
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.UI.Gtk.Types
{-# LINE 126 "./Graphics/UI/Gtk/Misc/Calendar.chs" #-}
import Graphics.UI.Gtk.Signals
{-# LINE 127 "./Graphics/UI/Gtk/Misc/Calendar.chs" #-}
import Graphics.UI.Gtk.General.Enums (CalendarDisplayOptions(..))


{-# LINE 130 "./Graphics/UI/Gtk/Misc/Calendar.chs" #-}

--------------------
-- Constructors

-- | Creates a new calendar, with the current date being selected.
--
calendarNew :: IO Calendar
calendarNew =
  makeNewObject mkCalendar $
  liftM (castPtr :: Ptr Widget -> Ptr Calendar) $
  gtk_calendar_new
{-# LINE 141 "./Graphics/UI/Gtk/Misc/Calendar.chs" #-}

--------------------
-- Methods

-- | Shifts the calendar to a different month.
--
calendarSelectMonth :: CalendarClass self => self
 -> Int -- ^ @month@ - a month number between 0 and 11.
 -> Int -- ^ @year@ - the year the month is in.
 -> IO ()
calendarSelectMonth self month year =
  liftM (const ()) $
  (\(Calendar arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_calendar_select_month argPtr1 arg2 arg3)
{-# LINE 154 "./Graphics/UI/Gtk/Misc/Calendar.chs" #-}
    (toCalendar self)
    (fromIntegral month)
    (fromIntegral year)

-- | Selects a day from the current month.
--
calendarSelectDay :: CalendarClass self => self
 -> Int -- ^ @day@ - the day number between 1 and 31, or 0 to unselect the
          -- currently selected day.
 -> IO ()
calendarSelectDay self day =
  (\(Calendar arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_calendar_select_day argPtr1 arg2)
{-# LINE 166 "./Graphics/UI/Gtk/Misc/Calendar.chs" #-}
    (toCalendar self)
    (fromIntegral day)

-- | Places a visual marker on a particular day.
--
calendarMarkDay :: CalendarClass self => self
 -> Int -- ^ @day@ - the day number to mark between 1 and 31.
 -> IO ()
calendarMarkDay self day =
  liftM (const ()) $
  (\(Calendar arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_calendar_mark_day argPtr1 arg2)
{-# LINE 177 "./Graphics/UI/Gtk/Misc/Calendar.chs" #-}
    (toCalendar self)
    (fromIntegral day)

-- | Removes the visual marker from a particular day.
--
calendarUnmarkDay :: CalendarClass self => self
 -> Int -- ^ @day@ - the day number to unmark between 1 and 31.
 -> IO ()
calendarUnmarkDay self day =
  liftM (const ()) $
  (\(Calendar arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_calendar_unmark_day argPtr1 arg2)
{-# LINE 188 "./Graphics/UI/Gtk/Misc/Calendar.chs" #-}
    (toCalendar self)
    (fromIntegral day)

-- | Remove all visual markers.
--
calendarClearMarks :: CalendarClass self => self -> IO ()
calendarClearMarks self =
  (\(Calendar arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_calendar_clear_marks argPtr1)
{-# LINE 196 "./Graphics/UI/Gtk/Misc/Calendar.chs" #-}
    (toCalendar self)


-- | Sets display options (whether to display the heading and the month
-- headings).
--
-- * Available since Gtk+ version 2.4
--
calendarSetDisplayOptions :: CalendarClass self => self
 -> [CalendarDisplayOptions]
 -> IO ()
calendarSetDisplayOptions self flags =
  (\(Calendar arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_calendar_set_display_options argPtr1 arg2)
{-# LINE 209 "./Graphics/UI/Gtk/Misc/Calendar.chs" #-}
    (toCalendar self)
    ((fromIntegral . fromFlags) flags)

-- | Returns the current display options for the calendar.
--
-- * Available since Gtk+ version 2.4
--
calendarGetDisplayOptions :: CalendarClass self => self
 -> IO [CalendarDisplayOptions]
calendarGetDisplayOptions self =
  liftM (toFlags . fromIntegral) $
  (\(Calendar arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_calendar_get_display_options argPtr1)
{-# LINE 221 "./Graphics/UI/Gtk/Misc/Calendar.chs" #-}
    (toCalendar self)




-- | Sets display options (whether to display the heading and the month
-- headings).
--
-- * Warning: this function is deprecated and should not be used in
-- newly-written code. Use 'calendarSetDisplayOptions' instead.
--
-- Removed in Gtk3.
calendarDisplayOptions :: CalendarClass self => self
 -> [CalendarDisplayOptions] -> IO ()
calendarDisplayOptions self flags =
  (\(Calendar arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_calendar_display_options argPtr1 arg2)
{-# LINE 237 "./Graphics/UI/Gtk/Misc/Calendar.chs" #-}
    (toCalendar self)
    ((fromIntegral . fromFlags) flags)



-- | Retrieve the currently selected date.
--
calendarGetDate :: CalendarClass self => self
 -> IO (Int,Int,Int) -- ^ @(year, month, day)@
calendarGetDate self =
  alloca $ \yearPtr ->
  alloca $ \monthPtr ->
  alloca $ \dayPtr -> do
  (\(Calendar arg1) arg2 arg3 arg4 -> withForeignPtr arg1 $ \argPtr1 ->gtk_calendar_get_date argPtr1 arg2 arg3 arg4)
{-# LINE 251 "./Graphics/UI/Gtk/Misc/Calendar.chs" #-}
    (toCalendar self)
    yearPtr
    monthPtr
    dayPtr
  year <- liftM fromIntegral $ peek yearPtr
  month <- liftM fromIntegral $ peek monthPtr
  day <- liftM fromIntegral $ peek dayPtr
  return (year,month,day)



-- | Does nothing. Previously locked the display of the calendar for several
-- update operations.
--
-- * Warning: this function is deprecated and should not be used in
-- newly-written code.
--
-- Removed in Gtk3.
calendarFreeze :: CalendarClass self => self
 -> IO a -- ^ An action that performs several update operations on the
         -- calendar. After the action finnishes all the changes made by it
         -- are displayed.
 -> IO a
calendarFreeze self update = do
  (\(Calendar arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_calendar_freeze argPtr1) (toCalendar self)
  res <- update
  (\(Calendar arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_calendar_thaw argPtr1) (toCalendar self)
  return res



--------------------
-- Attributes

-- | The selected year.
--
-- Allowed values: >= 0
--
-- Default value: 0
--
calendarYear :: CalendarClass self => Attr self Int
calendarYear = newAttrFromIntProperty "year"

-- | The selected month (as a number between 0 and 11).
--
-- Allowed values: [0,11]
--
-- Default value: 0
--
calendarMonth :: CalendarClass self => Attr self Int
calendarMonth = newAttrFromIntProperty "month"

-- | The selected day (as a number between 1 and 31, or 0 to unselect the
-- currently selected day).
--
-- Allowed values: [0,31]
--
-- Default value: 0
--
calendarDay :: CalendarClass self => Attr self Int
calendarDay = newAttrFromIntProperty "day"


-- | Determines whether a heading is displayed.
--
-- Default value: @True@
--
calendarShowHeading :: CalendarClass self => Attr self Bool
calendarShowHeading = newAttrFromBoolProperty "show-heading"

-- | Determines whether day names are displayed.
--
-- Default value: @True@
--
calendarShowDayNames :: CalendarClass self => Attr self Bool
calendarShowDayNames = newAttrFromBoolProperty "show-day-names"

-- | Determines whether the selected month can be changed.
--
-- Default value: @False@
--
calendarNoMonthChange :: CalendarClass self => Attr self Bool
calendarNoMonthChange = newAttrFromBoolProperty "no-month-change"

-- | Determines whether week numbers are displayed.
--
-- Default value: @False@
--
calendarShowWeekNumbers :: CalendarClass self => Attr self Bool
calendarShowWeekNumbers = newAttrFromBoolProperty "show-week-numbers"


-- | \'displayOptions\' property. See 'calendarGetDisplayOptions' and
-- 'calendarSetDisplayOptions'
--
--calendarDisplayOptions :: CalendarClass self => Attr self [CalendarDisplayOptions]
--calendarDisplayOptions = newAttr
-- calendarGetDisplayOptions
-- calendarSetDisplayOptions

--------------------
-- Signals

-- | Emitted when a day was selected.
--
onDaySelected, afterDaySelected :: CalendarClass self => self
 -> IO ()
 -> IO (ConnectId self)
onDaySelected = connect_NONE__NONE "day-selected" False
afterDaySelected = connect_NONE__NONE "day-selected" True

-- | Emitted when a day received a double click.
--
onDaySelectedDoubleClick, afterDaySelectedDoubleClick :: CalendarClass self => self
 -> IO ()
 -> IO (ConnectId self)
onDaySelectedDoubleClick =
  connect_NONE__NONE "day-selected-double-click" False
afterDaySelectedDoubleClick =
  connect_NONE__NONE "day-selected-double-click" True

-- | The month changed.
--
onMonthChanged, afterMonthChanged :: CalendarClass self => self
 -> IO ()
 -> IO (ConnectId self)
onMonthChanged = connect_NONE__NONE "month-changed" False
afterMonthChanged = connect_NONE__NONE "month-changed" True

-- | The next month was selected.
--
onNextMonth, afterNextMonth :: CalendarClass self => self
 -> IO ()
 -> IO (ConnectId self)
onNextMonth = connect_NONE__NONE "next-month" False
afterNextMonth = connect_NONE__NONE "next-month" True

-- | The next year was selected.
--
onNextYear, afterNextYear :: CalendarClass self => self
 -> IO ()
 -> IO (ConnectId self)
onNextYear = connect_NONE__NONE "next-year" False
afterNextYear = connect_NONE__NONE "next-year" True

-- | The previous month was selected.
--
onPrevMonth, afterPrevMonth :: CalendarClass self => self
 -> IO ()
 -> IO (ConnectId self)
onPrevMonth = connect_NONE__NONE "prev-month" False
afterPrevMonth = connect_NONE__NONE "prev-month" True

-- | The previous year was selected.
--
onPrevYear, afterPrevYear :: CalendarClass self => self
 -> IO ()
 -> IO (ConnectId self)
onPrevYear = connect_NONE__NONE "prev-year" False
afterPrevYear = connect_NONE__NONE "prev-year" True

foreign import ccall unsafe "gtk_calendar_new"
  gtk_calendar_new :: (IO (Ptr Widget))

foreign import ccall safe "gtk_calendar_select_month"
  gtk_calendar_select_month :: ((Ptr Calendar) -> (CUInt -> (CUInt -> (IO CInt))))

foreign import ccall safe "gtk_calendar_select_day"
  gtk_calendar_select_day :: ((Ptr Calendar) -> (CUInt -> (IO ())))

foreign import ccall safe "gtk_calendar_mark_day"
  gtk_calendar_mark_day :: ((Ptr Calendar) -> (CUInt -> (IO CInt)))

foreign import ccall safe "gtk_calendar_unmark_day"
  gtk_calendar_unmark_day :: ((Ptr Calendar) -> (CUInt -> (IO CInt)))

foreign import ccall safe "gtk_calendar_clear_marks"
  gtk_calendar_clear_marks :: ((Ptr Calendar) -> (IO ()))

foreign import ccall safe "gtk_calendar_set_display_options"
  gtk_calendar_set_display_options :: ((Ptr Calendar) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_calendar_get_display_options"
  gtk_calendar_get_display_options :: ((Ptr Calendar) -> (IO CInt))

foreign import ccall safe "gtk_calendar_display_options"
  gtk_calendar_display_options :: ((Ptr Calendar) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_calendar_get_date"
  gtk_calendar_get_date :: ((Ptr Calendar) -> ((Ptr CUInt) -> ((Ptr CUInt) -> ((Ptr CUInt) -> (IO ())))))

foreign import ccall unsafe "gtk_calendar_freeze"
  gtk_calendar_freeze :: ((Ptr Calendar) -> (IO ()))

foreign import ccall safe "gtk_calendar_thaw"
  gtk_calendar_thaw :: ((Ptr Calendar) -> (IO ()))
