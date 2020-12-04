{-# LINE 1 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- -*-haskell-*-




--  GIMP Toolkit (GTK) GDK Events
--
--  Author : Axel Simon
--
--  Created: 27 April 2001
--
--  Copyright (C) 2001-2005 Axel Simon
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public
--  License as published by the Free Software Foundation; either
--  version 2.1 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--
-- |
-- Maintainer  : gtk2hs-users\@lists.sourceforge.net
-- Stability   : deprecated
-- Portability : portable (depends on GHC)
--
-- Definiton of a record that contains event information. Deprecated in
-- favor of 'Graphics.UI.Gtk.Gdk.EventM' and not exported by Gtk.hs.
--
module Graphics.UI.Gtk.Gdk.Events (
  Modifier(..),         -- a mask of control keys
  TimeStamp,
  currentTime,

  -- | Deprecated way of conveying event information.
  Event(..),            -- information in event callbacks from Gdk
  EventButton,
  EventScroll,
  EventMotion,
  EventExpose,
  EventKey,
  EventConfigure,
  EventCrossing,
  EventFocus,
  EventProperty,
  EventProximity,
  EventVisibility,
  EventWindowState,
  EventGrabBroken,

  marshExposeRect,

  -- selector functions
  marshalEvent,         -- convert a pointer to an event data structure
  -- used data structures
  VisibilityState(..),
  CrossingMode(..),
  NotifyType(..),
  WindowState(..),
  ScrollDirection(..),
  MouseButton(..),
  Click(..),
  Rectangle(..)
  ) where

import System.IO.Unsafe (unsafeInterleaveIO)
import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Flags
import Graphics.UI.Gtk.Gdk.Keys         (KeyVal, keyvalToChar, keyvalName)

{-# LINE 75 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
import Graphics.UI.Gtk.Gdk.Region       (Region, makeNewRegion)

{-# LINE 77 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
import Graphics.UI.Gtk.Gdk.Enums        (Modifier(..),
                                         VisibilityState(..),
                                         CrossingMode(..),
                                         NotifyType(..),
                                         WindowState(..),
                                         ScrollDirection(..))
import Graphics.UI.Gtk.General.Enums    (MouseButton(..), Click(..))
import Graphics.UI.Gtk.General.Structs  (Rectangle(..))

-- | The time (in milliseconds) when an event happened. This is used mostly
-- for ordering events and responses to events.
--
type TimeStamp = Word32
-- TODO: make this a newtype

-- | Represents the current time, and can be used anywhere a time is expected.
currentTime :: TimeStamp
currentTime = 0
{-# LINE 95 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}

-- Note on Event:
-- * 'Event' can communicate a small array of data to another widget. This
--   functionality is not bound as it can be done easier in Haskell.
--
-- * EventDND is not implemented as registering a DND source or sink
--   should be easier and sufficient for everything.
--
-- * EventProperty is not bound since it involves Atoms and its hard to see
--   how a Haskell application should extract the data. It should be possible
--   to connect directly to 'propertyChanged' signals. If there is a need
--   to monitor a property for which there is no signal we could add
--   a trigger for just that property.
--
-- * EventSelection - I don\'t quite see how this works, so not bound.
--
-- * NoExpose - seems pointless: you copy from a drawable and this signal
--   tells you that it was up-to-date without redrawing. Maybe I'm missing
--   something.
--
-- * EventSetting informs about a change in setting that are shared among
--   several applications. They are probably not relevant to user defined
--   widgets. Anyway they don\'t make sense before GtkSettings isn\'t bound.
--
-- * Property is a TODO. These come from RC files which are useful for
--   custom widgets.

-- | An event that contains information on a button press.
type EventButton = Event

-- | An event that contains information on scrolling.
type EventScroll = Event

-- | An event that contains information on the movement of the mouse pointer.
type EventMotion = Event

-- | An area of the 'DrawWindow' needs redrawing.
type EventExpose = Event

-- | An event that contains information about a key press.
type EventKey = Event

-- | An event that contains the new size of a window.
type EventConfigure = Event

-- | Generated when the pointer enters or leaves a window.
type EventCrossing = Event

-- | An event that informs about a change of the input focus.
type EventFocus = Event

-- | An event that indicates a property of the window changed.
type EventProperty = Event

-- | An event that indicates that the pen of a graphics table is touching or
--   not touching the tablet.
type EventProximity = Event

-- | Parts of the window have been exposed or obscured.
type EventVisibility = Event

-- | The window state has changed.
type EventWindowState = Event

-- | A grab has been broken by unusual means.
type EventGrabBroken = Event

-- | Events that are delivered to a widget.
--
-- * Any given signal only emits one of these variants as described
--   in 'Graphics.UI.Gtk.Abstract.Widget.Widget'.
--   Many events share common attributes:
--
--   * The 'eventSent' attribute is @True@ if the event was not created by the
--      user but by another application.
--
--   * The 'eventTime' attribute contains a time in milliseconds when the event
--      happened.
--
--   * The 'eventX' and 'eventY' attributes contain the coordinates relative
--      to the 'Graphics.UI.Gtk.Abstract.Gdk.DrawWindow' associated with this
--      widget. The values can contain sub-pixel information if the input
--      device is a graphics tablet or the like.
--
--   * The 'eventModifier' attribute denotes what modifier key was pressed
--      during the event.
--
data Event =
  -- | An event that is not in one of the more specific categories below. This
  -- includes delete, destroy, map and unmap events. These events
  -- have no extra information associated with them.
  Event { eventSent :: Bool }
  -- | The expose event.
  --
  -- * A region of widget that receives this event needs to be redrawn.
  --   This event is the result of revealing part or all of a window
  --   or by the application calling functions like
  --   'Graphics.UI.Gtk.Abstract.Widget.widgetQueueDrawArea'.
  --
  | Expose {
    eventSent   :: Bool,
    -- | A bounding box denoting what needs to be updated. For a more
    -- detailed information on the area that needs redrawing, use the
    -- next field.
    eventArea   :: Rectangle,

{-# LINE 201 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
    -- | A set of horizontal stripes that denote the invalid area.
    eventRegion      :: Region,

{-# LINE 204 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}

    -- | The number of contiguous 'Expose' events following this
    --   one. The only use for this is \"exposure compression\", i.e.
    --   handling all contiguous 'Expose' events in one go, though Gdk
    --   performs some exposure compression so this is not normally needed.
    eventCount  :: Int }
  -- | Mouse motion.
  --
  -- * Captures the movement of the mouse cursor while it is within the area
  --   of the widget.
  --
  | Motion {
    eventSent   :: Bool,
    eventTime   :: TimeStamp,
    eventX,eventY       :: Double,
    eventModifier       :: [Modifier],
    -- | Indicate if this event is only a hint of the motion.
    --
    -- * If the 'Graphics.UI.Gtk.Abstract.Widget.PointerMotionHintMask'
    --  is set with 'Data.Array.MArray.widgetAddEvents' then
    --   mouse positions are only generated each time
    --  'Graphics.UI.Gtk.Gdk.DrawWindow.drawWindowGetPointer'
    --   is called. In this case 'eventIsHint' is set to @True@.
    --
    eventIsHint :: Bool,
    eventXRoot,
    eventYRoot  :: Double }
  -- | A mouse button was pressed or released.
  --
  -- * This event is triggered if the mouse button was pressed or released
  --   while the mouse cursor was within the region of the widget.
  --
  | Button {
    eventSent   :: Bool,
    -- | The kind of button press, see 'Click'. Note that double clicks will
    --   trigger this event with 'eventClick' set to 'SingleClick',
    --   'ReleaseClick',
    --   'SingleClick', 'DoubleClick', 'ReleaseClick'. Triple clicks will
    --   produce this sequence followed by 'SingleClick', 'DoubleClick',
    --   'TripleClick', 'ReleaseClick'.
    eventClick  :: Click,
    -- | The time of the event in milliseconds.
    eventTime   :: TimeStamp,
    eventX,eventY       :: Double,
    eventModifier       :: [Modifier],
    -- | The button that was pressed.
    eventButton :: MouseButton,
    -- | The coordinates of the click relative to the screen origin.
    eventXRoot,
    eventYRoot  :: Double }
  -- | A key was pressed while the widget had the input focus.
  --
  -- * If the widget has the current input focus (see
  --   'Graphics.UI.Gtk.Abstract.Widget.widgetSetCanFocus')
  --   it will receive key pressed events. Certain key combinations are of
  --   no interest to a normal widget like Alt-F to access the file menu.
  --   For all these keys, the handler must return @False@ to indicate that
  --   the key stroke should be propagated to the parent widget. At the
  --   top-level widget, keyboard shortcuts like Alt-F are turned into the
  --   corresponding signals.
  --
  | Key {
    -- | This flag is set if the key was released. This flag makes it possible
    --   to connect the same handler to
    --  'Graphics.UI.Gtk.Abstract.Widget.onKeyPress' and
    --  'Graphics.UI.Gtk.Abstract.Widget.onKeyRelease'.
    eventRelease        :: Bool,
    eventSent   :: Bool,
    eventTime   :: TimeStamp,
    eventModifier       :: [Modifier],
    -- | This flag is @True@ if Caps Lock is on while this key was pressed.
    eventWithCapsLock   :: Bool,
    -- | This flag is @True@ if Number Lock is on while this key was pressed.
    eventWithNumLock    :: Bool,
    -- | This flag is @True@ if Scroll Lock is on while this key was pressed.
    eventWithScrollLock :: Bool,
    -- | A number representing the key that was pressed or released. A more convenient
    --   interface is provided by the next two fields.
    eventKeyVal :: KeyVal,
    -- | A string representing the key that was pressed or released.
    --
    -- * This string contains a description of the key rather than what
    --   should appear on screen. For example, pressing "1" on the keypad
    --   results in "KP_1". Of particular interest are "F1" till "F12",
    --   for a complete list refer to \"<gdk/gdkkeysyms.h>\" where all
    --   possible values are defined. The corresponding strings are the
    --   constants without the GDK_ prefix.
    eventKeyName :: DefaultGlibString,
    -- | A character matching the key that was pressed.
    --
    -- * This entry can be used to build up a whole input string.
    --   The character is @Nothing@ if the key does not correspond to a simple
    --   unicode character.
    --
    eventKeyChar     :: Maybe Char }
  -- | Mouse cursor crossing event.
  --
  -- * This event indicates that the mouse cursor is hovering over this
  --   widget. It is used to set a widget into the pre-focus state where
  --   some GUI elements like buttons on a toolbar change their appearance.
  --
  | Crossing {
    eventSent   :: Bool,
    eventTime   :: TimeStamp,
    eventX,eventY       :: Double,
    eventXRoot,
    eventYRoot  :: Double,
    -- | This flag is false if the widget was entered, it is true when the
    --   widget the mouse cursor left the widget.
    eventLeaves :: Bool,
    -- | Kind of enter\/leave event.
    --
    -- * The mouse cursor might enter this widget because it grabs the mouse
    --   cursor for e.g. a modal dialog box.
    --
    eventCrossingMode   :: CrossingMode,
    -- | Information on from what level of the widget hierarchy the mouse
    --   cursor came.
    --
    -- * See 'NotifyType'.
    --
    eventNotifyType     :: NotifyType,
    eventModifier       :: [Modifier]}
  -- | Gaining or loosing input focus.
  --
  | Focus {
    eventSent   :: Bool,
    -- | This flag is @True@ if the widget receives the focus and @False@ if
    -- it just lost the input focus.
    eventInFocus        :: Bool}
  -- | The widget\'s size has changed.
  --
  -- * In response to this event the application can allocate resources that
  --   are specific to the size of the widget. It is emitted when the widget
  --   is shown the first time and on every resize.
  --
  | Configure {
    eventSent   :: Bool,
    -- | Position within the parent window.
    eventXParent        :: Int,
    -- | Position within the parent window.
    eventYParent        :: Int,
    eventWidth  :: Int,
    eventHeight :: Int}
  -- | Change of visibility of a widget.
  | Visibility {
    eventSent   :: Bool,
    -- | Denote what portions of the widget is visible.
    eventVisible        :: VisibilityState }
  -- | Wheel movement of the mouse.
  --
  -- * This action denotes that the content of the widget should be scrolled.
  --   The event is triggered by the movement of the mouse wheel. Surrounding
  --   scroll bars are independant of this signal. Most mice do not have
  --   buttons for horizontal scrolling, hence 'eventDirection' will usually not
  --   contain 'ScrollLeft' and 'ScrollRight'. Mice with additional
  --   buttons may not work on X since only five buttons are supported
  --   (the three main buttons and two for the wheel).
  --
  -- * The handler of this signal should update the scroll bars that
  --   surround this widget which in turn tell this widget to update.
  --
  | Scroll {
    eventSent   :: Bool,
    eventTime   :: TimeStamp,
    eventX,eventY       :: Double,
    eventDirection      :: ScrollDirection,
    eventXRoot,
    eventYRoot  :: Double}
  -- | Indicate how the appearance of this window has changed.
  | WindowState {
    eventSent   :: Bool,
    -- | The mask indicates which flags have changed.
    eventWindowMask     :: [WindowState],
    -- | The state indicates the current state of the window.
    eventWindowState    :: [WindowState]}
  -- | The state of the pen of a graphics tablet pen or touchscreen device.
  | Proximity {
    eventSent   :: Bool,
    eventTime   :: TimeStamp,
    -- | Whether the stylus has moved in or out of contact with the tablet.
    eventInContact     :: Bool
  } deriving Show

marshalEvent :: Ptr Event -> IO Event
marshalEvent ptr = do
  (eType::Int32) <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 391 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  (case eType of
    0         -> marshAny
{-# LINE 393 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
    1        -> marshAny
{-# LINE 394 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
    2         -> marshExpose
{-# LINE 395 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
    3  -> marshMotion
{-# LINE 396 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
    4   -> marshButton SingleClick
{-# LINE 397 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
    5  -> marshButton DoubleClick
{-# LINE 398 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
    6  -> marshButton TripleClick
{-# LINE 399 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
    7 -> marshButton ReleaseClick
{-# LINE 400 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
    8      -> marshKey False
{-# LINE 401 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
    9    -> marshKey True
{-# LINE 402 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
    10   -> marshCrossing False
{-# LINE 403 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
    11   -> marshCrossing True
{-# LINE 404 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
    12   -> marshFocus
{-# LINE 405 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
    13      -> marshConfigure
{-# LINE 406 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
    14            -> marshAny
{-# LINE 407 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
    15          -> marshAny
{-# LINE 408 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
--    #{const GDK_PROPERTY_NOTIFY}-> marshProperty
    20   -> marshProximity True
{-# LINE 410 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
    21  -> marshProximity False
{-# LINE 411 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
    29-> marshVisibility
{-# LINE 412 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
    31         -> marshScroll
{-# LINE 413 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
    32   -> marshWindowState
{-# LINE 414 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
    code                        -> \_ -> fail $
      "marshalEvent: unhandled event type " ++ show code ++
      "\nplease report this as a bug to gtk2hs-devel@lists.sourceforge.net"
    ) ptr

marshAny ptr = do
  (sent   ::Int8) <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 421 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  return Event {
    eventSent = toBool sent
  }

marshExpose ptr = do
  (2::Int32) <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 427 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  (sent_   ::Int8)        <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 428 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  (area_   ::Rectangle)         <- (\hsc_ptr -> peekByteOff hsc_ptr 20) ptr
{-# LINE 429 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}

{-# LINE 430 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  (reg_   :: Ptr Region)        <- (\hsc_ptr -> peekByteOff hsc_ptr 40) ptr
{-# LINE 431 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  reg_ <- gdk_region_copy reg_
  region_ <- makeNewRegion reg_

{-# LINE 434 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  (count_  ::Int32) <- (\hsc_ptr -> peekByteOff hsc_ptr 48) ptr
{-# LINE 435 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  return $ Expose {
    eventSent   = toBool sent_,
    eventArea   = area_,

{-# LINE 439 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
    eventRegion = region_,

{-# LINE 441 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
    eventCount  = fromIntegral count_}


{-# LINE 444 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
foreign import ccall "gdk_region_copy"
  gdk_region_copy :: Ptr Region -> IO (Ptr Region)

{-# LINE 447 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}

marshExposeRect :: Ptr Event -> IO Rectangle
marshExposeRect ptr = do
  (2::Int32) <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 451 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  (area_   ::Rectangle)         <- (\hsc_ptr -> peekByteOff hsc_ptr 20) ptr
{-# LINE 452 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  return area_

marshMotion ptr = do
  (sent_   ::Int8)        <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 456 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  (time_   ::Word32)      <- (\hsc_ptr -> peekByteOff hsc_ptr 20) ptr
{-# LINE 457 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  (x_      ::Double)      <- (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
{-# LINE 458 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  (y_      ::Double)      <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 459 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  (modif_  ::Word32)        <- (\hsc_ptr -> peekByteOff hsc_ptr 48) ptr
{-# LINE 460 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  (isHint_ ::Int16)       <- (\hsc_ptr -> peekByteOff hsc_ptr 52) ptr
{-# LINE 461 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  (xRoot_  ::Double)      <- (\hsc_ptr -> peekByteOff hsc_ptr 64) ptr
{-# LINE 462 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  (yRoot_  ::Double)      <- (\hsc_ptr -> peekByteOff hsc_ptr 72) ptr
{-# LINE 463 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  return $ Motion {
    eventSent   = toBool sent_,
    eventTime   = fromIntegral time_,
    eventX         = realToFrac x_,
    eventY         = realToFrac y_,
    eventModifier  = (toFlags . fromIntegral) modif_,
    eventIsHint = toBool isHint_,
    eventXRoot  = realToFrac xRoot_,
    eventYRoot  = realToFrac yRoot_}

marshButton but ptr = do
  (sent_   ::Int8)        <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 475 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  (time_   ::Word32)      <- (\hsc_ptr -> peekByteOff hsc_ptr 20) ptr
{-# LINE 476 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  (x_      ::Double)      <- (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
{-# LINE 477 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  (y_      ::Double)      <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 478 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  (modif_  ::Word32)        <- (\hsc_ptr -> peekByteOff hsc_ptr 48) ptr
{-# LINE 479 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  (button_ ::Word32)        <- (\hsc_ptr -> peekByteOff hsc_ptr 52) ptr
{-# LINE 480 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  (xRoot_  ::Double)      <- (\hsc_ptr -> peekByteOff hsc_ptr 64) ptr
{-# LINE 481 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  (yRoot_  ::Double)      <- (\hsc_ptr -> peekByteOff hsc_ptr 72) ptr
{-# LINE 482 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  return $ Button {
    eventClick  = but,
    eventSent   = toBool sent_,
    eventTime   = fromIntegral time_,
    eventX         = realToFrac x_,
    eventY         = realToFrac y_,
    eventModifier  = (toFlags . fromIntegral) modif_,
    eventButton = (toEnum.fromIntegral) button_,
    eventXRoot  = realToFrac xRoot_,
    eventYRoot  = realToFrac yRoot_}


marshKey up ptr = do
  (sent_   ::Int8)        <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 496 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  (time_   ::Word32)      <- (\hsc_ptr -> peekByteOff hsc_ptr 20) ptr
{-# LINE 497 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  (modif_  ::Word32)        <- (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
{-# LINE 498 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  (keyval_ ::Word32)        <- (\hsc_ptr -> peekByteOff hsc_ptr 28) ptr
{-# LINE 499 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}

  (length_ ::Int32) <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 501 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  keyChar <- keyvalToChar keyval_
  keyName <- unsafeInterleaveIO $ keyvalName keyval_
  return $ Key {
    eventRelease = up,
    eventSent = toBool sent_,
    eventTime   = fromIntegral time_,
    eventModifier  = (toFlags . fromIntegral) modif_,
    eventWithCapsLock = (modif_ .&. 2)/=0,
{-# LINE 509 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
    eventWithNumLock = (modif_ .&. 16)/=0,
{-# LINE 510 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
    eventWithScrollLock = (modif_ .&. 32)/=0,
{-# LINE 511 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
    eventKeyVal = keyval_,
    eventKeyName = keyName,
    eventKeyChar = keyChar }

marshCrossing leave ptr = do
  (sent_   ::Int8)        <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 517 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  (time_   ::Word32)      <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 518 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  (x_      ::Double)      <- (\hsc_ptr -> peekByteOff hsc_ptr 40) ptr
{-# LINE 519 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  (y_      ::Double)      <- (\hsc_ptr -> peekByteOff hsc_ptr 48) ptr
{-# LINE 520 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  (modif_  ::Word32)        <- (\hsc_ptr -> peekByteOff hsc_ptr 84) ptr
{-# LINE 521 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  (xRoot_  ::Double)      <- (\hsc_ptr -> peekByteOff hsc_ptr 56) ptr
{-# LINE 522 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  (yRoot_  ::Double)      <- (\hsc_ptr -> peekByteOff hsc_ptr 64) ptr
{-# LINE 523 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  (cMode_  ::Word32)
{-# LINE 524 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
                                <- (\hsc_ptr -> peekByteOff hsc_ptr 72) ptr
{-# LINE 525 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  (nType_  ::Word32)
{-# LINE 526 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
                                <- (\hsc_ptr -> peekByteOff hsc_ptr 76) ptr
{-# LINE 527 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  (modif_  ::Word32)        <- (\hsc_ptr -> peekByteOff hsc_ptr 84) ptr
{-# LINE 528 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  return $ Crossing {
    eventSent   = toBool sent_,
    eventTime   = fromIntegral time_,
    eventX         = realToFrac x_,
    eventY         = realToFrac y_,
    eventXRoot  = realToFrac xRoot_,
    eventYRoot  = realToFrac yRoot_,
    eventLeaves = leave,
    eventCrossingMode  = (toEnum.fromIntegral) cMode_,
    eventNotifyType    = (toEnum.fromIntegral) nType_,
    eventModifier      = (toFlags . fromIntegral) modif_}


marshFocus ptr = do
  (sent_   ::Int8)        <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 543 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  (inFocus_::Int16)       <- (\hsc_ptr -> peekByteOff hsc_ptr 18) ptr
{-# LINE 544 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  return $ Focus {
    eventSent   = toBool sent_,
    eventInFocus= toBool inFocus_}

marshConfigure ptr = do
  (sent_   ::Int8)        <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 550 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  (xPar_   ::Int32) <- (\hsc_ptr -> peekByteOff hsc_ptr 20) ptr
{-# LINE 551 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  (yPar_   ::Int32) <- (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
{-# LINE 552 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  (width_  ::Int32) <- (\hsc_ptr -> peekByteOff hsc_ptr 28) ptr
{-# LINE 553 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  (height_ ::Int32) <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 554 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  return $ Configure {
    eventSent   = toBool sent_,
    eventXParent   = fromIntegral xPar_,
    eventYParent   = fromIntegral yPar_,
    eventWidth  = fromIntegral width_,
    eventHeight = fromIntegral height_}

{-
marshProperty ptr = do
  (sent_   ::#gtk2hs_type gint8)        <- #{peek GdkEventProperty, send_event} ptr
  (time_   ::#gtk2hs_type guint32)      <- #{peek GdkEventProperty, time} ptr
  return $ Property {
    eventSent   = toBool sent_,
    eventTime   = fromIntegral time_}
-}

marshProximity contact ptr = do
  (sent_   ::Int8)        <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 572 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  (time_   ::Word32)      <- (\hsc_ptr -> peekByteOff hsc_ptr 20) ptr
{-# LINE 573 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  return $ Proximity {
    eventSent   = toBool sent_,
    eventTime   = fromIntegral time_,
    eventInContact = contact}

marshVisibility ptr = do
  (sent_   ::Int8)        <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 580 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  (state_  ::Word32)
{-# LINE 581 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
                                <- (\hsc_ptr -> peekByteOff hsc_ptr 20) ptr
{-# LINE 582 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  return $ Visibility {
    eventSent   = toBool sent_,
    eventVisible= (toEnum.fromIntegral) state_}

marshScroll ptr = do
  (sent_   ::Int8)        <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 588 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  (time_   ::Word32)      <- (\hsc_ptr -> peekByteOff hsc_ptr 20) ptr
{-# LINE 589 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  (x_     ::Double)       <- (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
{-# LINE 590 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  (y_     ::Double)       <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 591 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  (direc_  ::Word32)
{-# LINE 592 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
                                <- (\hsc_ptr -> peekByteOff hsc_ptr 44) ptr
{-# LINE 593 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  (xRoot_  ::Double)      <- (\hsc_ptr -> peekByteOff hsc_ptr 56) ptr
{-# LINE 594 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  (yRoot_  ::Double)      <- (\hsc_ptr -> peekByteOff hsc_ptr 64) ptr
{-# LINE 595 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  return $ Scroll {
    eventSent   = toBool sent_,
    eventTime   = fromIntegral time_,
    eventX         = realToFrac x_,
    eventY         = realToFrac y_,
    eventDirection  = (toEnum.fromIntegral) direc_,
    eventXRoot  = realToFrac xRoot_,
    eventYRoot  = realToFrac yRoot_}


marshWindowState ptr = do
  (sent_   ::Int8)        <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 607 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  (wMask_  ::Word32)
{-# LINE 608 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
                        <- (\hsc_ptr -> peekByteOff hsc_ptr 20) ptr
{-# LINE 609 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  (wState_ ::Word32)
{-# LINE 610 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
                        <- (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
{-# LINE 611 "Graphics/UI/Gtk/Gdk/Events.hsc" #-}
  return $ WindowState {
    eventSent   = toBool sent_,
    eventWindowMask  = (toFlags.fromIntegral) wMask_,
    eventWindowState = (toFlags.fromIntegral) wState_}

