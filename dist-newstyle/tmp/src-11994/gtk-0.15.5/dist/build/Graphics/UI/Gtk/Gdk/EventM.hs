{-# LINE 1 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE EmptyDataDecls #-}

{-# LINE 4 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
{-# LANGUAGE FlexibleContexts #-}

{-# LINE 6 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
-- -*-haskell-*-




--  GIMP Toolkit (GTK) GDK Event information in a Monad
--
--  Author : Axel Simon
--
--  Created 12 October 2008
--
--  Copyright (C) 2008 Axel Simon
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
-- #prune

-- |
-- Maintainer  : gtk2hs-users\@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- Types and accessors to examine information in events.
--
module Graphics.UI.Gtk.Gdk.EventM (
-- * Detail
--
-- | This modules provides a monad that encapsulates the information in an
--   event.
--
--   The events a widget can receive are defined in
--   "Graphics.UI.Gtk.Abstract.Widget#7". Every event carries additional
--   information which is accessible through functions in the 'EventM' monad.
--   For instance, every event is associated with a
--   'Graphics.UI.Gtk.Gdk.DrawWindow.DrawWindow' which is accessed using the
--   'eventWindow' accessor function. Other information is only available in
--   one specific event. For example, the
--   area that has to be redrawn, accessed by 'eventArea' is only available in
--   the 'Graphics.UI.Gtk.Abstract.Widget.exposeEvent'. Indeed, you can only
--   call 'eventArea' if the first type parameter of 'EventM' is the phantom
--   type 'EExpose'. (A phantom type is a type for which no values exist and
--   which is only used to enforce certain constraints on the usage of
--   functions such as 'eventArea'.) Some information is available in several
--   but not all events. In order to express these constraints the module
--   defines type classes whose names start with @Has...@ but which are not
--   exported, implying that no new instance can be created. (They could be
--   called phantom type classes.) For instance, the mouse pointer coordinates
--   can be retrieved using the function 'eventCoordinates' which requires
--   that the first type parameter of 'EventM' is in the class
--   'HasCoordinates'. The module supplies instance of class 'HasCoordinates'
--   for the types 'EButton', 'ECrossing', 'EMotion' and 'EScroll'. Thus for
--   all events that require an 'EventM' action with one of the types above,
--   the accessor function 'eventCoordinates' may be used.
--
--   Note that an event handler must always returns @True@ if the event
--   was handled or @False@ if the event should be dealt with by another
--   event handler. For instance, a handler for a key press should return
--   @False@ if the pressed key is not one of those that the widget reacts
--   to. In this case the event is passed to the parent widgets. This
--   ensures that pressing, say, @Alt-F@ opens the file menu even if the
--   current input focus is in a text entry widget. In order to facilitate
--   writing handlers that may abort handling an event, this module provides
--   the function 'tryEvent'. This function catches pattern match exceptions
--   and returns @False@. If the signal successfully runs to its end, it
--   returns @True@. A typical use is as follows:
--
--   > widget `on` keyPressEvent $ tryEvent $ do
--   >   [Control] <- eventModifier
--   >   "Return" <- eventKeyName
--   >   liftIO $ putStrLn "Ctrl-Return pressed"
--
--   The rationale is that the action will throw an exception if the
--   two event functions 'eventModifier' and 'eventKeyName' return something
--   else than what is stated in
--   the pattern. When no exception is thrown, execution continues to
--   the last statement where the event is processed, here we merely
--   print a message. Note that the return
--   value of this statement must be @()@ since 'tryEvent' always
--   assumes that the
--   function handeled the event if no exception is thrown. A handler
--   wrapped by 'tryEvent' can also indicate that it cannot handle the
--   given event by calling 'stopEvent'.
--
--   Finally, not that the 'EventM' monad wraps the @IO@ monad. As such
--   you can (and usually have to) use @liftIO@ to execute @IO@ functions.
--

-- * Classes

  HasCoordinates,
  HasRootCoordinates,
  HasModifier,
  HasTime,

-- * Event monad and type tags
  EventM,
  EAny,
  EKey,
  EButton,
  EScroll,
  EMotion,
  EExpose,
  EVisibility,
  ECrossing,
  EFocus,
  EConfigure,
  EProperty,
  EProximity,
  EWindowState,

{-# LINE 124 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
  EOwnerChange,

{-# LINE 126 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}

{-# LINE 127 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
  EGrabBroken,

{-# LINE 129 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}

-- * Accessor functions for event information
  eventWindow,
  eventSent,
  eventCoordinates,
  eventRootCoordinates,
  eventModifier,
  eventModifierAll,
  eventModifierMouse,
  eventTime,
  eventKeyVal,
  eventKeyName,
  eventHardwareKeycode,
  eventKeyboardGroup,
  MouseButton(..),
  eventButton,
  Click(..),
  eventClick,
  ScrollDirection(..),
  eventScrollDirection,
  eventIsHint,

{-# LINE 151 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
  eventRequestMotions,

{-# LINE 153 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
  eventArea,

{-# LINE 155 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
  eventRegion,

{-# LINE 157 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
  VisibilityState(..),
  eventVisibilityState,
  CrossingMode(..),
  eventCrossingMode,
  NotifyType(..),
  eventNotifyType,
  eventCrossingFocus,
  eventFocusIn,
  eventPosition,
  eventSize,
  eventProperty,
  WindowState(..),
  eventWindowStateChanged,
  eventWindowState,

{-# LINE 172 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
  OwnerChange(..),
  eventChangeReason,
  eventSelection,
  eventSelectionTime,

{-# LINE 177 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}

{-# LINE 178 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
  eventKeyboardGrab,
  eventImplicit,
  eventGrabWindow,

{-# LINE 182 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}

-- * Auxilliary Definitions
  Modifier(..),         -- a mask of control keys
  TimeStamp,
  currentTime,
  tryEvent,
  stopEvent,
  ) where

import Prelude hiding (catch)
import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Flags
import System.Glib.GObject ( makeNewGObject )
import Graphics.UI.Gtk.Gdk.Keys         (KeyVal, KeyCode, keyName)

{-# LINE 198 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
import Graphics.UI.Gtk.Gdk.Region       (Region, makeNewRegion)

{-# LINE 200 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
import Graphics.UI.Gtk.Gdk.Enums        (Modifier(..), VisibilityState(..),
  CrossingMode(..), NotifyType(..), WindowState(..), ScrollDirection(..),

{-# LINE 203 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
  OwnerChange(..)

{-# LINE 205 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
  )
import Graphics.UI.Gtk.General.Enums    (MouseButton(..), Click(..))
import Graphics.UI.Gtk.General.Structs  (Rectangle(..))
import Graphics.UI.Gtk.General.DNDTypes (Atom(..), SelectionTag)
import Graphics.UI.Gtk.Types ( DrawWindow, mkDrawWindow )

import Data.List (isPrefixOf)
import Control.Monad.Reader ( ReaderT, ask, runReaderT )
import Control.Monad.Trans ( liftIO )
import Control.Monad ( liftM )

{-# LINE 216 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
import Control.Exception ( Handler(..)
                         , PatternMatchFail(..)
                         , catches, throw )
import System.IO.Error (isUserError, ioeGetErrorString)

{-# LINE 224 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}


-- | A monad providing access to data in an event.
--
type EventM t = ReaderT (Ptr t) IO

-- | A tag for events that do not carry any event-specific information.
data EAny

-- | A tag for /key/ events.
data EKey

-- | A tag for /Button/ events.
data EButton

-- | A tag for /Scroll/ events.
data EScroll

-- | A tag for /Motion/ events.
data EMotion

-- | A tag for /Expose/ events.
data EExpose

-- | A tag for /Visibility/ events.
data EVisibility

-- | A tag for /Crossing/ events.
data ECrossing

-- | A tag for /Focus/ events.
data EFocus

-- | A tag for /Configure/ events.
data EConfigure

-- | A tag for /Property/ events.
data EProperty

-- | A tag for /Proximity/ events.
data EProximity

-- | A tag for /WindowState/ event.
data EWindowState


{-# LINE 270 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
-- | A tag for /OwnerChange/ events.
data EOwnerChange

{-# LINE 273 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}



{-# LINE 276 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
-- | A tag for /GrabBroken/ events.
data EGrabBroken

{-# LINE 279 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}

-- | Retrieve the 'Graphics.UI.Gtk.Gdk.DrawWindow.DrawWindow' that this
--   event relates to.
eventWindow :: EventM any DrawWindow
eventWindow = do
  ptr <- ask
  liftIO $ makeNewGObject mkDrawWindow ((\hsc_ptr -> peekByteOff hsc_ptr 8) ptr)
{-# LINE 286 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}

-- | Query if this event was sent sent explicitly by the application
--   (rather than being generated by human interaction).
eventSent :: EventM any Bool
eventSent = do
  ptr <- ask
  liftIO $ (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 293 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}

class HasCoordinates a
instance HasCoordinates EButton
instance HasCoordinates EScroll
instance HasCoordinates EMotion
instance HasCoordinates ECrossing

-- | Retrieve the @(x,y)@ coordinates of the mouse.
eventCoordinates :: HasCoordinates t => EventM t (Double, Double)
eventCoordinates = do
  ptr <- ask
  liftIO $ do
    (ty :: Int32) <- peek (castPtr ptr)
{-# LINE 306 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
    if ty `elem` [ 4,
{-# LINE 307 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
                   5,
{-# LINE 308 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
                   6,
{-# LINE 309 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
                   7] then do
{-# LINE 310 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
        (x :: Double) <- (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
{-# LINE 311 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
        (y :: Double) <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 312 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
        return (realToFrac x, realToFrac y)
      else if ty `elem` [ 31 ] then do
{-# LINE 314 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
        (x :: Double) <- (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
{-# LINE 315 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
        (y :: Double) <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 316 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
        return (realToFrac x, realToFrac y)
      else if ty `elem` [ 3 ] then do
{-# LINE 318 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
        (x :: Double) <- (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
{-# LINE 319 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
        (y :: Double) <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 320 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
        return (realToFrac x, realToFrac y)
      else if ty `elem` [ 10,
{-# LINE 322 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
                          11] then do
{-# LINE 323 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
        (x :: Double) <- (\hsc_ptr -> peekByteOff hsc_ptr 40) ptr
{-# LINE 324 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
        (y :: Double) <- (\hsc_ptr -> peekByteOff hsc_ptr 48) ptr
{-# LINE 325 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
        return (realToFrac x, realToFrac y)
      else error ("eventCoordinates: none for event type "++show ty)

class HasRootCoordinates a
instance HasRootCoordinates EButton
instance HasRootCoordinates EScroll
instance HasRootCoordinates EMotion
instance HasRootCoordinates ECrossing

-- | Retrieve the @(x,y)@ coordinates of the mouse relative to the
--   root (origin) of the screen.
eventRootCoordinates :: HasRootCoordinates t => EventM t (Double, Double)
eventRootCoordinates = do
  ptr <- ask
  liftIO $ do
    (ty :: Int32) <- peek (castPtr ptr)
{-# LINE 341 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
    if ty `elem` [ 4,
{-# LINE 342 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
                   5,
{-# LINE 343 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
                   6,
{-# LINE 344 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
                   7] then do
{-# LINE 345 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
        (x :: Double) <- (\hsc_ptr -> peekByteOff hsc_ptr 64) ptr
{-# LINE 346 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
        (y :: Double) <- (\hsc_ptr -> peekByteOff hsc_ptr 72) ptr
{-# LINE 347 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
        return (realToFrac x, realToFrac y)
      else if ty `elem` [ 31 ] then do
{-# LINE 349 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
        (x :: Double) <- (\hsc_ptr -> peekByteOff hsc_ptr 56) ptr
{-# LINE 350 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
        (y :: Double) <- (\hsc_ptr -> peekByteOff hsc_ptr 64) ptr
{-# LINE 351 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
        return (realToFrac x, realToFrac y)
      else if ty `elem` [ 3 ] then do
{-# LINE 353 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
        (x :: Double) <- (\hsc_ptr -> peekByteOff hsc_ptr 64) ptr
{-# LINE 354 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
        (y :: Double) <- (\hsc_ptr -> peekByteOff hsc_ptr 72) ptr
{-# LINE 355 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
        return (realToFrac x, realToFrac y)
      else if ty `elem` [ 10,
{-# LINE 357 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
                          11] then do
{-# LINE 358 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
        (x :: Double) <- (\hsc_ptr -> peekByteOff hsc_ptr 56) ptr
{-# LINE 359 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
        (y :: Double) <- (\hsc_ptr -> peekByteOff hsc_ptr 64) ptr
{-# LINE 360 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
        return (realToFrac x, realToFrac y)
      else error ("eventRootCoordinates: none for event type "++show ty)

class HasModifier a
instance HasModifier EKey
instance HasModifier EButton
instance HasModifier EScroll
instance HasModifier EMotion
instance HasModifier ECrossing

-- | Query the modifier keys that were depressed when the event happened.
--   Sticky modifiers such as CapsLock are omitted in the return value.
--   Use 'eventModifierAll' your application requires all modifiers.
--   Use 'eventModifierMouse' if you just need the mouse buttons.
--
eventModifier :: HasModifier t => EventM t [Modifier]
eventModifier = eM defModMask

-- | Query the modifier keys that were depressed when the event happened.
--   The result includes sticky modifiers such as CapsLock. Normally,
--   'eventModifier' is more appropriate in applications.
--
eventModifierAll :: HasModifier t => EventM t [Modifier]
eventModifierAll = eM allModMask

-- | Query the mouse buttons that were depressed when the event happened.
--
eventModifierMouse :: HasModifier t => EventM t [Modifier]
eventModifierMouse = eM mouseModMask

allModMask = -1

foreign import ccall safe "gtk_accelerator_get_default_mod_mask"
  defModMask :: Word32
{-# LINE 394 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}

mouseModMask = 256
{-# LINE 396 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
           .|. 512
{-# LINE 397 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
           .|. 1024
{-# LINE 398 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
           .|. 2048
{-# LINE 399 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
           .|. 4096
{-# LINE 400 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}

eM mask = do
  ptr <- ask
  liftIO $ do
    (ty :: Int32) <- peek (castPtr ptr)
{-# LINE 405 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
    if ty `elem` [ 8,
{-# LINE 406 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
                   9] then do
{-# LINE 407 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
        (modif ::Word32)    <- (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
{-# LINE 408 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
        return (toFlags (fromIntegral (modif .&. mask)))
      else if ty `elem` [ 4,
{-# LINE 410 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
                   5,
{-# LINE 411 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
                   6,
{-# LINE 412 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
                   7] then do
{-# LINE 413 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
        (modif ::Word32)    <- (\hsc_ptr -> peekByteOff hsc_ptr 48) ptr
{-# LINE 414 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
        return (toFlags (fromIntegral (modif .&. mask)))
      else if ty `elem` [ 31 ] then do
{-# LINE 416 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
        (modif ::Word32)    <- (\hsc_ptr -> peekByteOff hsc_ptr 40) ptr
{-# LINE 417 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
        return (toFlags (fromIntegral (modif .&. mask)))
      else if ty `elem` [ 3 ] then do
{-# LINE 419 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
        (modif ::Word32)    <- (\hsc_ptr -> peekByteOff hsc_ptr 48) ptr
{-# LINE 420 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
        return (toFlags (fromIntegral (modif .&. mask)))
      else if ty `elem` [ 10,
{-# LINE 422 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
                          11] then do
{-# LINE 423 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
        (modif ::Word32)    <- (\hsc_ptr -> peekByteOff hsc_ptr 84) ptr
{-# LINE 424 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
        return (toFlags (fromIntegral (modif .&. mask)))
      else error ("eventModifiers: none for event type "++show ty)

class HasTime a
instance HasTime EKey
instance HasTime EButton
instance HasTime EScroll
instance HasTime EMotion
instance HasTime ECrossing
instance HasTime EProperty
instance HasTime EProximity

{-# LINE 436 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
instance HasTime EOwnerChange

{-# LINE 438 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}

-- | The time (in milliseconds) when an event happened. This is used mostly
-- for ordering events and responses to events.
--
type TimeStamp = Word32
-- TODO: make this a newtype

-- | Represents the current time, and can be used anywhere a time is expected.
currentTime :: TimeStamp
currentTime = 0
{-# LINE 448 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}

-- | Query the time when the event occurred.
eventTime :: HasTime t => EventM t TimeStamp
eventTime = do
  ptr <- ask
  liftIO $ do
    (ty :: Int32) <- peek (castPtr ptr)
{-# LINE 455 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
    if ty `elem` [ 8,
{-# LINE 456 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
                   9] then do
{-# LINE 457 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
        (time :: Word32) <- (\hsc_ptr -> peekByteOff hsc_ptr 20) ptr
{-# LINE 458 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
        return (fromIntegral time)
      else if ty `elem` [ 4,
{-# LINE 460 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
                   5,
{-# LINE 461 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
                   6,
{-# LINE 462 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
                   7] then do
{-# LINE 463 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
        (time :: Word32) <- (\hsc_ptr -> peekByteOff hsc_ptr 20) ptr
{-# LINE 464 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
        return (fromIntegral time)
      else if ty `elem` [ 31 ] then do
{-# LINE 466 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
        (time :: Word32) <- (\hsc_ptr -> peekByteOff hsc_ptr 20) ptr
{-# LINE 467 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
        return (fromIntegral time)
      else if ty `elem` [ 3 ] then do
{-# LINE 469 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
        (time :: Word32) <- (\hsc_ptr -> peekByteOff hsc_ptr 20) ptr
{-# LINE 470 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
        return (fromIntegral time)
      else if ty `elem` [ 10,
{-# LINE 472 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
                          11] then do
{-# LINE 473 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
        (time :: Word32) <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 474 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
        return (fromIntegral time)
      else if ty `elem` [ 16 ] then do
{-# LINE 476 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
        (time :: Word32) <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 477 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
        return (fromIntegral time)
      else if ty `elem` [ 20,
{-# LINE 479 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
                          21] then do
{-# LINE 480 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
        (time :: Word32) <- (\hsc_ptr -> peekByteOff hsc_ptr 20) ptr
{-# LINE 481 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
        return (fromIntegral time)

{-# LINE 483 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
      else if ty `elem` [ 34 ] then do
{-# LINE 484 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
        (time :: Word32) <- (\hsc_ptr -> peekByteOff hsc_ptr 48) ptr
{-# LINE 485 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
        return (fromIntegral time)

{-# LINE 487 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
      else error ("eventModifiers: none for event type "++show ty)

-- | The key value. See 'Graphics.UI.Gtk.Gdk.Keys.KeyVal'.
eventKeyVal :: EventM EKey KeyVal
eventKeyVal = ask >>= \ptr -> liftIO $ liftM fromIntegral
  ((\hsc_ptr -> peekByteOff hsc_ptr 28) ptr :: IO Word32)
{-# LINE 493 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}

-- | The key value as a string. See 'Graphics.UI.Gtk.Gdk.Keys.KeyVal'.
eventKeyName :: EventM EKey DefaultGlibString
eventKeyName = liftM keyName $ eventKeyVal

-- | The hardware key code.
eventHardwareKeycode :: EventM EKey KeyCode
eventHardwareKeycode = ask >>= \ptr -> liftIO $ liftM fromIntegral
  ((\hsc_ptr -> peekByteOff hsc_ptr 48) ptr :: IO Word16)
{-# LINE 502 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}

-- | The keyboard group.
eventKeyboardGroup :: EventM EKey Word8
eventKeyboardGroup = ask >>= \ptr -> liftIO $ liftM fromIntegral
  ((\hsc_ptr -> peekByteOff hsc_ptr 50) ptr :: IO Word8)
{-# LINE 507 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}

-- | Query the mouse buttons.
eventButton :: EventM EButton MouseButton
eventButton = ask >>= \ptr -> liftIO $ liftM (toEnum . fromIntegral)
  ((\hsc_ptr -> peekByteOff hsc_ptr 52) ptr :: IO Word32)
{-# LINE 512 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}

--- | Query the mouse click.
eventClick :: EventM EButton Click
eventClick = do
  ptr <- ask
  liftIO $ do
    (ty :: Int32) <- peek (castPtr ptr)
{-# LINE 519 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
    case ty of
      4 -> return SingleClick
{-# LINE 521 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
      5 -> return DoubleClick
{-# LINE 522 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
      6 -> return TripleClick
{-# LINE 523 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
      7 -> return ReleaseClick
{-# LINE 524 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
      _ -> error ("eventClick: non for event type "++show ty)

-- | Query the direction of scrolling.
eventScrollDirection :: EventM EScroll ScrollDirection
eventScrollDirection = ask >>= \ptr -> liftIO $ liftM (toEnum . fromIntegral)
  ((\hsc_ptr -> peekByteOff hsc_ptr 44) ptr :: IO Word32)
{-# LINE 530 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}

-- | Check if the motion event is only a hint rather than the full mouse
--   movement information.
eventIsHint :: EventM EMotion Bool
eventIsHint = ask >>= \ptr -> liftIO $ liftM toBool
  ((\hsc_ptr -> peekByteOff hsc_ptr 52) ptr :: IO Int16)
{-# LINE 536 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}


{-# LINE 538 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
-- | Request more motion notifies if this event is a motion notify hint event.
--
-- This action should be used instead of 'drawWindowGetPointer' to request
-- further motion notifies, because it also works for extension events where
-- motion notifies are provided for devices other than the core pointer.
--
-- Coordinate extraction, processing and requesting more motion events from a
-- 'motionNotifyEvent' usually works like this:
--
-- > on widget motionNotifyEvent $ do
-- >   (x, y) <- eventCoordinates
-- >   -- handle the x,y motion:
-- >   ...
-- >   -- finally, notify that we are ready to get more motion events:
-- >   eventRequestMotions
--
eventRequestMotions :: EventM EMotion ()
eventRequestMotions = ask >>= \ptr -> liftIO $
  gdk_event_request_motions ptr

foreign import ccall "gdk_event_request_motions"
  gdk_event_request_motions :: Ptr EMotion -> IO ()

{-# LINE 561 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}

-- | Query a bounding box of the region that needs to be updated.
eventArea :: EventM EExpose Rectangle
eventArea = ask >>= \ptr -> liftIO $
  ((\hsc_ptr -> peekByteOff hsc_ptr 20) ptr :: IO Rectangle)
{-# LINE 566 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}


{-# LINE 568 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
-- | Query the region that needs to be updated.
-- Removed in Gtk3.
eventRegion :: EventM EExpose Region
eventRegion = ask >>= \ptr -> liftIO $ do
  (reg_   :: Ptr Region)        <- (\hsc_ptr -> peekByteOff hsc_ptr 40) ptr
{-# LINE 573 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
  reg_ <- gdk_region_copy reg_
  makeNewRegion reg_

foreign import ccall "gdk_region_copy"
  gdk_region_copy :: Ptr Region -> IO (Ptr Region)

{-# LINE 579 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}

-- | Get the visibility status of a window.
eventVisibilityState :: EventM EVisibility VisibilityState
eventVisibilityState = ask >>= \ptr -> liftIO $ liftM (toEnum . fromIntegral)
  ((\hsc_ptr -> peekByteOff hsc_ptr 20) ptr :: IO Word32)
{-# LINE 584 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}

-- | Get the mode of the mouse cursor crossing a window.
eventCrossingMode :: EventM ECrossing CrossingMode
eventCrossingMode = ask >>= \ptr -> liftIO $ liftM (toEnum . fromIntegral)
  ((\hsc_ptr -> peekByteOff hsc_ptr 72) ptr :: IO Word32)
{-# LINE 589 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}

-- | Get the notify type of the mouse cursor crossing a window.
eventNotifyType :: EventM ECrossing NotifyType
eventNotifyType = ask >>= \ptr -> liftIO $ liftM (toEnum . fromIntegral)
  ((\hsc_ptr -> peekByteOff hsc_ptr 76) ptr :: IO Word32)
{-# LINE 594 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}

-- | Query if the window has the focus or is an inferior window.
eventCrossingFocus :: EventM ECrossing Bool
eventCrossingFocus = ask >>= \ptr -> liftIO $ liftM toBool
  ((\hsc_ptr -> peekByteOff hsc_ptr 80) ptr :: IO Int32)
{-# LINE 599 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}

-- | Query if a window gained focus (@True@) or lost the focus (@False@).
eventFocusIn :: EventM EFocus Bool
eventFocusIn = ask >>= \ptr -> liftIO $ liftM toBool
  ((\hsc_ptr -> peekByteOff hsc_ptr 18) ptr :: IO Int16)
{-# LINE 604 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}

-- | Get the @(x,y)@ position of the window within the parent window.
eventPosition :: EventM EConfigure (Int,Int)
eventPosition = ask >>= \ptr -> liftIO $ do
  (x :: Int32)    <- (\hsc_ptr -> peekByteOff hsc_ptr 20) ptr
{-# LINE 609 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
  (y :: Int32)    <- (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
{-# LINE 610 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
  return (fromIntegral x, fromIntegral y)

-- | Get the new size of the window as @(width,height)@.
eventSize :: EventM EConfigure (Int,Int)
eventSize = ask >>= \ptr -> liftIO $ do
  (x :: Int32)    <- (\hsc_ptr -> peekByteOff hsc_ptr 28) ptr
{-# LINE 616 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
  (y :: Int32)    <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 617 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
  return (fromIntegral x, fromIntegral y)

eventProperty :: EventM EProperty Atom
eventProperty = ask >>= \ptr -> liftIO $ liftM Atom
  ((\hsc_ptr -> peekByteOff hsc_ptr 24) ptr :: IO (Ptr ()))
{-# LINE 622 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}

-- | Query which window state bits have changed.
eventWindowStateChanged :: EventM EWindowState [WindowState]
eventWindowStateChanged = ask >>= \ptr -> liftIO $ liftM (toFlags . fromIntegral)
  ((\hsc_ptr -> peekByteOff hsc_ptr 20) ptr :: IO Word32)
{-# LINE 627 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}

-- | Query the new window state.
eventWindowState :: EventM EWindowState [WindowState]
eventWindowState = ask >>= \ptr -> liftIO $ liftM (toFlags . fromIntegral)
  ((\hsc_ptr -> peekByteOff hsc_ptr 24) ptr :: IO Word32)
{-# LINE 632 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}


{-# LINE 634 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
-- | Query why a seleciton changed its owner.
eventChangeReason :: EventM EOwnerChange OwnerChange
eventChangeReason = ask >>= \ptr -> liftIO $ liftM (toEnum . fromIntegral)
  ((\hsc_ptr -> peekByteOff hsc_ptr 32) ptr :: IO Word32)
{-# LINE 638 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}

-- | Query what selection changed its owner.
eventSelection :: EventM EOwnerChange SelectionTag
eventSelection = ask >>= \ptr -> liftIO $ liftM Atom
  ((\hsc_ptr -> peekByteOff hsc_ptr 40) ptr :: IO (Ptr ()))
{-# LINE 643 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}

-- | Query the time when the selection was taken over.
eventSelectionTime :: EventM EOwnerChange TimeStamp
eventSelectionTime = ask >>= \ptr -> liftIO $ liftM fromIntegral
  ((\hsc_ptr -> peekByteOff hsc_ptr 52) ptr :: IO (Word32))
{-# LINE 648 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}

{-# LINE 649 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}


{-# LINE 651 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
-- | Check if a keyboard (@True@) or a mouse pointer grap (@False@) was
--   broken.
eventKeyboardGrab :: EventM EGrabBroken Bool
eventKeyboardGrab = ask >>= \ptr -> liftIO $ liftM toBool
  ((\hsc_ptr -> peekByteOff hsc_ptr 20) ptr :: IO Int32)
{-# LINE 656 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}

-- | Check if a grab was broken implicitly.
eventImplicit :: EventM EGrabBroken Bool
eventImplicit = ask >>= \ptr -> liftIO $ liftM toBool
  ((\hsc_ptr -> peekByteOff hsc_ptr 24) ptr :: IO Int32)
{-# LINE 661 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}

-- | Get the new window that owns the grab or @Nothing@ if the window
--   is not part of this application.
eventGrabWindow :: EventM EGrabBroken (Maybe DrawWindow)
eventGrabWindow = do
  ptr <- ask
  liftIO $ maybeNull (makeNewGObject mkDrawWindow) ((\hsc_ptr -> peekByteOff hsc_ptr 8) ptr)
{-# LINE 668 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}

{-# LINE 669 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}


-- | Execute an event handler and assume it handled the event unless it
--   threw a pattern match exception or calls mzero (e.g. via guard).
tryEvent :: EventM any () -> EventM any Bool
tryEvent act = do
  ptr <- ask
  liftIO $ (runReaderT (act >> return True) ptr)

{-# LINE 678 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}
    `catches` [ Handler (\ (PatternMatchFail _) -> return False)
              , Handler (\ e -> if isUserError e &&
                                   ("Pattern" `isPrefixOf` ioeGetErrorString e ||
                                    "mzero" == ioeGetErrorString e)
                                then return False
                                else throw e) ]

{-# LINE 694 "Graphics/UI/Gtk/Gdk/EventM.hsc" #-}


-- | Explicitly stop the handling of an event. This function should only be
--   called inside a handler that is wrapped with 'tryEvent'. (It merely
--   throws a bogus pattern matching error which 'tryEvent' interprets as if
--   the handler does not handle the event.)
stopEvent :: EventM any ()
stopEvent =
  liftIO $ throw (PatternMatchFail "EventM.stopEvent called explicitly")
