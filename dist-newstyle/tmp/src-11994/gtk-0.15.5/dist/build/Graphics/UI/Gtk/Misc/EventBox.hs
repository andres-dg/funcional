
{-# LINE 2 "./Graphics/UI/Gtk/Misc/EventBox.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget EventBox
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
-- A widget used to catch events for widgets which do not have their own
-- window
--
module Graphics.UI.Gtk.Misc.EventBox (
-- * Detail
--
-- | The 'EventBox' widget is a subclass of 'Bin' which also has its own
-- window. It is useful since it allows you to catch events for widgets which
-- do not have their own window.

-- * Class Hierarchy
-- |
-- @
-- | 'GObject'
-- | +----'Object'
-- | +----'Widget'
-- | +----'Container'
-- | +----'Bin'
-- | +----EventBox
-- @

-- * Types
  EventBox,
  EventBoxClass,
  castToEventBox, gTypeEventBox,
  toEventBox,

-- * Constructors
  eventBoxNew,

-- * Methods

  eventBoxSetVisibleWindow,
  eventBoxGetVisibleWindow,
  eventBoxSetAboveChild,
  eventBoxGetAboveChild,

-- * Attributes
  eventBoxVisibleWindow,
  eventBoxAboveChild,

  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.Attributes
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.UI.Gtk.Types
{-# LINE 75 "./Graphics/UI/Gtk/Misc/EventBox.chs" #-}


{-# LINE 77 "./Graphics/UI/Gtk/Misc/EventBox.chs" #-}

--------------------
-- Constructors

-- | Creates a new 'EventBox'.
--
eventBoxNew :: IO EventBox
eventBoxNew =
  makeNewObject mkEventBox $
  liftM (castPtr :: Ptr Widget -> Ptr EventBox) $
  gtk_event_box_new
{-# LINE 88 "./Graphics/UI/Gtk/Misc/EventBox.chs" #-}

--------------------
-- Methods


-- | Set whether the event box uses a visible or invisible child window. The
-- default is to use visible windows.
--
-- In an invisible window event box, the window that that the event box
-- creates is a \"input only\" window, which means that it is invisible and only
-- serves to receive events.
--
-- A visible window event box creates a visible (\"input output\") window that
-- acts as the parent window for all the widgets contained in the event box.
--
-- You should generally make your event box invisible if you just want to
-- trap events. Creating a visible window may cause artifacts that are visible
-- to the user, especially if the user is using a theme with gradients or
-- pixmaps.
--
-- The main reason to create a non input-only event box is if you want to
-- set the background to a different color or draw on it.
--
-- * Available since Gtk+ version 2.4
--
eventBoxSetVisibleWindow :: EventBox -> Bool -> IO ()
eventBoxSetVisibleWindow self visibleWindow =
  (\(EventBox arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_event_box_set_visible_window argPtr1 arg2)
{-# LINE 116 "./Graphics/UI/Gtk/Misc/EventBox.chs" #-}
    self
    (fromBool visibleWindow)

-- | Returns whether the event box has a visible window. See
-- 'eventBoxSetVisibleWindow' for details.
--
-- * Available since Gtk+ version 2.4
--
eventBoxGetVisibleWindow :: EventBox -> IO Bool
eventBoxGetVisibleWindow self =
  liftM toBool $
  (\(EventBox arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_event_box_get_visible_window argPtr1)
{-# LINE 128 "./Graphics/UI/Gtk/Misc/EventBox.chs" #-}
    self

-- | Set whether the event box window is positioned above the windows of its
-- child, as opposed to below it. If the window is above, all events inside the
-- event box will go to the event box. If the window is below, events in
-- windows of child widgets will first got to that widget, and then to its
-- parents.
--
-- The default is to keep the window below the child.
--
-- * Available since Gtk+ version 2.4
--
eventBoxSetAboveChild :: EventBox -> Bool -> IO ()
eventBoxSetAboveChild self aboveChild =
  (\(EventBox arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_event_box_set_above_child argPtr1 arg2)
{-# LINE 143 "./Graphics/UI/Gtk/Misc/EventBox.chs" #-}
    self
    (fromBool aboveChild)

-- | Returns whether the event box window is above or below the windows of its
-- child. See 'eventBoxSetAboveChild' for details.
--
-- * Available since Gtk+ version 2.4
--
eventBoxGetAboveChild :: EventBox -> IO Bool
eventBoxGetAboveChild self =
  liftM toBool $
  (\(EventBox arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_event_box_get_above_child argPtr1)
{-# LINE 155 "./Graphics/UI/Gtk/Misc/EventBox.chs" #-}
    self

--------------------
-- Attributes

-- | Whether the event box is visible, as opposed to invisible and only used
-- to trap events.
--
-- Default value: @True@
--
eventBoxVisibleWindow :: Attr EventBox Bool
eventBoxVisibleWindow = newAttr
  eventBoxGetVisibleWindow
  eventBoxSetVisibleWindow

-- | Whether the event-trapping window of the eventbox is above the window of
-- the child widget as opposed to below it.
--
-- Default value: @False@
--
eventBoxAboveChild :: Attr EventBox Bool
eventBoxAboveChild = newAttr
  eventBoxGetAboveChild
  eventBoxSetAboveChild

foreign import ccall unsafe "gtk_event_box_new"
  gtk_event_box_new :: (IO (Ptr Widget))

foreign import ccall safe "gtk_event_box_set_visible_window"
  gtk_event_box_set_visible_window :: ((Ptr EventBox) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_event_box_get_visible_window"
  gtk_event_box_get_visible_window :: ((Ptr EventBox) -> (IO CInt))

foreign import ccall safe "gtk_event_box_set_above_child"
  gtk_event_box_set_above_child :: ((Ptr EventBox) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_event_box_get_above_child"
  gtk_event_box_get_above_child :: ((Ptr EventBox) -> (IO CInt))
