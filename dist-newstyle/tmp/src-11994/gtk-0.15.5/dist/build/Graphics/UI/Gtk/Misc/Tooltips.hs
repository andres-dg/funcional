
{-# LINE 2 "./Graphics/UI/Gtk/Misc/Tooltips.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget Tooltips
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
-- Add tips to your widgets
--
module Graphics.UI.Gtk.Misc.Tooltips (
-- * Detail
--
-- | Tooltips are the messages that appear next to a widget when the mouse
-- pointer is held over it for a short amount of time. They are especially
-- helpful for adding more verbose descriptions of things such as buttons in a
-- toolbar.
--
-- An individual tooltip belongs to a group of tooltips. A group is created
-- with a call to 'tooltipsNew'. Every tooltip in the group can then be turned
-- off with a call to 'tooltipsDisable' and enabled with 'tooltipsEnable'.
--

-- The length of time the user must keep the mouse over a widget before the
-- tip is shown, can be altered with 'tooltipsSetDelay'. This is set on a \'per
-- group of tooltips\' basis.
--

-- To assign a tip to a particular 'Widget', 'tooltipsSetTip' is used.
--
-- To associate 'Tooltips' to a widget it is has to have its own 'DrawWindow'.
-- Otherwise the widget must be set into an 'EventBox'.
--
-- The default appearance of all tooltips in a program is determined by the
-- current Gtk+ theme that the user has selected.
--
-- Information about the tooltip (if any) associated with an arbitrary
-- widget can be retrieved using 'tooltipsDataGet'.
--
-- * This module is deprecated. It is empty in Gtk3.

-- * Class Hierarchy
-- |
-- @
-- | 'GObject'
-- | +----'Object'
-- | +----Tooltips
-- @

-- * Types
  Tooltips,
  TooltipsClass,
  castToTooltips, gTypeTooltips,
  toTooltips,

-- * Constructors
  tooltipsNew,

-- * Methods
  tooltipsEnable,
  tooltipsDisable,

  tooltipsSetDelay,

  tooltipsSetTip,
  tooltipsDataGet

  ) where


import System.Glib.FFI
import System.Glib.UTFString
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.UI.Gtk.Types
{-# LINE 92 "./Graphics/UI/Gtk/Misc/Tooltips.chs" #-}


{-# LINE 94 "./Graphics/UI/Gtk/Misc/Tooltips.chs" #-}

--------------------
-- Constructors

-- | Create a new goup of 'Tooltips'.
--
tooltipsNew :: IO Tooltips
tooltipsNew =
  makeNewObject mkTooltips $
  gtk_tooltips_new
{-# LINE 104 "./Graphics/UI/Gtk/Misc/Tooltips.chs" #-}

--------------------
-- Methods

-- | Allows the user to see your tooltips as they navigate your application.
--
tooltipsEnable :: TooltipsClass self => self -> IO ()
tooltipsEnable self =
  (\(Tooltips arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tooltips_enable argPtr1)
{-# LINE 113 "./Graphics/UI/Gtk/Misc/Tooltips.chs" #-}
    (toTooltips self)

-- | Causes all tooltips in @tooltips@ to become inactive. Any widgets that
-- have tips associated with that group will no longer display their tips until
-- they are enabled again with 'tooltipsEnable'.
--
tooltipsDisable :: TooltipsClass self => self -> IO ()
tooltipsDisable self =
  (\(Tooltips arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tooltips_disable argPtr1)
{-# LINE 122 "./Graphics/UI/Gtk/Misc/Tooltips.chs" #-}
    (toTooltips self)


-- | Sets the time between the user moving the mouse over a widget and the
-- widget's tooltip appearing.
--
-- * Warning: this function is deprecated and should not be used in
-- newly-written code.
--
tooltipsSetDelay :: TooltipsClass self => self
 -> Int -- ^ @delay@ - the delay in milliseconds
 -> IO ()
tooltipsSetDelay self delay =
  (\(Tooltips arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tooltips_set_delay argPtr1 arg2)
{-# LINE 136 "./Graphics/UI/Gtk/Misc/Tooltips.chs" #-}
    (toTooltips self)
    (fromIntegral delay)


-- | Adds a tooltip containing the message @tipText@ to the specified
-- 'Widget'.
--
tooltipsSetTip :: (TooltipsClass self, WidgetClass widget, GlibString string) => self
 -> widget -- ^ @widget@ - the 'Widget' you wish to associate the tip with.
 -> string -- ^ @tipText@ - a string containing the tip itself.
 -> string -- ^ @tipPrivate@ - a string of any further information that may be
           -- useful if the user gets stuck.
 -> IO ()
tooltipsSetTip self widget tipText tipPrivate =
  withUTFString tipPrivate $ \tipPrivatePtr ->
  withUTFString tipText $ \tipTextPtr ->
  (\(Tooltips arg1) (Widget arg2) arg3 arg4 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_tooltips_set_tip argPtr1 argPtr2 arg3 arg4)
{-# LINE 153 "./Graphics/UI/Gtk/Misc/Tooltips.chs" #-}
    (toTooltips self)
    (toWidget widget)
    tipTextPtr
    tipPrivatePtr

-- | Retrieves any 'Tooltips' previously associated with the given widget.
--
tooltipsDataGet :: (WidgetClass w, GlibString string) => w -> IO (Maybe (Tooltips, string, string))
tooltipsDataGet w = do
  tipDataPtr <- (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tooltips_data_get argPtr1) (toWidget w)
  if tipDataPtr == nullPtr
    then return Nothing
    else do --next line is a hack, tooltips struct member is at offset 0
           tooltips <- makeNewObject mkTooltips (return $ castPtr tipDataPtr)
           tipText <- (\ptr -> do {peekByteOff ptr 16 ::IO (Ptr CChar)}) tipDataPtr
                   >>= peekUTFString
           tipPrivate <- (\ptr -> do {peekByteOff ptr 24 ::IO (Ptr CChar)}) tipDataPtr
                     >>= peekUTFString
           return $ Just $ (tooltips, tipText, tipPrivate)

foreign import ccall unsafe "gtk_tooltips_new"
  gtk_tooltips_new :: (IO (Ptr Tooltips))

foreign import ccall unsafe "gtk_tooltips_enable"
  gtk_tooltips_enable :: ((Ptr Tooltips) -> (IO ()))

foreign import ccall unsafe "gtk_tooltips_disable"
  gtk_tooltips_disable :: ((Ptr Tooltips) -> (IO ()))

foreign import ccall unsafe "gtk_tooltips_set_delay"
  gtk_tooltips_set_delay :: ((Ptr Tooltips) -> (CUInt -> (IO ())))

foreign import ccall unsafe "gtk_tooltips_set_tip"
  gtk_tooltips_set_tip :: ((Ptr Tooltips) -> ((Ptr Widget) -> ((Ptr CChar) -> ((Ptr CChar) -> (IO ())))))

foreign import ccall unsafe "gtk_tooltips_data_get"
  gtk_tooltips_data_get :: ((Ptr Widget) -> (IO (Ptr ())))
