
{-# LINE 2 "./Graphics/UI/Gtk/Layout/Fixed.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget Fixed
--
-- Author : Duncan Coutts
--
-- Created: 2 August 2004
--
-- Copyright (C) 2004-2005 Duncan Coutts
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
-- A container which allows you to position widgets at fixed coordinates
--
module Graphics.UI.Gtk.Layout.Fixed (
-- * Detail
--
-- | The 'Fixed' widget is a container which can place child widgets at fixed
-- positions and with fixed sizes, given in pixels. 'Fixed' performs no
-- automatic layout management.
--
-- For most applications, you should not use this container! It keeps you
-- from having to learn about the other Gtk+ containers, but it results in
-- broken applications. With 'Fixed', the following things will result in
-- truncated text, overlapping widgets, and other display bugs:
--
-- * Themes, which may change widget sizes.
--
-- * Fonts other than the one you used to write the app will of course
-- change the size of widgets containing text; keep in mind that users may use
-- a larger font because of difficulty reading the default, or they may be
-- using Windows or the framebuffer port of Gtk+, where different fonts are
-- available.
--
-- * Translation of text into other languages changes its size. Also,
-- display of non-English text will use a different font in many cases.
--
-- In addition, the fixed widget can't properly be mirrored in right-to-left
-- languages such as Hebrew and Arabic. i.e. normally Gtk+ will flip the
-- interface to put labels to the right of the thing they label, but it can't
-- do that with 'Fixed'. So your application will not be usable in
-- right-to-left languages.
--
-- Finally, fixed positioning makes it kind of annoying to add\/remove GUI
-- elements, since you have to reposition all the other elements. This is a
-- long-term maintenance problem for your application.
--
-- If you know none of these things are an issue for your application, and
-- prefer the simplicity of 'Fixed', by all means use the widget. But you
-- should be aware of the tradeoffs.

-- * Class Hierarchy
-- |
-- @
-- | 'GObject'
-- | +----'Object'
-- | +----'Widget'
-- | +----'Container'
-- | +----Fixed
-- @

-- * Types
  Fixed,
  FixedClass,
  castToFixed, gTypeFixed,
  toFixed,

-- * Constructors
  fixedNew,

-- * Methods
  fixedPut,
  fixedMove,

  fixedSetHasWindow,
  fixedGetHasWindow,

-- * Attributes
  fixedHasWindow,

-- * Child Attributes
  fixedChildX,
  fixedChildY,
  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.Attributes
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.UI.Gtk.Types
{-# LINE 105 "./Graphics/UI/Gtk/Layout/Fixed.chs" #-}
import Graphics.UI.Gtk.Abstract.ContainerChildProperties


{-# LINE 108 "./Graphics/UI/Gtk/Layout/Fixed.chs" #-}

--------------------
-- Constructors

-- | Creates a new 'Fixed'.
--
fixedNew :: IO Fixed
fixedNew =
  makeNewObject mkFixed $
  liftM (castPtr :: Ptr Widget -> Ptr Fixed) $
  gtk_fixed_new
{-# LINE 119 "./Graphics/UI/Gtk/Layout/Fixed.chs" #-}

--------------------
-- Methods

-- | Adds a widget to a 'Fixed' container at the given position.
--
fixedPut :: (FixedClass self, WidgetClass widget) => self
 -> widget -- ^ @widget@ - the widget to add.
 -> (Int, Int) -- ^ @(x,y)@ - the horizontal and vertical position to place
               -- the widget at.
 -> IO ()
fixedPut self widget (x, y) =
  (\(Fixed arg1) (Widget arg2) arg3 arg4 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_fixed_put argPtr1 argPtr2 arg3 arg4)
{-# LINE 132 "./Graphics/UI/Gtk/Layout/Fixed.chs" #-}
    (toFixed self)
    (toWidget widget)
    (fromIntegral x)
    (fromIntegral y)

-- | Moves a child of a 'Fixed' container to the given position.
--
fixedMove :: (FixedClass self, WidgetClass widget) => self
 -> widget -- ^ @widget@ - the child widget.
 -> (Int, Int) -- ^ @(x,y)@ - the horizontal and vertical position to move the
               -- widget to.
 -> IO ()
fixedMove self widget (x, y) =
  (\(Fixed arg1) (Widget arg2) arg3 arg4 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_fixed_move argPtr1 argPtr2 arg3 arg4)
{-# LINE 146 "./Graphics/UI/Gtk/Layout/Fixed.chs" #-}
    (toFixed self)
    (toWidget widget)
    (fromIntegral x)
    (fromIntegral y)


-- | Sets whether the 'Fixed' widget is created with a separate 'DrawWindow' for
-- its window or not. (By default, it will be created with no separate
-- 'DrawWindow'). This function must be called while the 'Fixed' is not
-- realized, for instance, immediately after the window is created.
--
-- This function was added to provide an easy migration path for older
-- applications which may expect 'Fixed' to have a separate window.
--
-- Removed in Gtk3. Use the Widget version.
fixedSetHasWindow :: FixedClass self => self -> Bool -> IO ()
fixedSetHasWindow self hasWindow =
  (\(Fixed arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_fixed_set_has_window argPtr1 arg2)
{-# LINE 164 "./Graphics/UI/Gtk/Layout/Fixed.chs" #-}
    (toFixed self)
    (fromBool hasWindow)

-- | Gets whether the 'Fixed' has its own 'DrawWindow'. See
-- 'fixedSetHasWindow'.
--
-- Removed in Gtk3. Use the Widget version.
fixedGetHasWindow :: FixedClass self => self -> IO Bool
fixedGetHasWindow self =
  liftM toBool $
  (\(Fixed arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_fixed_get_has_window argPtr1)
{-# LINE 175 "./Graphics/UI/Gtk/Layout/Fixed.chs" #-}
    (toFixed self)

--------------------
-- Attributes

-- | \'hasWindow\' property. See 'fixedGetHasWindow' and 'fixedSetHasWindow'
--
-- Removed in Gtk3. Use the Widget version.
fixedHasWindow :: FixedClass self => Attr self Bool
fixedHasWindow = newAttr
  fixedGetHasWindow
  fixedSetHasWindow

--------------------
-- Child Attributes

-- | X position of child widget.
--
-- Default value: 0
--
fixedChildX :: (FixedClass self, WidgetClass child) => child -> Attr self Int
fixedChildX = newAttrFromContainerChildIntProperty "x"

-- | Y position of child widget.
--
-- Default value: 0
--
fixedChildY :: (FixedClass self, WidgetClass child) => child -> Attr self Int
fixedChildY = newAttrFromContainerChildIntProperty "y"

foreign import ccall unsafe "gtk_fixed_new"
  gtk_fixed_new :: (IO (Ptr Widget))

foreign import ccall safe "gtk_fixed_put"
  gtk_fixed_put :: ((Ptr Fixed) -> ((Ptr Widget) -> (CInt -> (CInt -> (IO ())))))

foreign import ccall safe "gtk_fixed_move"
  gtk_fixed_move :: ((Ptr Fixed) -> ((Ptr Widget) -> (CInt -> (CInt -> (IO ())))))

foreign import ccall safe "gtk_fixed_set_has_window"
  gtk_fixed_set_has_window :: ((Ptr Fixed) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_fixed_get_has_window"
  gtk_fixed_get_has_window :: ((Ptr Fixed) -> (IO CInt))
