
{-# LINE 2 "./Graphics/UI/Gtk/Layout/Layout.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget Layout
--
-- Author : Axel Simon
--
-- Created: 15 May 2001
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
-- Infinite scrollable area containing child widgets and\/or custom drawing
--
module Graphics.UI.Gtk.Layout.Layout (
-- * Detail
--
-- | 'Layout' is similar to 'DrawingArea' in that it's a \"blank slate\" and
-- doesn't do anything but paint a blank background by default. It's different
-- in that it supports scrolling natively (you can add it to a
-- 'ScrolledWindow'), and it can contain child widgets, since it's a
-- 'Container'. However if you\'re just going to draw, a 'DrawingArea' is a
-- better choice since it has lower overhead.

-- * Class Hierarchy
-- |
-- @
-- | 'GObject'
-- | +----'Object'
-- | +----'Widget'
-- | +----'Container'
-- | +----Layout
-- @

-- * Types
  Layout,
  LayoutClass,
  castToLayout, gTypeLayout,
  toLayout,

-- * Constructors
  layoutNew,

-- * Methods
  layoutPut,
  layoutMove,
  layoutSetSize,
  layoutGetSize,
  layoutGetHAdjustment,
  layoutGetVAdjustment,
  layoutSetHAdjustment,
  layoutSetVAdjustment,
  layoutGetDrawWindow,

-- * Attributes
  layoutHAdjustment,
  layoutVAdjustment,
  layoutWidth,
  layoutHeight,

-- * Child Attributes
  layoutChildX,
  layoutChildY,

-- * Signals
  onSetScrollAdjustments,
  afterSetScrollAdjustments,
  ) where

import Data.Maybe (fromMaybe)
import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.UI.Gtk.Types
{-# LINE 91 "./Graphics/UI/Gtk/Layout/Layout.chs" #-}
import Graphics.UI.Gtk.Signals
{-# LINE 92 "./Graphics/UI/Gtk/Layout/Layout.chs" #-}
import Graphics.UI.Gtk.Abstract.ContainerChildProperties


{-# LINE 95 "./Graphics/UI/Gtk/Layout/Layout.chs" #-}

--------------------
-- Constructors

-- | Creates a new 'Layout'. Unless you have a specific adjustment you'd like
-- the layout to use for scrolling, pass @Nothing@ for @hadjustment@ and
-- @vadjustment@.
--
layoutNew ::
    Maybe Adjustment -- ^ @hadjustment@ - horizontal scroll adjustment, or
                     -- @Nothing@
 -> Maybe Adjustment -- ^ @vadjustment@ - vertical scroll adjustment, or
                     -- @Nothing@
 -> IO Layout
layoutNew hadjustment vadjustment =
  makeNewObject mkLayout $
  liftM (castPtr :: Ptr Widget -> Ptr Layout) $
  (\(Adjustment arg1) (Adjustment arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_layout_new argPtr1 argPtr2)
{-# LINE 113 "./Graphics/UI/Gtk/Layout/Layout.chs" #-}
    (fromMaybe (Adjustment nullForeignPtr) hadjustment)
    (fromMaybe (Adjustment nullForeignPtr) vadjustment)

--------------------
-- Methods

-- | Adds @childWidget@ to @layout@, at position @(x,y)@. @layout@ becomes
-- the new parent container of @childWidget@.
--
layoutPut :: (LayoutClass self, WidgetClass childWidget) => self
 -> childWidget -- ^ @childWidget@ - child widget
 -> Int -- ^ @x@ - X position of child widget
 -> Int -- ^ @y@ - Y position of child widget
 -> IO ()
layoutPut self childWidget x y =
  (\(Layout arg1) (Widget arg2) arg3 arg4 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_layout_put argPtr1 argPtr2 arg3 arg4)
{-# LINE 129 "./Graphics/UI/Gtk/Layout/Layout.chs" #-}
    (toLayout self)
    (toWidget childWidget)
    (fromIntegral x)
    (fromIntegral y)

-- | Moves a current child of @layout@ to a new position.
--
layoutMove :: (LayoutClass self, WidgetClass childWidget) => self
 -> childWidget -- ^ @childWidget@ - a current child of @layout@
 -> Int -- ^ @x@ - X position to move to
 -> Int -- ^ @y@ - Y position to move to
 -> IO ()
layoutMove self childWidget x y =
  (\(Layout arg1) (Widget arg2) arg3 arg4 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_layout_move argPtr1 argPtr2 arg3 arg4)
{-# LINE 143 "./Graphics/UI/Gtk/Layout/Layout.chs" #-}
    (toLayout self)
    (toWidget childWidget)
    (fromIntegral x)
    (fromIntegral y)

-- | Sets the size of the scrollable area of the layout.
--
layoutSetSize :: LayoutClass self => self
 -> Int -- ^ @width@ - width of entire scrollable area
 -> Int -- ^ @height@ - height of entire scrollable area
 -> IO ()
layoutSetSize self width height =
  (\(Layout arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_layout_set_size argPtr1 arg2 arg3)
{-# LINE 156 "./Graphics/UI/Gtk/Layout/Layout.chs" #-}
    (toLayout self)
    (fromIntegral width)
    (fromIntegral height)

-- | Gets the size that has been set on the layout, and that determines the
-- total extents of the layout's scrollbar area. See 'layoutSetSize'.
--
layoutGetSize :: LayoutClass self => self
 -> IO (Int, Int) -- ^ @(width, height)@
layoutGetSize self =
  alloca $ \widthPtr ->
  alloca $ \heightPtr -> do
  (\(Layout arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_layout_get_size argPtr1 arg2 arg3)
{-# LINE 169 "./Graphics/UI/Gtk/Layout/Layout.chs" #-}
    (toLayout self)
    widthPtr
    heightPtr
  width <-peek widthPtr
  height <- peek heightPtr
  return (fromIntegral width, fromIntegral height)

-- | This function should only be called after the layout has been placed in a
-- 'ScrolledWindow' or otherwise configured for scrolling. It returns the
-- 'Adjustment' used for communication between the horizontal scrollbar and
-- @layout@.
--
-- See 'ScrolledWindow', 'Scrollbar', 'Adjustment' for details.
--
layoutGetHAdjustment :: LayoutClass self => self
 -> IO Adjustment -- ^ returns horizontal scroll adjustment
layoutGetHAdjustment self =
  makeNewObject mkAdjustment $
  (\(Layout arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_layout_get_hadjustment argPtr1)
{-# LINE 188 "./Graphics/UI/Gtk/Layout/Layout.chs" #-}
    (toLayout self)

-- | This function should only be called after the layout has been placed in a
-- 'ScrolledWindow' or otherwise configured for scrolling. It returns the
-- 'Adjustment' used for communication between the vertical scrollbar and
-- @layout@.
--
-- See 'ScrolledWindow', 'Scrollbar', 'Adjustment' for details.
--
layoutGetVAdjustment :: LayoutClass self => self
 -> IO Adjustment -- ^ returns vertical scroll adjustment
layoutGetVAdjustment self =
  makeNewObject mkAdjustment $
  (\(Layout arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_layout_get_vadjustment argPtr1)
{-# LINE 202 "./Graphics/UI/Gtk/Layout/Layout.chs" #-}
    (toLayout self)

-- | Sets the horizontal scroll adjustment for the layout.
--
-- See 'ScrolledWindow', 'Scrollbar', 'Adjustment' for details.
--
layoutSetHAdjustment :: LayoutClass self => self
 -> Adjustment -- ^ @adjustment@ - new scroll adjustment
 -> IO ()
layoutSetHAdjustment self adjustment =
  (\(Layout arg1) (Adjustment arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_layout_set_hadjustment argPtr1 argPtr2)
{-# LINE 213 "./Graphics/UI/Gtk/Layout/Layout.chs" #-}
    (toLayout self)
    adjustment

-- | Sets the vertical scroll adjustment for the layout.
--
-- See 'ScrolledWindow', 'Scrollbar', 'Adjustment' for details.
--
layoutSetVAdjustment :: LayoutClass self => self
 -> Adjustment -- ^ @adjustment@ - new scroll adjustment
 -> IO ()
layoutSetVAdjustment self adjustment =
  (\(Layout arg1) (Adjustment arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_layout_set_vadjustment argPtr1 argPtr2)
{-# LINE 225 "./Graphics/UI/Gtk/Layout/Layout.chs" #-}
    (toLayout self)
    adjustment

-- | Retrieves the 'Drawable' part of the layout used for drawing operations.
--
layoutGetDrawWindow :: Layout -> IO DrawWindow
layoutGetDrawWindow lay = makeNewGObject mkDrawWindow $
  (\(Layout arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_layout_get_bin_window argPtr1)
{-# LINE 233 "./Graphics/UI/Gtk/Layout/Layout.chs" #-}
    (toLayout lay)

--------------------
-- Attributes

-- | The 'Adjustment' for the horizontal position.
--
layoutHAdjustment :: LayoutClass self => Attr self Adjustment
layoutHAdjustment = newAttr
  layoutGetHAdjustment
  layoutSetHAdjustment

-- | The 'Adjustment' for the vertical position.
--
layoutVAdjustment :: LayoutClass self => Attr self Adjustment
layoutVAdjustment = newAttr
  layoutGetVAdjustment
  layoutSetVAdjustment

-- | The width of the layout.
--
-- Allowed values: \<= @('maxBound' :: Int)@
--
-- Default value: 100
--
layoutWidth :: LayoutClass self => Attr self Int
layoutWidth = newAttrFromUIntProperty "width"

-- | The height of the layout.
--
-- Allowed values: \<= @('maxBound' :: Int)@
--
-- Default value: 100
--
layoutHeight :: LayoutClass self => Attr self Int
layoutHeight = newAttrFromUIntProperty "height"

--------------------
-- Child Attributes

-- | X position of child widget.
--
-- Default value: 0
--
layoutChildX :: (LayoutClass self, WidgetClass child) => child -> Attr self Int
layoutChildX = newAttrFromContainerChildIntProperty "x"

-- | Y position of child widget.
--
-- Default value: 0
--
layoutChildY :: (LayoutClass self, WidgetClass child) => child -> Attr self Int
layoutChildY = newAttrFromContainerChildIntProperty "y"

--------------------
-- Signals

-- | In case the adjustments are replaced, this signal is emitted.
--
onSetScrollAdjustments, afterSetScrollAdjustments :: LayoutClass self => self
 -> (Adjustment -> Adjustment -> IO ())
 -> IO (ConnectId self)
onSetScrollAdjustments = connect_OBJECT_OBJECT__NONE "set-scroll-adjustments" False
afterSetScrollAdjustments = connect_OBJECT_OBJECT__NONE "set-scroll-adjustments" True

foreign import ccall unsafe "gtk_layout_new"
  gtk_layout_new :: ((Ptr Adjustment) -> ((Ptr Adjustment) -> (IO (Ptr Widget))))

foreign import ccall safe "gtk_layout_put"
  gtk_layout_put :: ((Ptr Layout) -> ((Ptr Widget) -> (CInt -> (CInt -> (IO ())))))

foreign import ccall safe "gtk_layout_move"
  gtk_layout_move :: ((Ptr Layout) -> ((Ptr Widget) -> (CInt -> (CInt -> (IO ())))))

foreign import ccall safe "gtk_layout_set_size"
  gtk_layout_set_size :: ((Ptr Layout) -> (CUInt -> (CUInt -> (IO ()))))

foreign import ccall unsafe "gtk_layout_get_size"
  gtk_layout_get_size :: ((Ptr Layout) -> ((Ptr CUInt) -> ((Ptr CUInt) -> (IO ()))))

foreign import ccall unsafe "gtk_layout_get_hadjustment"
  gtk_layout_get_hadjustment :: ((Ptr Layout) -> (IO (Ptr Adjustment)))

foreign import ccall unsafe "gtk_layout_get_vadjustment"
  gtk_layout_get_vadjustment :: ((Ptr Layout) -> (IO (Ptr Adjustment)))

foreign import ccall safe "gtk_layout_set_hadjustment"
  gtk_layout_set_hadjustment :: ((Ptr Layout) -> ((Ptr Adjustment) -> (IO ())))

foreign import ccall safe "gtk_layout_set_vadjustment"
  gtk_layout_set_vadjustment :: ((Ptr Layout) -> ((Ptr Adjustment) -> (IO ())))

foreign import ccall safe "gtk_layout_get_bin_window"
  gtk_layout_get_bin_window :: ((Ptr Layout) -> (IO (Ptr DrawWindow)))
