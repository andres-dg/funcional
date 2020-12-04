
{-# LINE 2 "./Graphics/UI/Gtk/Entry/HScale.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget HScale
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
-- A horizontal slider widget for selecting a value from a range
--
module Graphics.UI.Gtk.Entry.HScale (
-- * Detail
--
-- | The 'HScale' widget is used to allow the user to select a value using a
-- horizontal slider. To create one, use 'hScaleNewWithRange'.
--
-- The position to show the current value, and the number of decimal places
-- shown can be set using the parent 'Scale' class's functions.

-- * Class Hierarchy
-- |
-- @
-- | 'GObject'
-- | +----'Object'
-- | +----'Widget'
-- | +----'Range'
-- | +----'Scale'
-- | +----HScale
-- @

-- * Types
  HScale,
  HScaleClass,
  castToHScale, gTypeHScale,
  toHScale,

-- * Constructors
  hScaleNew,
  hScaleNewWithRange,
  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.UI.Gtk.Types
{-# LINE 64 "./Graphics/UI/Gtk/Entry/HScale.chs" #-}


{-# LINE 66 "./Graphics/UI/Gtk/Entry/HScale.chs" #-}

--------------------
-- Constructors

-- | Creates a new 'HScale'.
--
hScaleNew ::
    Adjustment -- ^ @adjustment@ - the 'Adjustment' which sets the range of
               -- the scale.
 -> IO HScale
hScaleNew adjustment =
  makeNewObject mkHScale $
  liftM (castPtr :: Ptr Widget -> Ptr HScale) $
  (\(Adjustment arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_hscale_new argPtr1)
{-# LINE 80 "./Graphics/UI/Gtk/Entry/HScale.chs" #-}
    adjustment

-- | Creates a new horizontal scale widget that lets the user input a number
-- between @min@ and @max@ (including @min@ and @max@) with the increment
-- @step@. @step@ must be nonzero; it's the distance the slider moves when
-- using the arrow keys to adjust the scale value.
--
-- Note that the way in which the precision is derived works best if @step@
-- is a power of ten. If the resulting precision is not suitable for your
-- needs, use 'Graphics.UI.Gtk.Abstract.Scale.scaleSetDigits' to correct it.
--
hScaleNewWithRange ::
    Double -- ^ @min@ - minimum value
 -> Double -- ^ @max@ - maximum value
 -> Double -- ^ @step@ - step increment (tick size) used with keyboard
              -- shortcuts
 -> IO HScale
hScaleNewWithRange min max step =
  makeNewObject mkHScale $
  liftM (castPtr :: Ptr Widget -> Ptr HScale) $
  gtk_hscale_new_with_range
{-# LINE 101 "./Graphics/UI/Gtk/Entry/HScale.chs" #-}
    (realToFrac min)
    (realToFrac max)
    (realToFrac step)

foreign import ccall unsafe "gtk_hscale_new"
  gtk_hscale_new :: ((Ptr Adjustment) -> (IO (Ptr Widget)))

foreign import ccall unsafe "gtk_hscale_new_with_range"
  gtk_hscale_new_with_range :: (CDouble -> (CDouble -> (CDouble -> (IO (Ptr Widget)))))
