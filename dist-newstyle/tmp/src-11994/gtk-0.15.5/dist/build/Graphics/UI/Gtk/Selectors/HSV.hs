
{-# LINE 2 "./Graphics/UI/Gtk/Selectors/HSV.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget HSV
--
-- Author : Andy Stewart
--
-- Created: 25 Mar 2010
--
-- Copyright (C) 2010 Andy Stewart
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
-- A \'color wheel\' widget
--
-- * Module available since Gtk+ version 2.14
--
module Graphics.UI.Gtk.Selectors.HSV (

-- * Detail
--
-- | 'HSV' is the \'color wheel\' part of a complete color selector widget. It
-- allows to select a color by determining its 'HSV' components in an intuitive
-- way. Moving the selection around the outer ring changes the hue, and moving
-- the selection point inside the inner triangle changes value and saturation.

-- * Class Hierarchy
--
-- |
-- @
-- | 'GObject'
-- | +----'Object'
-- | +----'Widget'
-- | +----HSV
-- @


-- * Types
  HSV,
  HSVClass,
  castToHSV,
  toHSV,

-- * Constructors
  hsvNew,

-- * Methods
  hsvIsAdjusting,
  hsvToRgb,
  rgbToHsv,

-- * Attributes
  hsvColor,
  hsvMetrics,

-- * Signals
  hsvChanged,
  hsvMove,

  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.Attributes
import Graphics.UI.Gtk.General.Enums (DirectionType (..))
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.UI.Gtk.Signals
{-# LINE 81 "./Graphics/UI/Gtk/Selectors/HSV.chs" #-}
import Graphics.UI.Gtk.Types
{-# LINE 82 "./Graphics/UI/Gtk/Selectors/HSV.chs" #-}


{-# LINE 84 "./Graphics/UI/Gtk/Selectors/HSV.chs" #-}


--------------------
-- Constructors

-- | Creates a new 'HSV' color selector.
--
-- * Available since 2.14
--
hsvNew :: IO HSV
hsvNew =
  makeNewObject mkHSV $
  liftM (castPtr :: Ptr Widget -> Ptr HSV) $
  gtk_hsv_new
{-# LINE 98 "./Graphics/UI/Gtk/Selectors/HSV.chs" #-}

--------------------
-- Methods

-- | Sets the current color in an 'HSV' color selector. Color component values
-- must be in the [0.0, 1.0] range.
--
-- * Available since 2.14
--
hsvSetColor :: HSVClass self => self
 -> (Double, Double, Double)
   -- ^ @(h, s, v)@
   -- @h@ - value for the hue
   -- @s@ value for the saturation
   -- @v@ value for the value
 -> IO ()
hsvSetColor self (h, s, v) =
  (\(HSV arg1) arg2 arg3 arg4 -> withForeignPtr arg1 $ \argPtr1 ->gtk_hsv_set_color argPtr1 arg2 arg3 arg4)
{-# LINE 116 "./Graphics/UI/Gtk/Selectors/HSV.chs" #-}
    (toHSV self)
    (realToFrac h)
    (realToFrac s)
    (realToFrac v)

-- | Queries the current color in an 'HSV' color selector. Returned values will
-- be in the [0.0, 1.0] range.
--
hsvGetColor :: HSVClass self => self
 -> IO (Double, Double, Double) -- ^ @(h, s, v)@ @h@ - Return value for the hue @s@ -
                                -- Return value for the saturation @v@ - Return
                                -- value for the value
hsvGetColor self =
  alloca $ \hPtr ->
  alloca $ \sPtr ->
  alloca $ \vPtr -> do
  (\(HSV arg1) arg2 arg3 arg4 -> withForeignPtr arg1 $ \argPtr1 ->gtk_hsv_get_color argPtr1 arg2 arg3 arg4)
{-# LINE 133 "./Graphics/UI/Gtk/Selectors/HSV.chs" #-}
    (toHSV self)
    hPtr
    sPtr
    vPtr
  h <- peek hPtr
  s <- peek sPtr
  v <- peek vPtr
  return (realToFrac h, realToFrac s, realToFrac v)

-- | Sets the size and ring width of an 'HSV' color selector.
--
hsvSetMetrics :: HSVClass self => self
 -> (Int, Int)
 -- ^ @(size, ringWidth)@
 -- ^ @size@ - Diameter for the hue ring
 -- ^ @ringWidth@ - Width of the hue ring
 -> IO ()
hsvSetMetrics self (size, ringWidth) =
  (\(HSV arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_hsv_set_metrics argPtr1 arg2 arg3)
{-# LINE 152 "./Graphics/UI/Gtk/Selectors/HSV.chs" #-}
    (toHSV self)
    (fromIntegral size)
    (fromIntegral ringWidth)

-- | Queries the size and ring width of an 'HSV' color selector.
--
hsvGetMetrics :: HSVClass self => self
 -> IO (Int, Int) -- ^ @(size, ringWidth)@
                  -- @size@ - Return value for the diameter of the hue ring
                  -- @ringWidth@ - Return value for the width of the hue ring
hsvGetMetrics self =
  alloca $ \sizePtr ->
  alloca $ \ringWidthPtr -> do
  (\(HSV arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_hsv_get_metrics argPtr1 arg2 arg3)
{-# LINE 166 "./Graphics/UI/Gtk/Selectors/HSV.chs" #-}
    (toHSV self)
    sizePtr
    ringWidthPtr
  size <- peek sizePtr
  ringWidth <- peek ringWidthPtr
  return (fromIntegral size, fromIntegral ringWidth)

-- | An 'HSV' color selector can be said to be adjusting if multiple rapid
-- changes are being made to its value, for example, when the user is adjusting
-- the value with the mouse. This function queries whether the 'HSV' color
-- selector is being adjusted or not.
--
hsvIsAdjusting :: HSVClass self => self
 -> IO Bool -- ^ returns @True@ if clients can ignore changes to the color
            -- value, since they may be transitory, or @False@ if they should
            -- consider the color value status to be final.
hsvIsAdjusting self =
  liftM toBool $
  (\(HSV arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_hsv_is_adjusting argPtr1)
{-# LINE 185 "./Graphics/UI/Gtk/Selectors/HSV.chs" #-}
    (toHSV self)

-- | Converts a color from 'HSV' space to RGB. Input values must be in the [0.0,
-- 1.0] range; output values will be in the same range.
--
hsvToRgb ::
 (Double, Double, Double)
   -- ^ @(h, s, v)@
   -- @h@ - value for the hue
   -- @s@ value for the saturation
   -- @v@ value for the value
 -> (Double, Double, Double) -- ^ @(r, g, b)@ @r@ - Return value for the red
                            -- component @g@ - Return value for the green
                            -- component @b@ - Return value for the blue
                            -- component
hsvToRgb (h, s, v) =
  unsafePerformIO $
  alloca $ \rPtr ->
  alloca $ \gPtr ->
  alloca $ \bPtr -> do
  gtk_hsv_to_rgb
{-# LINE 206 "./Graphics/UI/Gtk/Selectors/HSV.chs" #-}
    (realToFrac h)
    (realToFrac s)
    (realToFrac v)
    rPtr
    gPtr
    bPtr
  r <- peek rPtr
  g <- peek gPtr
  b <- peek bPtr
  return (realToFrac r, realToFrac g, realToFrac b)

-- | Converts a color from RGB space to 'HSV'. Input values must be in the [0.0, 1.0] range; output values
-- will be in the same range.
rgbToHsv ::
       (Double, Double, Double)
    -- ^ @(r, g, b)@ @r@ value for the red component
    -- @g@ value for the green component
    -- @b@ value for the blue component
    -> (Double, Double, Double)
   -- ^ @(h, s, v)@
   -- @h@ - Return value for the hue
   -- @s@ - Return value for the saturation
   -- @v@ - Return value for the value
rgbToHsv (r, g, b) =
  unsafePerformIO $
  alloca $ \hPtr ->
  alloca $ \sPtr ->
  alloca $ \vPtr -> do
  gtk_rgb_to_hsv
{-# LINE 235 "./Graphics/UI/Gtk/Selectors/HSV.chs" #-}
    (realToFrac r)
    (realToFrac g)
    (realToFrac b)
    hPtr
    sPtr
    vPtr
  h <- peek hPtr
  s <- peek sPtr
  v <- peek vPtr
  return (realToFrac h, realToFrac s, realToFrac v)

--------------------
-- Attributes
-- | Color in an 'HSV' color selector.
-- Color component values must be in the [0.0, 1.0] range.
hsvColor :: HSVClass self => Attr self (Double, Double, Double)
hsvColor = newAttr
    hsvGetColor
    hsvSetColor

-- | The size and ring width of an 'HSV' color selector.
hsvMetrics :: HSVClass self => Attr self (Int, Int)
hsvMetrics = newAttr
    hsvGetMetrics
    hsvSetMetrics

--------------------
-- Signals

-- |
--
hsvChanged :: HSVClass self => Signal self (IO ())
hsvChanged = Signal (connect_NONE__NONE "changed")

-- |
--
hsvMove :: HSVClass self => Signal self (DirectionType -> IO ())
hsvMove = Signal (connect_ENUM__NONE "move")

foreign import ccall safe "gtk_hsv_new"
  gtk_hsv_new :: (IO (Ptr Widget))

foreign import ccall safe "gtk_hsv_set_color"
  gtk_hsv_set_color :: ((Ptr HSV) -> (CDouble -> (CDouble -> (CDouble -> (IO ())))))

foreign import ccall safe "gtk_hsv_get_color"
  gtk_hsv_get_color :: ((Ptr HSV) -> ((Ptr CDouble) -> ((Ptr CDouble) -> ((Ptr CDouble) -> (IO ())))))

foreign import ccall safe "gtk_hsv_set_metrics"
  gtk_hsv_set_metrics :: ((Ptr HSV) -> (CInt -> (CInt -> (IO ()))))

foreign import ccall safe "gtk_hsv_get_metrics"
  gtk_hsv_get_metrics :: ((Ptr HSV) -> ((Ptr CInt) -> ((Ptr CInt) -> (IO ()))))

foreign import ccall safe "gtk_hsv_is_adjusting"
  gtk_hsv_is_adjusting :: ((Ptr HSV) -> (IO CInt))

foreign import ccall safe "gtk_hsv_to_rgb"
  gtk_hsv_to_rgb :: (CDouble -> (CDouble -> (CDouble -> ((Ptr CDouble) -> ((Ptr CDouble) -> ((Ptr CDouble) -> (IO ())))))))

foreign import ccall safe "gtk_rgb_to_hsv"
  gtk_rgb_to_hsv :: (CDouble -> (CDouble -> (CDouble -> ((Ptr CDouble) -> ((Ptr CDouble) -> ((Ptr CDouble) -> (IO ())))))))
