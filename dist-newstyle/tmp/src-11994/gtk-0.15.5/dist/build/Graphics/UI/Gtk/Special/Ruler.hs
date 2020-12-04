
{-# LINE 2 "./Graphics/UI/Gtk/Special/Ruler.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget Ruler
--
-- Author : Andy Stewart
--
-- Created: 28 Mar 2010
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
-- Base class for horizontal or vertical rulers
--
module Graphics.UI.Gtk.Special.Ruler (

-- * Detail
--
-- | The 'Ruler' widget is a base class for horizontal and vertical rulers.
-- Rulers are used to show the mouse pointer's location in a window. The ruler
-- can either be horizontal or vertical on the window. Within the ruler a small
-- triangle indicates the location of the mouse relative to the horizontal or
-- vertical ruler. See 'HRuler' to learn how to create a new horizontal ruler.
-- See 'VRuler' to learn how to create a new vertical ruler.
--
-- * Rulers are removed in Gtk3 and thus this module is blank. There is no
-- replacement

-- * Class Hierarchy
--
-- |
-- @
-- | 'GObject'
-- | +----'Object'
-- | +----'Widget'
-- | +----Ruler
-- | +----'HRuler'
-- | +----'VRuler'
-- @

-- * Types
  Ruler,
  RulerClass,
  castToRuler,
  toRuler,

-- * Enums
  MetricType (..),

-- * Attributes
  rulerRange,
  rulerLower,
  rulerUpper,
  rulerPosition,
  rulerMaxSize,

  rulerMetric,


  ) where

import System.Glib.FFI
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.General.Enums
import Graphics.UI.Gtk.Types
{-# LINE 80 "./Graphics/UI/Gtk/Special/Ruler.chs" #-}


{-# LINE 82 "./Graphics/UI/Gtk/Special/Ruler.chs" #-}

--------------------
-- Methods

-- | This sets the range of the ruler.
--
rulerSetRange :: RulerClass self => self
 -> (Double
   ,Double
   ,Double
   ,Double)
      -- ^ @lower@ - the lower limit of the ruler
      -- ^ @upper@ - the upper limit of the ruler
      -- ^ @position@ - the mark on the ruler
      -- ^ @maxSize@ - the maximum size of the ruler used when calculating the space to leave for the text
 -> IO ()
rulerSetRange self (lower, upper, position, maxSize) =
  (\(Ruler arg1) arg2 arg3 arg4 arg5 -> withForeignPtr arg1 $ \argPtr1 ->gtk_ruler_set_range argPtr1 arg2 arg3 arg4 arg5)
{-# LINE 100 "./Graphics/UI/Gtk/Special/Ruler.chs" #-}
    (toRuler self)
    (realToFrac lower)
    (realToFrac upper)
    (realToFrac position)
    (realToFrac maxSize)

-- | Retrieves values indicating the range and current position of a 'Ruler'.
-- See 'rulerSetRange'.
--
rulerGetRange :: RulerClass self => self
 -> IO (Double, Double, Double, Double)
rulerGetRange self =
  alloca $ \lowerPtr ->
  alloca $ \upperPtr ->
  alloca $ \positionPtr ->
  alloca $ \maxSizePtr -> do
  (\(Ruler arg1) arg2 arg3 arg4 arg5 -> withForeignPtr arg1 $ \argPtr1 ->gtk_ruler_get_range argPtr1 arg2 arg3 arg4 arg5)
{-# LINE 117 "./Graphics/UI/Gtk/Special/Ruler.chs" #-}
    (toRuler self)
    lowerPtr
    upperPtr
    positionPtr
    maxSizePtr
  lower <- peek lowerPtr
  upper <- peek upperPtr
  position <- peek positionPtr
  maxSize <- peek maxSizePtr
  return (realToFrac lower, realToFrac upper, realToFrac position, realToFrac maxSize)

--------------------
-- Attributes

-- | Range of ruler
--
rulerRange :: RulerClass self => Attr self (Double, Double, Double, Double)
rulerRange = newAttr
  rulerGetRange
  rulerSetRange

-- | Lower limit of ruler.
--
-- Default value: 0
--
rulerLower :: RulerClass self => Attr self Double
rulerLower = newAttrFromDoubleProperty "lower"

-- | Upper limit of ruler.
--
-- Default value: 0
--
rulerUpper :: RulerClass self => Attr self Double
rulerUpper = newAttrFromDoubleProperty "upper"

-- | Position of mark on the ruler.
--
-- Default value: 0
--
rulerPosition :: RulerClass self => Attr self Double
rulerPosition = newAttrFromDoubleProperty "position"

-- | Maximum size of the ruler.
--
-- Default value: 0
--
rulerMaxSize :: RulerClass self => Attr self Double
rulerMaxSize = newAttrFromDoubleProperty "max-size"


-- | The metric used for the ruler.
--
-- Default value: ''Pixels''
--
-- Since 2.8
--
rulerMetric :: RulerClass self => Attr self MetricType
rulerMetric = newAttrFromEnumProperty "metric"
                gtk_metric_type_get_type
{-# LINE 176 "./Graphics/UI/Gtk/Special/Ruler.chs" #-}

foreign import ccall safe "gtk_ruler_set_range"
  gtk_ruler_set_range :: ((Ptr Ruler) -> (CDouble -> (CDouble -> (CDouble -> (CDouble -> (IO ()))))))

foreign import ccall safe "gtk_ruler_get_range"
  gtk_ruler_get_range :: ((Ptr Ruler) -> ((Ptr CDouble) -> ((Ptr CDouble) -> ((Ptr CDouble) -> ((Ptr CDouble) -> (IO ()))))))

foreign import ccall unsafe "gtk_metric_type_get_type"
  gtk_metric_type_get_type :: CULong
