
{-# LINE 2 "./Graphics/UI/Gtk/Misc/Viewport.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget Viewport
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
-- Issues:
--
-- The binding of this widget is superfluous as far as I can tell.
--
-- The only signal this widget registers is \"set-scroll-adjustments\". It is
-- not bound because it is meant to be received by the 'Viewport'
-- and sent by 'ScrolledWindow'.
--
-- |
-- Maintainer : gtk2hs-users@lists.sourceforge.net
-- Stability : provisional
-- Portability : portable (depends on GHC)
--
-- An adapter which makes widgets scrollable
--
module Graphics.UI.Gtk.Misc.Viewport (
-- * Detail
--
-- | A 'Viewport' is a helper widget that adds Adjustment slots to a
-- widget, i.e. the widget becomes scrollable. It can then be put into
-- 'ScrolledWindow' and will behave as expected.

-- * Class Hierarchy
-- |
-- @
-- | 'GObject'
-- | +----'Object'
-- | +----'Widget'
-- | +----'Container'
-- | +----'Bin'
-- | +----Viewport
-- @

-- * Types
  Viewport,
  ViewportClass,
  ShadowType(..),
  castToViewport, gTypeViewport,
  toViewport,

-- * Constructors
  viewportNew,

-- * Methods
  viewportGetHAdjustment,
  viewportGetVAdjustment,
  viewportSetHAdjustment,
  viewportSetVAdjustment,
  viewportSetShadowType,
  viewportGetShadowType,

  viewportGetBinWindow,


  viewportGetViewWindow,


-- * Attributes
  viewportHAdjustment,
  viewportVAdjustment,
  viewportShadowType,
  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.Attributes
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.UI.Gtk.Types
{-# LINE 90 "./Graphics/UI/Gtk/Misc/Viewport.chs" #-}
import Graphics.UI.Gtk.General.Enums (ShadowType(..))


{-# LINE 93 "./Graphics/UI/Gtk/Misc/Viewport.chs" #-}

--------------------
-- Constructors

-- | Creates a new 'Viewport' with the given adjustments.
--
viewportNew ::
    Adjustment -- ^ @hadjustment@ - horizontal adjustment.
 -> Adjustment -- ^ @vadjustment@ - vertical adjustment.
 -> IO Viewport
viewportNew hadjustment vadjustment =
  makeNewObject mkViewport $
  liftM (castPtr :: Ptr Widget -> Ptr Viewport) $
  (\(Adjustment arg1) (Adjustment arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_viewport_new argPtr1 argPtr2)
{-# LINE 107 "./Graphics/UI/Gtk/Misc/Viewport.chs" #-}
    hadjustment
    vadjustment

--------------------
-- Methods

-- | Returns the horizontal adjustment of the viewport.
--
viewportGetHAdjustment :: ViewportClass self => self -> IO Adjustment
viewportGetHAdjustment self =
  makeNewObject mkAdjustment $
  (\(Viewport arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_viewport_get_hadjustment argPtr1)
{-# LINE 119 "./Graphics/UI/Gtk/Misc/Viewport.chs" #-}
    (toViewport self)

-- | Returns the vertical adjustment of the viewport.
--
viewportGetVAdjustment :: ViewportClass self => self -> IO Adjustment
viewportGetVAdjustment self =
  makeNewObject mkAdjustment $
  (\(Viewport arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_viewport_get_vadjustment argPtr1)
{-# LINE 127 "./Graphics/UI/Gtk/Misc/Viewport.chs" #-}
    (toViewport self)

-- | Sets the horizontal adjustment of the viewport.
--
viewportSetHAdjustment :: ViewportClass self => self -> Adjustment -> IO ()
viewportSetHAdjustment self adjustment =
  (\(Viewport arg1) (Adjustment arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_viewport_set_hadjustment argPtr1 argPtr2)
{-# LINE 134 "./Graphics/UI/Gtk/Misc/Viewport.chs" #-}
    (toViewport self)
    adjustment

-- | Sets the vertical adjustment of the viewport.
--
viewportSetVAdjustment :: ViewportClass self => self -> Adjustment -> IO ()
viewportSetVAdjustment self adjustment =
  (\(Viewport arg1) (Adjustment arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_viewport_set_vadjustment argPtr1 argPtr2)
{-# LINE 142 "./Graphics/UI/Gtk/Misc/Viewport.chs" #-}
    (toViewport self)
    adjustment

-- | Sets the shadow type of the viewport.
--
viewportSetShadowType :: ViewportClass self => self
 -> ShadowType -- ^ @type@ - the new shadow type.
 -> IO ()
viewportSetShadowType self type_ =
  (\(Viewport arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_viewport_set_shadow_type argPtr1 arg2)
{-# LINE 152 "./Graphics/UI/Gtk/Misc/Viewport.chs" #-}
    (toViewport self)
    ((fromIntegral . fromEnum) type_)

-- | Gets the shadow type of the 'Viewport'. See 'viewportSetShadowType'.
--
viewportGetShadowType :: ViewportClass self => self
 -> IO ShadowType -- ^ returns the shadow type
viewportGetShadowType self =
  liftM (toEnum . fromIntegral) $
  (\(Viewport arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_viewport_get_shadow_type argPtr1)
{-# LINE 162 "./Graphics/UI/Gtk/Misc/Viewport.chs" #-}
    (toViewport self)


-- | Gets the bin window of the 'Viewport'.
--
-- * Available since Gtk version 2.20
--
viewportGetBinWindow :: ViewportClass self => self -> IO DrawWindow
viewportGetBinWindow self =
    makeNewGObject mkDrawWindow $
    (\(Viewport arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_viewport_get_bin_window argPtr1)
{-# LINE 173 "./Graphics/UI/Gtk/Misc/Viewport.chs" #-}
      (toViewport self)



-- | Gets the view window of the 'Viewport'.
--
-- * Available since Gtk+ version 2.22
--
viewportGetViewWindow :: ViewportClass self => self -> IO DrawWindow
viewportGetViewWindow self =
    makeNewGObject mkDrawWindow $
    (\(Viewport arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_viewport_get_view_window argPtr1)
{-# LINE 185 "./Graphics/UI/Gtk/Misc/Viewport.chs" #-}
      (toViewport self)


--------------------
-- Attributes

-- | The 'Adjustment' that determines the values of the horizontal position
-- for this viewport.
--
viewportHAdjustment :: ViewportClass self => Attr self Adjustment
viewportHAdjustment = newAttr
  viewportGetHAdjustment
  viewportSetHAdjustment

-- | The 'Adjustment' that determines the values of the vertical position for
-- this viewport.
--
viewportVAdjustment :: ViewportClass self => Attr self Adjustment
viewportVAdjustment = newAttr
  viewportGetVAdjustment
  viewportSetVAdjustment

-- | Determines how the shadowed box around the viewport is drawn.
--
-- Default value: 'ShadowIn'
--
viewportShadowType :: ViewportClass self => Attr self ShadowType
viewportShadowType = newAttr
  viewportGetShadowType
  viewportSetShadowType

--------------------
-- Signals

foreign import ccall unsafe "gtk_viewport_new"
  gtk_viewport_new :: ((Ptr Adjustment) -> ((Ptr Adjustment) -> (IO (Ptr Widget))))

foreign import ccall unsafe "gtk_viewport_get_hadjustment"
  gtk_viewport_get_hadjustment :: ((Ptr Viewport) -> (IO (Ptr Adjustment)))

foreign import ccall unsafe "gtk_viewport_get_vadjustment"
  gtk_viewport_get_vadjustment :: ((Ptr Viewport) -> (IO (Ptr Adjustment)))

foreign import ccall safe "gtk_viewport_set_hadjustment"
  gtk_viewport_set_hadjustment :: ((Ptr Viewport) -> ((Ptr Adjustment) -> (IO ())))

foreign import ccall safe "gtk_viewport_set_vadjustment"
  gtk_viewport_set_vadjustment :: ((Ptr Viewport) -> ((Ptr Adjustment) -> (IO ())))

foreign import ccall safe "gtk_viewport_set_shadow_type"
  gtk_viewport_set_shadow_type :: ((Ptr Viewport) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_viewport_get_shadow_type"
  gtk_viewport_get_shadow_type :: ((Ptr Viewport) -> (IO CInt))

foreign import ccall safe "gtk_viewport_get_bin_window"
  gtk_viewport_get_bin_window :: ((Ptr Viewport) -> (IO (Ptr DrawWindow)))

foreign import ccall safe "gtk_viewport_get_view_window"
  gtk_viewport_get_view_window :: ((Ptr Viewport) -> (IO (Ptr DrawWindow)))
