
{-# LINE 2 "./Graphics/UI/Gtk/Gdk/GC.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) GC
--
-- Author : Axel Simon
--
-- Created: 28 September 2002
--
-- Copyright (C) 2002-2005 Axel Simon
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
-- Graphics contexts - objects to encapsulate drawing properties
--
module Graphics.UI.Gtk.Gdk.GC (
-- * Detail
--
-- | All drawing operations in Gdk take a graphics context (GC) argument. A
-- graphics context encapsulates information about the way things are drawn,
-- such as the foreground color or line width. By using graphics contexts, the
-- number of arguments to each drawing call is greatly reduced, and
-- communication overhead is minimized, since identical arguments do not need
-- to be passed repeatedly.
--
-- Most values of a graphics context can be set at creation time by using
-- 'gcNewWithValues'. A few of the values in the GC, such as the dash
-- pattern, can only be set by the latter method.
--
-- Graphics Contexts are removed in Gtk3, so this module is empty.


  GC,
  GCClass,
  castToGC, gTypeGC,
  gcNew,
  GCValues(GCValues),
  newGCValues,
  Color(..),
  foreground,
  background,
  Function(..),
  function,
  Fill(..),
  fill,
  tile,
  stipple,
  clipMask,
  SubwindowMode(..),
  subwindowMode,
  tsXOrigin,
  tsYOrigin,
  clipXOrigin,
  clipYOrigin,
  graphicsExposure,
  lineWidth,
  LineStyle(..),
  lineStyle,
  CapStyle(..),
  capStyle,
  JoinStyle(..),
  joinStyle,
  gcNewWithValues,
  gcSetValues,
  gcGetValues,
  gcSetClipRectangle,
  gcSetClipRegion,
  gcSetDashes

  ) where


import Control.Monad (when)
import Data.Maybe (fromJust, isJust)
import Control.Exception (handle, ErrorCall(..))

import System.Glib.FFI
import Graphics.UI.Gtk.Types
{-# LINE 91 "./Graphics/UI/Gtk/Gdk/GC.chs" #-}
import Graphics.UI.Gtk.General.Structs
import Graphics.UI.Gtk.General.Enums (Function(..), Fill(..), SubwindowMode(..), LineStyle(..),
                                         CapStyle(..), JoinStyle(..))

import Graphics.UI.Gtk.Gdk.Region (Region(Region))



{-# LINE 99 "./Graphics/UI/Gtk/Gdk/GC.chs" #-}

-- | Create an empty graphics context.
--
gcNew :: DrawableClass d => d -> IO GC
gcNew d = do
  gcPtr <- (\(Drawable arg1) -> withForeignPtr arg1 $ \argPtr1 ->gdk_gc_new argPtr1) (toDrawable d)
  if (gcPtr==nullPtr) then return (error "gcNew: null graphics context.")
                      else wrapNewGObject mkGC (return gcPtr)


-- | Creates a graphics context with specific values.
--
gcNewWithValues :: DrawableClass d => d -> GCValues -> IO GC
gcNewWithValues d gcv = allocaBytes (sizeOf gcv) $ \vPtr -> do
  mask <- pokeGCValues vPtr gcv
  gc <- wrapNewGObject mkGC $ (\(Drawable arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gdk_gc_new_with_values argPtr1 arg2 arg3)
{-# LINE 115 "./Graphics/UI/Gtk/Gdk/GC.chs" #-}
    (toDrawable d) (castPtr vPtr) mask
  handle (\(ErrorCall _) -> return ()) $ when (isJust (tile gcv)) $
    touchForeignPtr ((unPixmap.fromJust.tile) gcv)
  handle (\(ErrorCall _) -> return ()) $ when (isJust (stipple gcv)) $
    touchForeignPtr ((unPixmap.fromJust.stipple) gcv)
  handle (\(ErrorCall _) -> return ()) $ when (isJust (clipMask gcv)) $
    touchForeignPtr ((unPixmap.fromJust.clipMask) gcv)
  return gc

-- | Change some of the values of a graphics context.
--
gcSetValues :: GC -> GCValues -> IO ()
gcSetValues gc gcv = allocaBytes (sizeOf gcv) $ \vPtr -> do
  mask <- pokeGCValues vPtr gcv
  gc <- (\(GC arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gdk_gc_set_values argPtr1 arg2 arg3) gc (castPtr vPtr) mask
  handle (\(ErrorCall _) -> return ()) $ when (isJust (tile gcv)) $
    touchForeignPtr ((unPixmap.fromJust.tile) gcv)
  handle (\(ErrorCall _) -> return ()) $ when (isJust (stipple gcv)) $
    touchForeignPtr ((unPixmap.fromJust.stipple) gcv)
  handle (\(ErrorCall _) -> return ()) $ when (isJust (clipMask gcv)) $
    touchForeignPtr ((unPixmap.fromJust.clipMask) gcv)
  return gc

-- | Retrieve the values in a graphics context.
--
gcGetValues :: GC -> IO GCValues
gcGetValues gc = alloca $ \vPtr -> do
  (\(GC arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gdk_gc_get_values argPtr1 arg2) gc (castPtr vPtr)
  peek vPtr

-- | Set a clipping rectangle.
--
-- * All drawing operations are restricted to this rectangle. This rectangle
-- is interpreted relative to the clip origin.
--
gcSetClipRectangle :: GC -> Rectangle -> IO ()
gcSetClipRectangle gc r = with r $ \rPtr ->
  (\(GC arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gdk_gc_set_clip_rectangle argPtr1 arg2) gc (castPtr rPtr)

-- | Set a clipping region.
--
-- * All drawing operations are restricted to this region. This region
-- is interpreted relative to the clip origin.
--
gcSetClipRegion :: GC -> Region -> IO ()
gcSetClipRegion = (\(GC arg1) (Region arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gdk_gc_set_clip_region argPtr1 argPtr2)
{-# LINE 161 "./Graphics/UI/Gtk/Gdk/GC.chs" #-}

-- | Specify the pattern with which lines are drawn.
--
-- * Every tuple in the list contains an even and an odd segment. Even
-- segments are drawn normally, whereby the 'lineStyle'
-- member of the graphics context defines if odd segements are drawn
-- or not. A @phase@ argument greater than 0 will drop
-- @phase@ pixels before starting to draw.
--
gcSetDashes :: GC -> Int -> [(Int,Int)] -> IO ()
gcSetDashes gc phase onOffList = do
  let onOff :: [(CSChar)]
      onOff = concatMap (\(on,off) -> [fromIntegral on, fromIntegral off])
              onOffList
  withArray onOff $ \aPtr ->
    (\(GC arg1) arg2 arg3 arg4 -> withForeignPtr arg1 $ \argPtr1 ->gdk_gc_set_dashes argPtr1 arg2 arg3 arg4) gc (fromIntegral phase) aPtr
    (fromIntegral (length onOff))

foreign import ccall unsafe "gdk_gc_new"
  gdk_gc_new :: ((Ptr Drawable) -> (IO (Ptr GC)))

foreign import ccall unsafe "gdk_gc_new_with_values"
  gdk_gc_new_with_values :: ((Ptr Drawable) -> ((Ptr ()) -> (CInt -> (IO (Ptr GC)))))

foreign import ccall unsafe "gdk_gc_set_values"
  gdk_gc_set_values :: ((Ptr GC) -> ((Ptr ()) -> (CInt -> (IO ()))))

foreign import ccall unsafe "gdk_gc_get_values"
  gdk_gc_get_values :: ((Ptr GC) -> ((Ptr ()) -> (IO ())))

foreign import ccall unsafe "gdk_gc_set_clip_rectangle"
  gdk_gc_set_clip_rectangle :: ((Ptr GC) -> ((Ptr ()) -> (IO ())))

foreign import ccall unsafe "gdk_gc_set_clip_region"
  gdk_gc_set_clip_region :: ((Ptr GC) -> ((Ptr Region) -> (IO ())))

foreign import ccall unsafe "gdk_gc_set_dashes"
  gdk_gc_set_dashes :: ((Ptr GC) -> (CInt -> ((Ptr CSChar) -> (CInt -> (IO ())))))
