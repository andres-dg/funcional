{-# LINE 1 "Graphics/UI/Gtk/General/Structs.hsc" #-}
{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- -*-haskell-*-





{-# LINE 12 "Graphics/UI/Gtk/General/Structs.hsc" #-}
--  GIMP Toolkit (GTK) Structures
--
--  Author : Axel Simon
--
--  Created: 2 May 2001
--
--  Copyright (C) 1999-2005 Axel Simon
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
-- #hide

-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
module Graphics.UI.Gtk.General.Structs (
  Point,
  Rectangle(..),
  Color(..),

{-# LINE 44 "Graphics/UI/Gtk/General/Structs.hsc" #-}

{-# LINE 45 "Graphics/UI/Gtk/General/Structs.hsc" #-}
  GCValues(..),
  pokeGCValues,
  newGCValues,
  widgetGetState,
  widgetGetSavedState,

{-# LINE 51 "Graphics/UI/Gtk/General/Structs.hsc" #-}
  Allocation,
  Requisition(..),
  treeIterSize,
  textIterSize,
  inputError,

{-# LINE 57 "Graphics/UI/Gtk/General/Structs.hsc" #-}
  dialogGetUpper,
  dialogGetActionArea,
  fileSelectionGetButtons,

{-# LINE 61 "Graphics/UI/Gtk/General/Structs.hsc" #-}
  ResponseId(..),
  fromResponse,
  toResponse,

{-# LINE 65 "Graphics/UI/Gtk/General/Structs.hsc" #-}
  NativeWindowId,
  toNativeWindowId,
  fromNativeWindowId,
  nativeWindowIdNone,

{-# LINE 70 "Graphics/UI/Gtk/General/Structs.hsc" #-}
  drawableGetID,

{-# LINE 72 "Graphics/UI/Gtk/General/Structs.hsc" #-}

{-# LINE 73 "Graphics/UI/Gtk/General/Structs.hsc" #-}
  toolbarChildButton,
  toolbarChildToggleButton,
  toolbarChildRadioButton,

{-# LINE 77 "Graphics/UI/Gtk/General/Structs.hsc" #-}

{-# LINE 78 "Graphics/UI/Gtk/General/Structs.hsc" #-}
  IconSize(..),

{-# LINE 80 "Graphics/UI/Gtk/General/Structs.hsc" #-}

{-# LINE 81 "Graphics/UI/Gtk/General/Structs.hsc" #-}
  comboGetList,

{-# LINE 83 "Graphics/UI/Gtk/General/Structs.hsc" #-}
  widgetGetDrawWindow,
  widgetGetSize,
  windowGetFrame,

{-# LINE 87 "Graphics/UI/Gtk/General/Structs.hsc" #-}
  styleGetForeground,
  styleGetBackground,
  styleGetLight,
  styleGetMiddle,
  styleGetDark,
  styleGetText,
  styleGetBase,
  styleGetAntiAliasing,

{-# LINE 96 "Graphics/UI/Gtk/General/Structs.hsc" #-}
  colorSelectionDialogGetColor,
  colorSelectionDialogGetOkButton,
  colorSelectionDialogGetCancelButton,
  colorSelectionDialogGetHelpButton,
  dragContextGetActions,
  dragContextSetActions,
  dragContextGetSuggestedAction,
  dragContextSetSuggestedAction,
  dragContextGetAction,
  dragContextSetAction,

{-# LINE 107 "Graphics/UI/Gtk/General/Structs.hsc" #-}
  SortColumnId,
  treeSortableDefaultSortColumnId,
  tagInvalid,
  selectionPrimary,
  selectionSecondary,
  selectionClipboard,
  targetString,
  selectionTypeAtom,
  selectionTypeInteger,
  selectionTypeString,

{-# LINE 118 "Graphics/UI/Gtk/General/Structs.hsc" #-}
  selectionDataGetType,

{-# LINE 120 "Graphics/UI/Gtk/General/Structs.hsc" #-}
  withTargetEntries,
  KeymapKey (..)
  ) where

import Control.Monad            (liftM)
import Data.IORef
import Control.Exception (handle, ErrorCall(..))

import System.Glib.FFI
import System.Glib.UTFString ( UTFCorrection, ofsToUTF )
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
import System.Glib.GObject              (makeNewGObject)
import Graphics.UI.Gtk.Types

{-# LINE 134 "Graphics/UI/Gtk/General/Structs.hsc" #-}
import Graphics.UI.Gtk.Gdk.Enums (Function, Fill, SubwindowMode,
                                  LineStyle, CapStyle, JoinStyle)

{-# LINE 137 "Graphics/UI/Gtk/General/Structs.hsc" #-}
import Graphics.UI.Gtk.General.Enums    (StateType)
import Graphics.UI.Gtk.General.DNDTypes (InfoId, Atom(Atom) , SelectionTag,
                                         TargetTag, SelectionTypeTag)
import Graphics.Rendering.Pango.Structs ( Color(..), Rectangle(..) )

{-# LINE 142 "Graphics/UI/Gtk/General/Structs.hsc" #-}

{-# LINE 145 "Graphics/UI/Gtk/General/Structs.hsc" #-}
-- | Represents the x and y coordinate of a point.
--
type Point = (Int, Int)

instance Storable Point where
  sizeOf _ = 8
{-# LINE 151 "Graphics/UI/Gtk/General/Structs.hsc" #-}
  alignment _ = alignment (undefined:: Int32)
{-# LINE 152 "Graphics/UI/Gtk/General/Structs.hsc" #-}
  peek ptr = do
    (x_      ::Int32)       <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 154 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    (y_      ::Int32)       <- (\hsc_ptr -> peekByteOff hsc_ptr 4) ptr
{-# LINE 155 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    return $ (fromIntegral x_, fromIntegral y_)
  poke ptr (x, y) = do
    (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr ((fromIntegral x)::Int32)
{-# LINE 158 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 4) ptr ((fromIntegral y)::Int32)
{-# LINE 159 "Graphics/UI/Gtk/General/Structs.hsc" #-}

instance Storable Rectangle where
  sizeOf _ = 16
{-# LINE 162 "Graphics/UI/Gtk/General/Structs.hsc" #-}
  alignment _ = alignment (undefined:: Int32)
{-# LINE 163 "Graphics/UI/Gtk/General/Structs.hsc" #-}
  peek ptr = do
    (x_      ::Int32)       <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 165 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    (y_      ::Int32)       <- (\hsc_ptr -> peekByteOff hsc_ptr 4) ptr
{-# LINE 166 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    (width_  ::Int32)       <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 167 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    (height_ ::Int32)       <- (\hsc_ptr -> peekByteOff hsc_ptr 12) ptr
{-# LINE 168 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    return $ Rectangle (fromIntegral x_) (fromIntegral y_)
                       (fromIntegral width_) (fromIntegral height_)
  poke ptr (Rectangle x y width height) = do
    (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr ((fromIntegral x)::Int32)
{-# LINE 172 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 4) ptr ((fromIntegral y)::Int32)
{-# LINE 173 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr ((fromIntegral width)::Int32)
{-# LINE 174 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 12) ptr ((fromIntegral height)::Int32)
{-# LINE 175 "Graphics/UI/Gtk/General/Structs.hsc" #-}

instance Storable Color where
  sizeOf _ = 12
{-# LINE 178 "Graphics/UI/Gtk/General/Structs.hsc" #-}
  alignment _ = alignment (undefined::Word32)
{-# LINE 179 "Graphics/UI/Gtk/General/Structs.hsc" #-}
  peek ptr = do
    red    <- (\hsc_ptr -> peekByteOff hsc_ptr 4) ptr
{-# LINE 181 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    green  <- (\hsc_ptr -> peekByteOff hsc_ptr 6) ptr
{-# LINE 182 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    blue   <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 183 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    return $ Color red green blue
  poke ptr (Color red green blue) = do
    (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr (0::Int32)
{-# LINE 186 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 4)   ptr red
{-# LINE 187 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 6) ptr green
{-# LINE 188 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 8)  ptr blue
{-# LINE 189 "Graphics/UI/Gtk/General/Structs.hsc" #-}

{-# LINE 190 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    cPtr <- gdkColormapGetSystem
    gdkColormapAllocColor cPtr ptr 0 1

{-# LINE 193 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    return ()


{-# LINE 214 "Graphics/UI/Gtk/General/Structs.hsc" #-}


{-# LINE 216 "Graphics/UI/Gtk/General/Structs.hsc" #-}
type ColorMap = ()

foreign import ccall unsafe "gdk_colormap_get_system"
  gdkColormapGetSystem :: IO (Ptr ColorMap)

foreign import ccall unsafe "gdk_colormap_alloc_color"
  gdkColormapAllocColor :: Ptr ColorMap -> Ptr Color -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "gdk_colormap_query_color"
  gdkColormapQueryColor :: Ptr ColorMap -> CULong -> Ptr Color -> IO ()

-- entry GC
-- | Intermediate data structure for 'GC's.
--
-- * If @graphicsExposure@ is set then copying portions into a
--   drawable will generate an @\"exposure\"@ event, even if the
--   destination area is not currently visible.
--
-- Removed in Gtk3.
data GCValues = GCValues {
  foreground :: Color,
  background :: Color,
  function   :: Function,
  fill       :: Fill,
  tile       :: Maybe Pixmap,
  stipple    :: Maybe Pixmap,
  clipMask   :: Maybe Pixmap,
  subwindowMode :: SubwindowMode,
  tsXOrigin  :: Int,
  tsYOrigin  :: Int,
  clipXOrigin:: Int,
  clipYOrigin:: Int,
  graphicsExposure :: Bool,
  lineWidth  :: Int,
  lineStyle  :: LineStyle,
  capStyle   :: CapStyle,
  joinStyle  :: JoinStyle
  }

instance Storable GCValues where
  sizeOf _ = 104
{-# LINE 257 "Graphics/UI/Gtk/General/Structs.hsc" #-}
  alignment _ = alignment (undefined::Color)
  peek ptr = do
    -- gdk_gc_get_values does not fill in the r,g,b members of the foreground
    -- and background colours (it only fills in the allocated pixel value),
    -- so we have to fill them in here:
    let foregroundPtr, backgroundPtr :: Ptr Color
        foregroundPtr = (\hsc_ptr -> hsc_ptr `plusPtr` 0) ptr
{-# LINE 264 "Graphics/UI/Gtk/General/Structs.hsc" #-}
        backgroundPtr = (\hsc_ptr -> hsc_ptr `plusPtr` 12) ptr
{-# LINE 265 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    (foregroundPixelPtr :: CULong) <- (\hsc_ptr -> peekByteOff hsc_ptr 0) foregroundPtr
{-# LINE 266 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    (backgroundPixelPtr :: CULong) <- (\hsc_ptr -> peekByteOff hsc_ptr 0) backgroundPtr
{-# LINE 267 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    colormapPtr <- gdkColormapGetSystem
    gdkColormapQueryColor colormapPtr foregroundPixelPtr foregroundPtr
    gdkColormapQueryColor colormapPtr backgroundPixelPtr backgroundPtr

    foreground_ <- peek ((\hsc_ptr -> hsc_ptr `plusPtr` 0) ptr)
{-# LINE 272 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    background_ <- peek ((\hsc_ptr -> hsc_ptr `plusPtr` 12) ptr)
{-# LINE 273 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    (function_  :: Word32) <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 274 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    (fill_      :: Word32) <- (\hsc_ptr -> peekByteOff hsc_ptr 36) ptr
{-# LINE 275 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    tile_       <- do
                     pPtr <- (\hsc_ptr -> peekByteOff hsc_ptr 40) ptr
{-# LINE 277 "Graphics/UI/Gtk/General/Structs.hsc" #-}
                     if (pPtr==nullPtr) then return Nothing else
                       liftM Just $ makeNewGObject mkPixmap $ return pPtr
    stipple_    <- do
                     pPtr <- (\hsc_ptr -> peekByteOff hsc_ptr 48) ptr
{-# LINE 281 "Graphics/UI/Gtk/General/Structs.hsc" #-}
                     if (pPtr==nullPtr) then return Nothing else
                       liftM Just $ makeNewGObject mkPixmap $ return pPtr
    clipMask_   <- do
                     pPtr <- (\hsc_ptr -> peekByteOff hsc_ptr 56) ptr
{-# LINE 285 "Graphics/UI/Gtk/General/Structs.hsc" #-}
                     if (pPtr==nullPtr) then return Nothing else
                       liftM Just $ makeNewGObject mkPixmap $ return pPtr
    (subwindow_ :: Word32)
{-# LINE 288 "Graphics/UI/Gtk/General/Structs.hsc" #-}
                <- (\hsc_ptr -> peekByteOff hsc_ptr 64) ptr
{-# LINE 289 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    (tsXOrigin_ :: Int32)
{-# LINE 290 "Graphics/UI/Gtk/General/Structs.hsc" #-}
                <- (\hsc_ptr -> peekByteOff hsc_ptr 68) ptr
{-# LINE 291 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    (tsYOrigin_ :: Int32)
{-# LINE 292 "Graphics/UI/Gtk/General/Structs.hsc" #-}
                <- (\hsc_ptr -> peekByteOff hsc_ptr 72) ptr
{-# LINE 293 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    (clipXOrigin_:: Int32)
{-# LINE 294 "Graphics/UI/Gtk/General/Structs.hsc" #-}
                <- (\hsc_ptr -> peekByteOff hsc_ptr 76) ptr
{-# LINE 295 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    (clipYOrigin_:: Int32)
{-# LINE 296 "Graphics/UI/Gtk/General/Structs.hsc" #-}
                <- (\hsc_ptr -> peekByteOff hsc_ptr 80) ptr
{-# LINE 297 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    (graphics_  :: Int32)
{-# LINE 298 "Graphics/UI/Gtk/General/Structs.hsc" #-}
                <- (\hsc_ptr -> peekByteOff hsc_ptr 84) ptr
{-# LINE 299 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    (lineWidth_ :: Int32)
{-# LINE 300 "Graphics/UI/Gtk/General/Structs.hsc" #-}
                <- (\hsc_ptr -> peekByteOff hsc_ptr 88) ptr
{-# LINE 301 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    (lineStyle_ :: Word32)
{-# LINE 302 "Graphics/UI/Gtk/General/Structs.hsc" #-}
                <- (\hsc_ptr -> peekByteOff hsc_ptr 92) ptr
{-# LINE 303 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    (capStyle_  :: Word32)
{-# LINE 304 "Graphics/UI/Gtk/General/Structs.hsc" #-}
                <- (\hsc_ptr -> peekByteOff hsc_ptr 96) ptr
{-# LINE 305 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    (joinStyle_ :: Word32)
{-# LINE 306 "Graphics/UI/Gtk/General/Structs.hsc" #-}
                <- (\hsc_ptr -> peekByteOff hsc_ptr 100) ptr
{-# LINE 307 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    return $ GCValues {
      foreground = foreground_,
      background = background_,
      function   = (toEnum.fromIntegral) function_,
      fill       = (toEnum.fromIntegral) fill_,
      tile       = tile_,
      stipple    = stipple_,
      clipMask   = clipMask_,
      subwindowMode = (toEnum.fromIntegral) subwindow_,
      tsXOrigin  = fromIntegral tsXOrigin_,
      tsYOrigin  = fromIntegral tsYOrigin_,
      clipXOrigin= fromIntegral clipXOrigin_,
      clipYOrigin= fromIntegral clipYOrigin_,
      graphicsExposure = toBool graphics_,
      lineWidth  = fromIntegral lineWidth_,
      lineStyle  = (toEnum.fromIntegral) lineStyle_,
      capStyle   = (toEnum.fromIntegral) capStyle_,
      joinStyle  = (toEnum.fromIntegral) joinStyle_
    }
  poke = error "GCValues poke undefined (not sure why)"

pokeGCValues :: Ptr GCValues -> GCValues -> IO CInt
pokeGCValues ptr (GCValues {
    foreground = foreground_,
    background = background_,
    function   = function_,
    fill       = fill_,
    tile       = tile_,
    stipple    = stipple_,
    clipMask   = clipMask_,
    subwindowMode = subwindow_,
    tsXOrigin  = tsXOrigin_,
    tsYOrigin  = tsYOrigin_,
    clipXOrigin= clipXOrigin_,
    clipYOrigin= clipYOrigin_,
    graphicsExposure = graphics_,
    lineWidth  = lineWidth_,
    lineStyle  = lineStyle_,
    capStyle   = capStyle_,
    joinStyle  = joinStyle_
  }) = do
    r <- newIORef 0
    add r 1 $
{-# LINE 350 "Graphics/UI/Gtk/General/Structs.hsc" #-}
      poke ((\hsc_ptr -> hsc_ptr `plusPtr` 0) ptr) foreground_
{-# LINE 351 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    add r 2 $
{-# LINE 352 "Graphics/UI/Gtk/General/Structs.hsc" #-}
      poke ((\hsc_ptr -> hsc_ptr `plusPtr` 12) ptr) background_
{-# LINE 353 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    add r 8 $
{-# LINE 354 "Graphics/UI/Gtk/General/Structs.hsc" #-}
      (\hsc_ptr -> pokeByteOff hsc_ptr 32) ptr
{-# LINE 355 "Graphics/UI/Gtk/General/Structs.hsc" #-}
      (fromIntegral (fromEnum function_):: Word32)
{-# LINE 356 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    add r 16 $
{-# LINE 357 "Graphics/UI/Gtk/General/Structs.hsc" #-}
      (\hsc_ptr -> pokeByteOff hsc_ptr 36) ptr
{-# LINE 358 "Graphics/UI/Gtk/General/Structs.hsc" #-}
      (fromIntegral (fromEnum fill_):: Word32)
{-# LINE 359 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    case tile_ of
      Nothing -> return ()
      Just tile_ -> add r 32 $
{-# LINE 362 "Graphics/UI/Gtk/General/Structs.hsc" #-}
                    withForeignPtr (unPixmap tile_) $
                    (\hsc_ptr -> pokeByteOff hsc_ptr 40) ptr
{-# LINE 364 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    case stipple_ of
      Nothing -> return ()
      Just stipple_ -> add r 64 $
{-# LINE 367 "Graphics/UI/Gtk/General/Structs.hsc" #-}
                       withForeignPtr (unPixmap stipple_) $
                       (\hsc_ptr -> pokeByteOff hsc_ptr 48) ptr
{-# LINE 369 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    case clipMask_ of
      Nothing -> return ()
      Just clipMask_ -> add r 128 $
{-# LINE 372 "Graphics/UI/Gtk/General/Structs.hsc" #-}
                        withForeignPtr (unPixmap clipMask_) $
                        (\hsc_ptr -> pokeByteOff hsc_ptr 56) ptr
{-# LINE 374 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    add r 256 $
{-# LINE 375 "Graphics/UI/Gtk/General/Structs.hsc" #-}
      (\hsc_ptr -> pokeByteOff hsc_ptr 64) ptr
{-# LINE 376 "Graphics/UI/Gtk/General/Structs.hsc" #-}
      (fromIntegral (fromEnum subwindow_):: Word32)
{-# LINE 377 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    add r 512 $
{-# LINE 378 "Graphics/UI/Gtk/General/Structs.hsc" #-}
      (\hsc_ptr -> pokeByteOff hsc_ptr 68) ptr
{-# LINE 379 "Graphics/UI/Gtk/General/Structs.hsc" #-}
      (fromIntegral tsXOrigin_:: Int32)
{-# LINE 380 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    add r 1024 $
{-# LINE 381 "Graphics/UI/Gtk/General/Structs.hsc" #-}
      (\hsc_ptr -> pokeByteOff hsc_ptr 72) ptr
{-# LINE 382 "Graphics/UI/Gtk/General/Structs.hsc" #-}
      (fromIntegral tsYOrigin_:: Int32)
{-# LINE 383 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    add r 2048 $
{-# LINE 384 "Graphics/UI/Gtk/General/Structs.hsc" #-}
      (\hsc_ptr -> pokeByteOff hsc_ptr 76) ptr
{-# LINE 385 "Graphics/UI/Gtk/General/Structs.hsc" #-}
      (fromIntegral clipXOrigin_:: Int32)
{-# LINE 386 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    add r 4096 $
{-# LINE 387 "Graphics/UI/Gtk/General/Structs.hsc" #-}
      (\hsc_ptr -> pokeByteOff hsc_ptr 80) ptr
{-# LINE 388 "Graphics/UI/Gtk/General/Structs.hsc" #-}
      (fromIntegral clipYOrigin_:: Int32)
{-# LINE 389 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    add r 8192 $
{-# LINE 390 "Graphics/UI/Gtk/General/Structs.hsc" #-}
      (\hsc_ptr -> pokeByteOff hsc_ptr 84) ptr
{-# LINE 391 "Graphics/UI/Gtk/General/Structs.hsc" #-}
      (fromBool graphics_:: Int32)
{-# LINE 392 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    add r 16384 $
{-# LINE 393 "Graphics/UI/Gtk/General/Structs.hsc" #-}
      (\hsc_ptr -> pokeByteOff hsc_ptr 88) ptr
{-# LINE 394 "Graphics/UI/Gtk/General/Structs.hsc" #-}
      (fromIntegral lineWidth_:: Int32)
{-# LINE 395 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    add r 32768 $
{-# LINE 396 "Graphics/UI/Gtk/General/Structs.hsc" #-}
      (\hsc_ptr -> pokeByteOff hsc_ptr 92) ptr
{-# LINE 397 "Graphics/UI/Gtk/General/Structs.hsc" #-}
      (fromIntegral (fromEnum lineStyle_):: Word32)
{-# LINE 398 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    add r 65536 $
{-# LINE 399 "Graphics/UI/Gtk/General/Structs.hsc" #-}
      (\hsc_ptr -> pokeByteOff hsc_ptr 96) ptr
{-# LINE 400 "Graphics/UI/Gtk/General/Structs.hsc" #-}
      (fromIntegral (fromEnum capStyle_):: Word32)
{-# LINE 401 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    add r 131072 $
{-# LINE 402 "Graphics/UI/Gtk/General/Structs.hsc" #-}
      (\hsc_ptr -> pokeByteOff hsc_ptr 100) ptr
{-# LINE 403 "Graphics/UI/Gtk/General/Structs.hsc" #-}
      (fromIntegral (fromEnum joinStyle_):: Word32)
{-# LINE 404 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    readIORef r
  where
    add :: IORef CInt -> CInt -> IO () -> IO ()
    add r mVal act = handle (\(ErrorCall _) -> return ()) $ do
      act
      modifyIORef r (\val -> val+mVal)

-- constant newGCValues An empty record of 'GCValues'.
--
-- * Use this value instead of the constructor to avoid compiler wanings
--   about uninitialized fields.
--
-- Removed in Gtk3.
newGCValues :: GCValues
newGCValues = GCValues {
    foreground = undefined,
    background = undefined,
    function   = undefined,
    fill       = undefined,
    tile       = Nothing,
    stipple    = Nothing,
    clipMask   = Nothing,
    subwindowMode = undefined,
    tsXOrigin  = undefined,
    tsYOrigin  = undefined,
    clipXOrigin= undefined,
    clipYOrigin= undefined,
    graphicsExposure = undefined,
    lineWidth  = undefined,
    lineStyle  = undefined,
    capStyle   = undefined,
    joinStyle  = undefined
  }

{-# LINE 438 "Graphics/UI/Gtk/General/Structs.hsc" #-}

-- Widget related methods

{-# LINE 441 "Graphics/UI/Gtk/General/Structs.hsc" #-}
-- | Retrieve the current state of the widget.
--
-- * The state refers to different modes of user interaction, see
--   'StateType' for more information.
--
-- Removed in Gtk3.
widgetGetState :: WidgetClass w => w -> IO StateType
widgetGetState w =
  liftM (\x -> toEnum (fromIntegral (x :: Word8))) $
{-# LINE 450 "Graphics/UI/Gtk/General/Structs.hsc" #-}
  withForeignPtr ((unWidget . toWidget) w) $ (\hsc_ptr -> peekByteOff hsc_ptr 34)
{-# LINE 451 "Graphics/UI/Gtk/General/Structs.hsc" #-}

-- | Retrieve the current state of the widget.
--
-- * If a widget is turned insensitive, the previous state is stored in
--   a specific location. This function retrieves this previous state.
--
-- Removed in Gtk3.
widgetGetSavedState :: WidgetClass w => w -> IO StateType
widgetGetSavedState w =
  liftM (\x -> toEnum (fromIntegral (x :: Word8))) $
{-# LINE 461 "Graphics/UI/Gtk/General/Structs.hsc" #-}
  withForeignPtr ((unWidget . toWidget) w) $ (\hsc_ptr -> peekByteOff hsc_ptr 35)
{-# LINE 462 "Graphics/UI/Gtk/General/Structs.hsc" #-}

{-# LINE 463 "Graphics/UI/Gtk/General/Structs.hsc" #-}

-- | Allocation
--
-- * For Widget's 'Graphics.UI.Gtk.Abstract.Widget.sizeAllocate' signal.
--   The @x@ and @y@ values of the rectangle refer to the widgets position
--   relative to its parent window.
--
type Allocation = Rectangle


-- | Requisition
--
-- * For 'Graphics.UI.Gtk.Abstract.Widget.widgetSizeRequest'. The values
--   represent the desired width and height of the widget.
--
data Requisition = Requisition Int Int deriving (Eq,Show)

instance Storable Requisition where
  sizeOf _ = 8
{-# LINE 482 "Graphics/UI/Gtk/General/Structs.hsc" #-}
  alignment _ = alignment (undefined::Int32)
{-# LINE 483 "Graphics/UI/Gtk/General/Structs.hsc" #-}
  peek ptr = do
    (width_  ::Int32)       <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 485 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    (height_ ::Int32)       <- (\hsc_ptr -> peekByteOff hsc_ptr 4) ptr
{-# LINE 486 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    return $ Requisition (fromIntegral width_) (fromIntegral height_)
  poke ptr (Requisition width height) = do
    (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr ((fromIntegral width)::Int32)
{-# LINE 489 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 4) ptr ((fromIntegral height)::Int32)
{-# LINE 490 "Graphics/UI/Gtk/General/Structs.hsc" #-}


-- SpinButton related mothods

-- If an invalid input has been put into a SpinButton the input function may
-- reject this value by returning this value.
inputError :: Int32
{-# LINE 497 "Graphics/UI/Gtk/General/Structs.hsc" #-}
inputError = -1
{-# LINE 498 "Graphics/UI/Gtk/General/Structs.hsc" #-}


-- The TreeIter struct is not used by itself. But we have to allocate space
-- for it in module TreeModel.
treeIterSize :: Int
treeIterSize = 32
{-# LINE 504 "Graphics/UI/Gtk/General/Structs.hsc" #-}


-- The TextIter struct can be a local variable in a C program. We have to
-- store it on the heap.
--
textIterSize :: Int
textIterSize = 80
{-# LINE 511 "Graphics/UI/Gtk/General/Structs.hsc" #-}

-- Dialog related methods

{-# LINE 514 "Graphics/UI/Gtk/General/Structs.hsc" #-}
-- | Get the upper part of a dialog.
--
-- * The upper part of a dialog window consists of a 'VBox'.
--   Add the required widgets into this box.
--
dialogGetUpper :: DialogClass dc => dc -> IO VBox
dialogGetUpper dc = makeNewObject mkVBox $ liftM castPtr $
  withForeignPtr ((unDialog.toDialog) dc) (\hsc_ptr -> peekByteOff hsc_ptr 240)
{-# LINE 522 "Graphics/UI/Gtk/General/Structs.hsc" #-}

-- | Extract the action area of a dialog box.
--
-- * This
-- is useful to add some special widgets that cannot be added with
-- dialogAddActionWidget.
--
dialogGetActionArea :: DialogClass dc => dc -> IO HBox
dialogGetActionArea dc = makeNewObject mkHBox $ liftM castPtr $
  withForeignPtr ((unDialog.toDialog) dc) (\hsc_ptr -> peekByteOff hsc_ptr 248)
{-# LINE 532 "Graphics/UI/Gtk/General/Structs.hsc" #-}

{-# LINE 533 "Graphics/UI/Gtk/General/Structs.hsc" #-}

-- | Some constructors that can be used as response
-- numbers for dialogs.
--
data ResponseId

  -- | GTK returns this if a response widget has no @response_id@,
  --   or if the dialog gets programmatically hidden or destroyed.
  = ResponseNone

  -- | GTK won't return these unless you pass them in as
  --   the response for an action widget. They are for your convenience.
  | ResponseReject
  | ResponseAccept -- ^ (as above)

  -- | If the dialog is deleted.
  | ResponseDeleteEvent

  -- | \"Ok\" was pressed.
  --
  -- * This value is returned from the \"Ok\" stock dialog button.
  | ResponseOk

  -- | \"Cancel\" was pressed.
  --
  -- * These value is returned from the \"Cancel\" stock dialog button.
  | ResponseCancel

  -- | \"Close\" was pressed.
  --
  -- * This value is returned from the \"Close\" stock dialog button.
        | ResponseClose

  -- | \"Yes\" was pressed.
  --
  -- * This value is returned from the \"Yes\" stock dialog button.
  | ResponseYes

  -- | \"No\" was pressed.
  --
  -- * This value is returned from the \"No\" stock dialog button.
  | ResponseNo

  -- | \"Apply\" was pressed.
  --
  -- * This value is returned from the \"Apply\" stock dialog button.
        | ResponseApply

  -- |  \"Help\" was pressed.
  --
  -- * This value is returned from the \"Help\" stock dialog button.
  | ResponseHelp

  -- | A user-defined response
  --
  -- * This value is returned from a user defined button
  | ResponseUser Int
  deriving (Show, Eq)

fromResponse :: Integral a => ResponseId -> a
fromResponse ResponseNone = -1
fromResponse ResponseReject = -2
fromResponse ResponseAccept = -3
fromResponse ResponseDeleteEvent = -4
fromResponse ResponseOk = -5
fromResponse ResponseCancel = -6
fromResponse ResponseClose = -7
fromResponse ResponseYes = -8
fromResponse ResponseNo = -9
fromResponse ResponseApply = -10
fromResponse ResponseHelp = -11
fromResponse (ResponseUser i) = fromIntegral i

toResponse :: Integral a => a -> ResponseId
toResponse (-1) = ResponseNone
toResponse (-2) = ResponseReject
toResponse (-3) = ResponseAccept
toResponse (-4) = ResponseDeleteEvent
toResponse (-5) = ResponseOk
toResponse (-6) = ResponseCancel
toResponse (-7) = ResponseClose
toResponse (-8) = ResponseYes
toResponse (-9) = ResponseNo
toResponse (-10) = ResponseApply
toResponse (-11) = ResponseHelp
toResponse i = ResponseUser $ fromIntegral i


{-# LINE 621 "Graphics/UI/Gtk/General/Structs.hsc" #-}
-- | The identifer of a window of the underlying windowing system.
--

{-# LINE 635 "Graphics/UI/Gtk/General/Structs.hsc" #-}
newtype NativeWindowId = NativeWindowId (Maybe DrawWindow) deriving (Eq)
unNativeWindowId :: NativeWindowId -> Maybe DrawWindow
unNativeWindowId (NativeWindowId id) = id
toNativeWindowId :: Maybe DrawWindow -> NativeWindowId
toNativeWindowId = NativeWindowId
fromNativeWindowId :: NativeWindowId -> Maybe DrawWindow
fromNativeWindowId = unNativeWindowId
nativeWindowIdNone :: NativeWindowId
nativeWindowIdNone = NativeWindowId Nothing

{-# LINE 659 "Graphics/UI/Gtk/General/Structs.hsc" #-}

{-# LINE 660 "Graphics/UI/Gtk/General/Structs.hsc" #-}


{-# LINE 662 "Graphics/UI/Gtk/General/Structs.hsc" #-}

{-# LINE 669 "Graphics/UI/Gtk/General/Structs.hsc" #-}

{-# LINE 675 "Graphics/UI/Gtk/General/Structs.hsc" #-}

-- | Get 'NativeWindowId' of 'Drawable'.

{-# LINE 680 "Graphics/UI/Gtk/General/Structs.hsc" #-}
drawableGetID :: DrawWindowClass d => d -> IO NativeWindowId

{-# LINE 682 "Graphics/UI/Gtk/General/Structs.hsc" #-}
drawableGetID d =
  liftM toNativeWindowId $

{-# LINE 687 "Graphics/UI/Gtk/General/Structs.hsc" #-}
  (\(DrawWindow drawable) ->

{-# LINE 689 "Graphics/UI/Gtk/General/Structs.hsc" #-}

{-# LINE 700 "Graphics/UI/Gtk/General/Structs.hsc" #-}
     return $ Just (DrawWindow drawable)

{-# LINE 702 "Graphics/UI/Gtk/General/Structs.hsc" #-}

{-# LINE 705 "Graphics/UI/Gtk/General/Structs.hsc" #-}
  ) (toDrawWindow d)

{-# LINE 707 "Graphics/UI/Gtk/General/Structs.hsc" #-}



{-# LINE 710 "Graphics/UI/Gtk/General/Structs.hsc" #-}

{-# LINE 711 "Graphics/UI/Gtk/General/Structs.hsc" #-}
-- Static values for different Toolbar widgets.
--
-- * c2hs and hsc should agree on types!
--
-- Removed in Gtk3.
toolbarChildButton, toolbarChildToggleButton, toolbarChildRadioButton ::
  CInt -- \#gtk2hs_type GtkToolbarChildType
toolbarChildButton       = 1
{-# LINE 719 "Graphics/UI/Gtk/General/Structs.hsc" #-}
toolbarChildToggleButton = 2
{-# LINE 720 "Graphics/UI/Gtk/General/Structs.hsc" #-}
toolbarChildRadioButton  = 3
{-# LINE 721 "Graphics/UI/Gtk/General/Structs.hsc" #-}

{-# LINE 722 "Graphics/UI/Gtk/General/Structs.hsc" #-}

{-# LINE 723 "Graphics/UI/Gtk/General/Structs.hsc" #-}

-- | The size of an icon in pixels.
--
-- * This enumeration contains one case that is not exported and which
--   is used when new sizes are registered using
--   'Graphics.UI.Gtk.General.IconFactory.iconSizeRegister'.
--
-- * Applying 'show' to this type will reveal the name of the size
--   that is registered with Gtk+.
--
data IconSize
  -- | Don't scale but use any of the available sizes.
  = IconSizeInvalid

  -- | Icon size to use in next to menu items in drop-down menus.
  | IconSizeMenu

  -- | Icon size for small toolbars.
  | IconSizeSmallToolbar

  -- | Icon size for larger toolbars.
  | IconSizeLargeToolbar

  -- | Icon size for icons in buttons, next to the label.
  | IconSizeButton

  -- | Icon size for icons in drag-and-drop.
  | IconSizeDnd

  -- | Icon size for icons next to dialog text.
  | IconSizeDialog

  | IconSizeUser Int
  deriving (Eq)

instance Enum IconSize where
  toEnum 0 = IconSizeInvalid
{-# LINE 760 "Graphics/UI/Gtk/General/Structs.hsc" #-}
  toEnum 1    = IconSizeMenu
{-# LINE 761 "Graphics/UI/Gtk/General/Structs.hsc" #-}
  toEnum 2 = IconSizeSmallToolbar
{-# LINE 762 "Graphics/UI/Gtk/General/Structs.hsc" #-}
  toEnum 3 = IconSizeLargeToolbar
{-# LINE 763 "Graphics/UI/Gtk/General/Structs.hsc" #-}
  toEnum 4 = IconSizeButton
{-# LINE 764 "Graphics/UI/Gtk/General/Structs.hsc" #-}
  toEnum 5 = IconSizeDnd
{-# LINE 765 "Graphics/UI/Gtk/General/Structs.hsc" #-}
  toEnum 6 = IconSizeDialog
{-# LINE 766 "Graphics/UI/Gtk/General/Structs.hsc" #-}
  toEnum n = IconSizeUser n
  fromEnum IconSizeInvalid = 0
{-# LINE 768 "Graphics/UI/Gtk/General/Structs.hsc" #-}
  fromEnum IconSizeMenu = 1
{-# LINE 769 "Graphics/UI/Gtk/General/Structs.hsc" #-}
  fromEnum IconSizeSmallToolbar = 2
{-# LINE 770 "Graphics/UI/Gtk/General/Structs.hsc" #-}
  fromEnum IconSizeLargeToolbar = 3
{-# LINE 771 "Graphics/UI/Gtk/General/Structs.hsc" #-}
  fromEnum IconSizeButton = 4
{-# LINE 772 "Graphics/UI/Gtk/General/Structs.hsc" #-}
  fromEnum IconSizeDnd = 5
{-# LINE 773 "Graphics/UI/Gtk/General/Structs.hsc" #-}
  fromEnum IconSizeDialog = 6
{-# LINE 774 "Graphics/UI/Gtk/General/Structs.hsc" #-}
  fromEnum (IconSizeUser n) = n

-- entry Widget Combo

{-# LINE 778 "Graphics/UI/Gtk/General/Structs.hsc" #-}

{-# LINE 779 "Graphics/UI/Gtk/General/Structs.hsc" #-}
-- | Extract the List container from a 'Combo' box.
--
-- Removed in Gtk3.
comboGetList :: Combo -> IO List
comboGetList c = withForeignPtr (unCombo c) $ \cPtr ->
  makeNewObject mkList $ (\hsc_ptr -> peekByteOff hsc_ptr 160) cPtr
{-# LINE 785 "Graphics/UI/Gtk/General/Structs.hsc" #-}

{-# LINE 786 "Graphics/UI/Gtk/General/Structs.hsc" #-}

{-# LINE 787 "Graphics/UI/Gtk/General/Structs.hsc" #-}

-- FileSelection related methods

{-# LINE 790 "Graphics/UI/Gtk/General/Structs.hsc" #-}
-- | Extract the buttons of a fileselection.
--
fileSelectionGetButtons :: FileSelectionClass fsel => fsel ->
                           IO (Button, Button)
fileSelectionGetButtons fsel =
    do
    ok <- butPtrToButton (\hsc_ptr -> peekByteOff hsc_ptr 304)
{-# LINE 797 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    cancel <- butPtrToButton (\hsc_ptr -> peekByteOff hsc_ptr 312)
{-# LINE 798 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    return (ok,cancel)
  where
  butPtrToButton bp = makeNewObject mkButton $ liftM castPtr $
      withForeignPtr ((unFileSelection . toFileSelection) fsel) bp

{-# LINE 803 "Graphics/UI/Gtk/General/Structs.hsc" #-}


{-# LINE 805 "Graphics/UI/Gtk/General/Structs.hsc" #-}
-- DrawingArea related methods

-- | Retrieves the 'DrawWindow' that the widget draws onto.
--
-- This function thows an error if the widget has not yet been realized, since
-- a widget does not allocate its window resources until just before it is
-- displayed on the screen. You can use the
-- 'Graphics.UI.Gtk.Abstract.Widget.onRealize' signal to give you the
-- opportunity to use a widget's 'DrawWindow' as soon as it has been created
-- but before the widget is displayed.
--
-- Removed in Gtk3.
widgetGetDrawWindow :: WidgetClass widget => widget -> IO DrawWindow
widgetGetDrawWindow da =
  withForeignPtr (unWidget.toWidget $ da) $ \da' -> do
  drawWindowPtr <- (\hsc_ptr -> peekByteOff hsc_ptr 80) da'
{-# LINE 821 "Graphics/UI/Gtk/General/Structs.hsc" #-}
  if drawWindowPtr == nullPtr
    then fail "widgetGetDrawWindow: no DrawWindow available (the widget is probably not realized)"
    else makeNewGObject mkDrawWindow (return $ castPtr drawWindowPtr)

-- | Returns the current size.
--
-- * This information may be out of date if the user is resizing the window.
--
-- Removed in Gtk3.
widgetGetSize :: WidgetClass widget => widget -> IO (Int, Int)
widgetGetSize da = withForeignPtr (unWidget.toWidget $ da) $ \wPtr -> do
    (width :: Int32) <- (\hsc_ptr -> peekByteOff hsc_ptr 8)
{-# LINE 833 "Graphics/UI/Gtk/General/Structs.hsc" #-}
                               ((\hsc_ptr -> hsc_ptr `plusPtr` 64) wPtr)
{-# LINE 834 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    (height :: Int32) <- (\hsc_ptr -> peekByteOff hsc_ptr 12)
{-# LINE 835 "Graphics/UI/Gtk/General/Structs.hsc" #-}
                                ((\hsc_ptr -> hsc_ptr `plusPtr` 64) wPtr)
{-# LINE 836 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    return (fromIntegral width, fromIntegral height)

-- Window related methods

-- | Retrieves the frame 'DrawWindow' that contains a 'Window'.
--
-- Removed in Gtk3.
windowGetFrame :: WindowClass widget => widget -> IO (Maybe DrawWindow)
windowGetFrame da =
  withForeignPtr (unWidget.toWidget $ da) $ \da' -> do
  drawWindowPtr <- (\hsc_ptr -> peekByteOff hsc_ptr 184) da'
{-# LINE 847 "Graphics/UI/Gtk/General/Structs.hsc" #-}
  if drawWindowPtr == nullPtr
    then return Nothing
    else liftM Just $ makeNewGObject mkDrawWindow (return $ castPtr drawWindowPtr)

{-# LINE 851 "Graphics/UI/Gtk/General/Structs.hsc" #-}
-- Styles related methods

-- | Retrieve the the foreground color.
--
-- * The parameter @state@ determines for which widget
--   state (one of 'StateType') the 'Color' should be retrieved.
--   Use 'widgetGetState' to determine the current state of the
--   widget.
--
styleGetForeground :: Style -> StateType -> IO Color
styleGetForeground st ty =
  withForeignPtr (unStyle st) $ \stPtr -> do
    peekElemOff ((\hsc_ptr -> hsc_ptr `plusPtr` 24) stPtr) (fromEnum ty)
{-# LINE 864 "Graphics/UI/Gtk/General/Structs.hsc" #-}

-- | Retrieve the background color.
--
-- * The parameter @state@ determines for which widget
--   state (one of 'StateType') the 'Color' should be retrieved.
--   Use 'widgetGetState' to determine the current state of the
--   widget.
--
styleGetBackground :: Style -> StateType -> IO Color
styleGetBackground st ty =
  withForeignPtr (unStyle st) $ \stPtr ->
    peekElemOff ((\hsc_ptr -> hsc_ptr `plusPtr` 84) stPtr) (fromEnum ty)
{-# LINE 876 "Graphics/UI/Gtk/General/Structs.hsc" #-}

-- | Retrieve a light color.
--
-- * The parameter @state@ determines for which widget
--   state (one of 'StateType') the 'Color' should be retrieved.
--   Use 'widgetGetState' to determine the current state of the
--   widget.
--
styleGetLight :: Style -> StateType -> IO Color
styleGetLight st ty =
  withForeignPtr (unStyle st) $ \stPtr ->
    peekElemOff ((\hsc_ptr -> hsc_ptr `plusPtr` 144) stPtr) (fromEnum ty)
{-# LINE 888 "Graphics/UI/Gtk/General/Structs.hsc" #-}

-- | Retrieve a middle color.
--
-- * The parameter @state@ determines for which widget
--   state (one of 'StateType') the 'Color' should be retrieved.
--   Use 'widgetGetState' to determine the current state of the
--   widget.
--
styleGetMiddle :: Style -> StateType -> IO Color
styleGetMiddle st ty =
  withForeignPtr (unStyle st) $ \stPtr ->
    peekElemOff ((\hsc_ptr -> hsc_ptr `plusPtr` 264) stPtr) (fromEnum ty)
{-# LINE 900 "Graphics/UI/Gtk/General/Structs.hsc" #-}

-- | Retrieve a dark color.
--
-- * The parameter @state@ determines for which widget
--   state (one of 'StateType') the 'Color' should be retrieved.
--   Use 'widgetGetState' to determine the current state of the
--   widget.
--
styleGetDark :: Style -> StateType -> IO Color
styleGetDark st ty =
  withForeignPtr (unStyle st) $ \stPtr ->
    peekElemOff ((\hsc_ptr -> hsc_ptr `plusPtr` 204) stPtr) (fromEnum ty)
{-# LINE 912 "Graphics/UI/Gtk/General/Structs.hsc" #-}

-- | Retrieve the text color.
--
-- * The parameter @state@ determines for which widget
--   state (one of 'StateType') the 'Color' should be retrieved.
--   Use 'widgetGetState' to determine the current state of the
--   widget.
--
styleGetText :: Style -> StateType -> IO Color
styleGetText st ty =
  withForeignPtr (unStyle st) $ \stPtr ->
    peekElemOff ((\hsc_ptr -> hsc_ptr `plusPtr` 324) stPtr) (fromEnum ty)
{-# LINE 924 "Graphics/UI/Gtk/General/Structs.hsc" #-}

-- | Retrieve the base color.
--
-- * The base color is the standard text background of a widget.
--
-- * The parameter @state@ determines for which widget
--   state (one of 'StateType') the 'Color' should be retrieved.
--   Use 'widgetGetState' to determine the current state of the
--   widget.
--
styleGetBase :: Style -> StateType -> IO Color
styleGetBase st ty =
  withForeignPtr (unStyle st) $ \stPtr ->
    peekElemOff ((\hsc_ptr -> hsc_ptr `plusPtr` 384) stPtr) (fromEnum ty)
{-# LINE 938 "Graphics/UI/Gtk/General/Structs.hsc" #-}

-- | Retrieve the color for drawing anti-aliased text.
--
-- * The anti-aliasing color is the color which is used when the rendering
--   of a character does not make it clear if a certain pixel shoud be set
--   or not. This color is between the text and the base color.
--
-- * The parameter @state@ determines for which widget
--   state (one of 'StateType') the 'Color' should be retrieved.
--   Use 'widgetGetState' to determine the current state of the
--   widget.
--
styleGetAntiAliasing :: Style -> StateType -> IO Color
styleGetAntiAliasing st ty =
  withForeignPtr (unStyle st) $ \stPtr ->
    peekElemOff ((\hsc_ptr -> hsc_ptr `plusPtr` 444) stPtr) (fromEnum ty)
{-# LINE 954 "Graphics/UI/Gtk/General/Structs.hsc" #-}


{-# LINE 956 "Graphics/UI/Gtk/General/Structs.hsc" #-}
-- | Retrieve the ColorSelection object contained within the dialog.
--
-- Removed in Gtk3.
colorSelectionDialogGetColor :: ColorSelectionDialog -> IO ColorSelection
colorSelectionDialogGetColor cd =
  makeNewObject mkColorSelection $ liftM castPtr $
    withForeignPtr (unColorSelectionDialog cd)
      (\hsc_ptr -> peekByteOff hsc_ptr 264)
{-# LINE 964 "Graphics/UI/Gtk/General/Structs.hsc" #-}

-- | Retrieve the OK button widget contained within the dialog.
--
-- Removed in Gtk3.
colorSelectionDialogGetOkButton :: ColorSelectionDialog -> IO Button
colorSelectionDialogGetOkButton cd =
  makeNewObject mkButton $ liftM castPtr $
    withForeignPtr (unColorSelectionDialog cd)
      (\hsc_ptr -> peekByteOff hsc_ptr 272)
{-# LINE 973 "Graphics/UI/Gtk/General/Structs.hsc" #-}

-- | Retrieve the Cancel button widget contained within the dialog.
--
-- Removed in Gtk3.
colorSelectionDialogGetCancelButton :: ColorSelectionDialog -> IO Button
colorSelectionDialogGetCancelButton cd =
  makeNewObject mkButton $ liftM castPtr $
    withForeignPtr (unColorSelectionDialog cd)
      (\hsc_ptr -> peekByteOff hsc_ptr 280)
{-# LINE 982 "Graphics/UI/Gtk/General/Structs.hsc" #-}

-- | Retrieve the Help button widget contained within the dialog.
--
-- Removed in Gtk3.
colorSelectionDialogGetHelpButton :: ColorSelectionDialog -> IO Button
colorSelectionDialogGetHelpButton cd =
  makeNewObject mkButton $ liftM castPtr $
    withForeignPtr (unColorSelectionDialog cd)
      (\hsc_ptr -> peekByteOff hsc_ptr 288)
{-# LINE 991 "Graphics/UI/Gtk/General/Structs.hsc" #-}

dragContextGetActions :: DragContext -> IO Int
dragContextGetActions dc = liftM (fromIntegral :: Int32 -> Int) $
{-# LINE 994 "Graphics/UI/Gtk/General/Structs.hsc" #-}
  withForeignPtr (unDragContext dc) (\hsc_ptr -> peekByteOff hsc_ptr 56)
{-# LINE 995 "Graphics/UI/Gtk/General/Structs.hsc" #-}

dragContextSetActions :: DragContext -> Int -> IO ()
dragContextSetActions dc val = withForeignPtr (unDragContext dc) $ \ptr ->
  (\hsc_ptr -> pokeByteOff hsc_ptr 56) ptr (fromIntegral val :: Int32)
{-# LINE 999 "Graphics/UI/Gtk/General/Structs.hsc" #-}

dragContextGetAction :: DragContext -> IO Int
dragContextGetAction dc = liftM (fromIntegral :: Int32 -> Int) $
{-# LINE 1002 "Graphics/UI/Gtk/General/Structs.hsc" #-}
  withForeignPtr (unDragContext dc) (\hsc_ptr -> peekByteOff hsc_ptr 64)
{-# LINE 1003 "Graphics/UI/Gtk/General/Structs.hsc" #-}

dragContextSetAction :: DragContext -> Int -> IO ()
dragContextSetAction dc val = withForeignPtr (unDragContext dc) $ \ptr ->
  (\hsc_ptr -> pokeByteOff hsc_ptr 64) ptr (fromIntegral val :: Int32)
{-# LINE 1007 "Graphics/UI/Gtk/General/Structs.hsc" #-}

dragContextGetSuggestedAction :: DragContext -> IO Int
dragContextGetSuggestedAction dc = liftM (fromIntegral :: Int32 -> Int) $
{-# LINE 1010 "Graphics/UI/Gtk/General/Structs.hsc" #-}
  withForeignPtr (unDragContext dc) (\hsc_ptr -> peekByteOff hsc_ptr 60)
{-# LINE 1011 "Graphics/UI/Gtk/General/Structs.hsc" #-}

dragContextSetSuggestedAction :: DragContext -> Int -> IO ()
dragContextSetSuggestedAction dc val = withForeignPtr (unDragContext dc) $ \ptr ->
  (\hsc_ptr -> pokeByteOff hsc_ptr 60) ptr (fromIntegral val :: Int32)
{-# LINE 1015 "Graphics/UI/Gtk/General/Structs.hsc" #-}

{-# LINE 1016 "Graphics/UI/Gtk/General/Structs.hsc" #-}

-- | ID number of a sort column.
--
-- * A 'SortColumnId' is a logical number to which a sorting function can
--   be associated. The number does not have to coincide with any column
--   number.
type SortColumnId = Int

-- | A special 'SortColumnId' to indicated that the default sorting function is used.
--
treeSortableDefaultSortColumnId :: SortColumnId
treeSortableDefaultSortColumnId = -1
{-# LINE 1028 "Graphics/UI/Gtk/General/Structs.hsc" #-}

intToAtom :: Int -> Atom
intToAtom = Atom . plusPtr nullPtr

-- | An invalid 'TargetTag', 'SelectionTag', 'SelectionTypeTag' or 'PropertyTag'.
--
tagInvalid :: Atom
tagInvalid = intToAtom 0
{-# LINE 1036 "Graphics/UI/Gtk/General/Structs.hsc" #-}

-- | The primary selection (the currently highlighted text in X11 that can
--   in many applications be pasted using the middle button).
selectionPrimary :: SelectionTag
selectionPrimary = intToAtom 1
{-# LINE 1041 "Graphics/UI/Gtk/General/Structs.hsc" #-}

-- | The secondary selection. Rarely used.
selectionSecondary :: SelectionTag
selectionSecondary = intToAtom 2
{-# LINE 1045 "Graphics/UI/Gtk/General/Structs.hsc" #-}

-- | The modern clipboard that is filled by copy or cut commands.
selectionClipboard :: SelectionTag
selectionClipboard = intToAtom 69
{-# LINE 1049 "Graphics/UI/Gtk/General/Structs.hsc" #-}

-- | If this target is provided by a selection, then the data is a string.
targetString :: TargetTag
targetString = intToAtom 31
{-# LINE 1053 "Graphics/UI/Gtk/General/Structs.hsc" #-}

-- | The type indicating that the associated data is itself a (list of)
-- 'Graphics.UI.Gtk.General.Selection.Atom's.
selectionTypeAtom :: SelectionTypeTag
selectionTypeAtom = intToAtom 4
{-# LINE 1058 "Graphics/UI/Gtk/General/Structs.hsc" #-}

-- | The type indicating that the associated data consists of integers.
selectionTypeInteger :: SelectionTypeTag
selectionTypeInteger = intToAtom 19
{-# LINE 1062 "Graphics/UI/Gtk/General/Structs.hsc" #-}

-- | The type indicating that the associated data is a string without further
-- information on its encoding.
selectionTypeString :: SelectionTypeTag
selectionTypeString = intToAtom 31
{-# LINE 1067 "Graphics/UI/Gtk/General/Structs.hsc" #-}


{-# LINE 1069 "Graphics/UI/Gtk/General/Structs.hsc" #-}
-- | Extract the type field of SelectionData*. This should be in the
--   Selection modules but c2hs chokes on the 'type' field.
selectionDataGetType :: Ptr () -> IO SelectionTypeTag
selectionDataGetType selPtr =
  liftM intToAtom $ (\hsc_ptr -> peekByteOff hsc_ptr 16) selPtr
{-# LINE 1074 "Graphics/UI/Gtk/General/Structs.hsc" #-}

{-# LINE 1075 "Graphics/UI/Gtk/General/Structs.hsc" #-}

-- A type that identifies a target. This is needed to marshal arrays of
-- GtkTargetEntries.
data TargetEntry = TargetEntry (Ptr Int8) InfoId
{-# LINE 1079 "Graphics/UI/Gtk/General/Structs.hsc" #-}

-- brain damaged API: the whole selection API doesn't need GtkTargetEntry
-- structure, but stupid Clipboard has two functions that only provide this
-- interface. Thus, convert the efficient Atoms back into strings, have
-- the clipboard functions convert them back to string before we get a
-- chance to free the freshly allocated strings.

withTargetEntries :: [(TargetTag, InfoId)] -> (Int -> Ptr () -> IO a) -> IO a
withTargetEntries tags fun = do
  ptrsInfo <- mapM (\(Atom tag, info) -> gdk_atom_name tag >>= \strPtr ->
                     return (TargetEntry strPtr info)) tags
  res <- withArrayLen ptrsInfo (\len ptr -> fun len (castPtr ptr))
  mapM_ (\(TargetEntry ptr _) -> g_free ptr) ptrsInfo
  return res

foreign import ccall unsafe "gdk_atom_name"
  gdk_atom_name :: Ptr () -> IO (Ptr Int8)
{-# LINE 1096 "Graphics/UI/Gtk/General/Structs.hsc" #-}

foreign import ccall unsafe "g_free"
  g_free :: Ptr Int8 -> IO ()
{-# LINE 1099 "Graphics/UI/Gtk/General/Structs.hsc" #-}

instance Storable TargetEntry where
  sizeOf _ = 16
{-# LINE 1102 "Graphics/UI/Gtk/General/Structs.hsc" #-}
  alignment _ = alignment (undefined::Word32)
{-# LINE 1103 "Graphics/UI/Gtk/General/Structs.hsc" #-}
  peek ptr = undefined
  poke ptr (TargetEntry cPtr info) = do
    (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr cPtr
{-# LINE 1106 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr (0::Word32)
{-# LINE 1107 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 12) ptr info
{-# LINE 1108 "Graphics/UI/Gtk/General/Structs.hsc" #-}

-- | A 'KeymapKey' is a hardware key that can be mapped to a keyval.
data KeymapKey = KeymapKey {
       keycode   :: Int -- ^ @keycode@ the hardware keycode. This is an identifying number for a physical key.
      ,group     :: Int -- ^ @group@ indicates movement in a horizontal direction.
                      -- Usually groups are used for two different languages.
                      -- In group  0, a key might have two English characters,
                      -- and in group 1 it might have two Hebrew characters.
                      -- The Hebrew characters will be printed on the key next to the English characters.
                      -- indicates which symbol on the key will be used,
                      -- in a vertical direction. So on a standard US keyboard, the
      ,level     :: Int -- ^ @level@ key with the number "1" on it also has the exclamation
                      -- point ("!") character on it. The level
                      -- indicates whether to use the "1" or the "!" symbol. The letter keys are considered to
                      -- have a lowercase letter at level 0, and an uppercase letter at level 1, though only
                      -- the uppercase letter is printed.
    } deriving (Eq, Show)

instance Storable KeymapKey where
  sizeOf _ = 12
{-# LINE 1128 "Graphics/UI/Gtk/General/Structs.hsc" #-}
  alignment _ = alignment (undefined::Int32)
{-# LINE 1129 "Graphics/UI/Gtk/General/Structs.hsc" #-}
  peek ptr = do
    (keycode_  ::Word32)    <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 1131 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    (group_  ::Int32)       <- (\hsc_ptr -> peekByteOff hsc_ptr 4) ptr
{-# LINE 1132 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    (level_ ::Int32)        <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 1133 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    return $ KeymapKey (fromIntegral keycode_) (fromIntegral group_) (fromIntegral level_)
  poke ptr (KeymapKey keycode group level) = do
    (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr ((fromIntegral keycode)::Word32)
{-# LINE 1136 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 4) ptr ((fromIntegral group)::Int32)
{-# LINE 1137 "Graphics/UI/Gtk/General/Structs.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr ((fromIntegral level)::Int32)
{-# LINE 1138 "Graphics/UI/Gtk/General/Structs.hsc" #-}


