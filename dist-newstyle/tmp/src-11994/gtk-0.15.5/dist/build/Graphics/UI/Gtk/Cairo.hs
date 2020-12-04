
{-# LINE 2 "./Graphics/UI/Gtk/Cairo.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Cairo GDK integration
--
-- Author : Duncan Coutts
--
-- Created: 17 August 2005
--
-- Copyright (C) 2005 Duncan Coutts
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
-- #hide

-- |
-- Maintainer : gtk2hs-users@lists.sourceforge.net
-- Stability : provisional
-- Portability : portable (depends on GHC)
--
--
-- Gtk specific functions to for rendering with Cairo.
--
-- Cairo is a graphics library that supports vector graphics and image
-- compositing that can be used with Gdk.
-- The Cairo API is an addition to Gdk\/Gtk (rather than a replacement).
-- Cairo rendering can be performed on any 'Graphics.UI.Gtk.Gdk.Drawable'
-- by calling 'renderWithDrawable'. The functions in this module provide
-- ways of drawing Gtk specific elements, such as 'Pixbuf's or text
-- laid out with Pango.
--
-- All functions in this module are only available in Gtk 2.8 or higher.
--
module Graphics.UI.Gtk.Cairo (

  -- * Global Cairo settings.
  cairoFontMapGetDefault,
  cairoFontMapSetResolution,
  cairoFontMapGetResolution,
  cairoCreateContext,
  cairoContextSetResolution,
  cairoContextGetResolution,
  cairoContextSetFontOptions,
  cairoContextGetFontOptions,
  -- * Functions for the 'Render' monad.

  renderWithDrawable,




  region,
  setSourceColor,
  setSourcePixbuf,
  rectangle,
  updateContext,
  createLayout,
  updateLayout,
  showGlyphString,
  showLayoutLine,
  showLayout,
  glyphStringPath,
  layoutLinePath,
  layoutPath

  ) where

import Control.Exception (bracket)

import System.Glib.FFI
import Graphics.UI.Gtk.Types
{-# LINE 79 "./Graphics/UI/Gtk/Cairo.chs" #-}

import Graphics.UI.Gtk.Gdk.Region (Region(..))

import Graphics.Rendering.Pango.Cairo
{-# LINE 83 "./Graphics/UI/Gtk/Cairo.chs" #-}



import Graphics.Rendering.Cairo.Types as Cairo hiding (Region)



import qualified Graphics.Rendering.Cairo.Internal as Cairo.Internal
import Graphics.Rendering.Cairo.Internal (Render(Render))
import Control.Monad.Reader
import Graphics.UI.Gtk.General.Structs (Rectangle(..))



{-# LINE 97 "./Graphics/UI/Gtk/Cairo.chs" #-}

--------------------
-- Methods



-- | Creates a Cairo context for drawing to a 'Drawable'.
--
-- Removed in Gtk3.
renderWithDrawable :: DrawableClass drawable =>
    drawable -- ^ @drawable@ - a 'Drawable'
 -> Render a -- ^ A newly created Cairo context.
 -> IO a
renderWithDrawable drawable m =
  bracket (liftM Cairo.Cairo $ (\(Drawable arg1) -> withForeignPtr arg1 $ \argPtr1 ->gdk_cairo_create argPtr1) (toDrawable drawable))
          (\context -> do status <- Cairo.Internal.status context
                          Cairo.Internal.destroy context
                          unless (status == Cairo.StatusSuccess) $
                            fail =<< Cairo.Internal.statusToString status)
          (\context -> runReaderT (Cairo.Internal.runRender m) context)
{-# LINE 148 "./Graphics/UI/Gtk/Cairo.chs" #-}
-- | Sets the given pixbuf as the source pattern for the Cairo context. The
-- pattern has an extend mode of 'ExtendNone' and is aligned so that the
-- origin of pixbuf is @(x, y)@.
--
setSourcePixbuf ::
    Pixbuf
 -> Double -- ^ x
 -> Double -- ^ y
 -> Render ()
setSourcePixbuf pixbuf pixbufX pixbufY = Render $ do
  cr <- ask
  liftIO $ (\(Cairo arg1) (Pixbuf arg2) arg3 arg4 -> withForeignPtr arg2 $ \argPtr2 ->gdk_cairo_set_source_pixbuf arg1 argPtr2 arg3 arg4)
{-# LINE 160 "./Graphics/UI/Gtk/Cairo.chs" #-}
    cr
    pixbuf
    (realToFrac pixbufX)
    (realToFrac pixbufY)

-- | Adds the given region to the current path of the 'Render' context.
rectangle :: Rectangle -> Render ()
rectangle rect = Render $ do
  cr <- ask
  liftIO $ with rect $ \ rectPtr ->
    (\(Cairo arg1) arg2 -> gdk_cairo_rectangle arg1 arg2)
{-# LINE 171 "./Graphics/UI/Gtk/Cairo.chs" #-}
      cr
      (castPtr rectPtr)

-- | Adds the given region to the current path of the 'Render' context.
region :: Region -> Render ()
region region = Render $ do
  cr <- ask
  liftIO $ (\(Cairo arg1) (Region arg2) -> withForeignPtr arg2 $ \argPtr2 ->gdk_cairo_region arg1 argPtr2)
{-# LINE 179 "./Graphics/UI/Gtk/Cairo.chs" #-}
    cr
    region

foreign import ccall unsafe "gdk_cairo_create"
  gdk_cairo_create :: ((Ptr Drawable) -> (IO (Ptr Cairo)))

foreign import ccall unsafe "gdk_cairo_set_source_pixbuf"
  gdk_cairo_set_source_pixbuf :: ((Ptr Cairo) -> ((Ptr Pixbuf) -> (CDouble -> (CDouble -> (IO ())))))

foreign import ccall unsafe "gdk_cairo_rectangle"
  gdk_cairo_rectangle :: ((Ptr Cairo) -> ((Ptr ()) -> (IO ())))

foreign import ccall unsafe "gdk_cairo_region"
  gdk_cairo_region :: ((Ptr Cairo) -> ((Ptr Region) -> (IO ())))
