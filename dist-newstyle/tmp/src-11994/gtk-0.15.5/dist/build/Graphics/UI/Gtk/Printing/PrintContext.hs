{-# LANGUAGE OverloadedStrings #-}

{-# LINE 2 "./Graphics/UI/Gtk/Printing/PrintContext.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget PrintContext
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
-- Encapsulates context for drawing pages
--
-- * Module available since Gtk+ version 2.10
--
module Graphics.UI.Gtk.Printing.PrintContext (

-- * Detail
--
-- | A 'PrintContext' encapsulates context information that is required when
-- drawing pages for printing, such as the cairo context and important
-- parameters like page size and resolution. It also lets you easily create
-- 'PangoLayout' and 'Context' objects that match the font metrics of the cairo
-- surface.
--
-- 'PrintContext' objects gets passed to the 'beginPrint', 'endPrint',
-- 'requestPageSetup' and 'drawPage' signals on the 'PrintOperation'.
--
-- Printing support was added in Gtk+ 2.10.

-- * Class Hierarchy
--
-- |
-- @
-- | 'GObject'
-- | +----PrintContext
-- @


-- * Types
  PrintContext,
  PrintContextClass,
  castToPrintContext,
  toPrintContext,

-- * Methods
  printContextGetCairoContext,
  printContextSetCairoContext,
  printContextGetPageSetup,
  printContextGetWidth,
  printContextGetHeight,
  printContextGetDpiX,
  printContextGetDpiY,
  printContextGetPangoFontmap,
  printContextCreatePangoContext,
  printContextCreatePangoLayout,

  printContextGetHardMargins,


  ) where

import Control.Monad (liftM)
import Data.IORef (newIORef)

import System.Glib.FFI
import System.Glib.UTFString
import Graphics.UI.Gtk.Types
{-# LINE 83 "./Graphics/UI/Gtk/Printing/PrintContext.chs" #-}
import Graphics.Rendering.Pango.Types
{-# LINE 84 "./Graphics/UI/Gtk/Printing/PrintContext.chs" #-}
import Graphics.Rendering.Pango.BasicTypes
{-# LINE 85 "./Graphics/UI/Gtk/Printing/PrintContext.chs" #-}
import Graphics.Rendering.Cairo.Types
{-# LINE 86 "./Graphics/UI/Gtk/Printing/PrintContext.chs" #-}


{-# LINE 88 "./Graphics/UI/Gtk/Printing/PrintContext.chs" #-}


--------------------
-- Methods

-- | Obtains the cairo context that is associated with the 'PrintContext'.
--
printContextGetCairoContext :: PrintContextClass self => self
 -> IO Cairo -- ^ returns the cairo context of @context@
printContextGetCairoContext self =
  liftM Cairo $
  (\(PrintContext arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_context_get_cairo_context argPtr1) (toPrintContext self)

-- | Sets a new cairo context on a print context.
--
-- This function is intended to be used when implementing an internal print
-- preview, it is not needed for printing, since Gtk+ itself creates a suitable
-- cairo context in that case.
--
printContextSetCairoContext :: PrintContextClass self => self
 -> Cairo -- ^ @cr@ - the cairo context
 -> Double -- ^ @dpiX@ - the horizontal resolution to use with @cr@
 -> Double -- ^ @dpiY@ - the vertical resolution to use with @cr@
 -> IO ()
printContextSetCairoContext self cr dpiX dpiY =
  (\(PrintContext arg1) (Cairo arg2) arg3 arg4 -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_context_set_cairo_context argPtr1 arg2 arg3 arg4)
{-# LINE 114 "./Graphics/UI/Gtk/Printing/PrintContext.chs" #-}
    (toPrintContext self)
    cr
    (realToFrac dpiX)
    (realToFrac dpiY)

-- | Obtains the 'PageSetup' that determines the page dimensions of the
-- 'PrintContext'.
--
printContextGetPageSetup :: PrintContextClass self => self
 -> IO PageSetup -- ^ returns the page setup of @context@
printContextGetPageSetup self =
  makeNewGObject mkPageSetup $
  (\(PrintContext arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_context_get_page_setup argPtr1)
{-# LINE 127 "./Graphics/UI/Gtk/Printing/PrintContext.chs" #-}
    (toPrintContext self)

-- | Obtains the width of the 'PrintContext', in pixels.
--
printContextGetWidth :: PrintContextClass self => self
 -> IO Double -- ^ returns the width of @context@
printContextGetWidth self =
  liftM realToFrac $
  (\(PrintContext arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_context_get_width argPtr1)
{-# LINE 136 "./Graphics/UI/Gtk/Printing/PrintContext.chs" #-}
    (toPrintContext self)

-- | Obtains the height of the 'PrintContext', in pixels.
--
printContextGetHeight :: PrintContextClass self => self
 -> IO Double -- ^ returns the height of @context@
printContextGetHeight self =
  liftM realToFrac $
  (\(PrintContext arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_context_get_height argPtr1)
{-# LINE 145 "./Graphics/UI/Gtk/Printing/PrintContext.chs" #-}
    (toPrintContext self)

-- | Obtains the horizontal resolution of the 'PrintContext', in dots per
-- inch.
--
printContextGetDpiX :: PrintContextClass self => self
 -> IO Double -- ^ returns the horizontal resolution of @context@
printContextGetDpiX self =
  liftM realToFrac $
  (\(PrintContext arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_context_get_dpi_x argPtr1)
{-# LINE 155 "./Graphics/UI/Gtk/Printing/PrintContext.chs" #-}
    (toPrintContext self)

-- | Obtains the vertical resolution of the 'PrintContext', in dots per inch.
--
printContextGetDpiY :: PrintContextClass self => self
 -> IO Double -- ^ returns the vertical resolution of @context@
printContextGetDpiY self =
  liftM realToFrac $
  (\(PrintContext arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_context_get_dpi_y argPtr1)
{-# LINE 164 "./Graphics/UI/Gtk/Printing/PrintContext.chs" #-}
    (toPrintContext self)

-- | Returns a 'FontMap' that is suitable for use with the 'PrintContext'.
--
printContextGetPangoFontmap :: PrintContextClass self => self
 -> IO FontMap -- ^ returns the font map of @context@
printContextGetPangoFontmap self =
  makeNewGObject mkFontMap $
  (\(PrintContext arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_context_get_pango_fontmap argPtr1)
{-# LINE 173 "./Graphics/UI/Gtk/Printing/PrintContext.chs" #-}
    (toPrintContext self)

-- | Creates a new 'Context' that can be used with the 'PrintContext'.
--
printContextCreatePangoContext :: PrintContextClass self => self
 -> IO PangoContext -- ^ returns a new Pango context for @context@
printContextCreatePangoContext self =
  wrapNewGObject mkPangoContext $
  (\(PrintContext arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_context_create_pango_context argPtr1)
{-# LINE 182 "./Graphics/UI/Gtk/Printing/PrintContext.chs" #-}
    (toPrintContext self)

-- | Creates a new 'PangoLayout' that is suitable for use with the
-- 'PrintContext'.
--
printContextCreatePangoLayout :: PrintContextClass self => self
 -> IO PangoLayout -- ^ returns a new Pango layout for @context@
printContextCreatePangoLayout self = do
  pl <- wrapNewGObject mkPangoLayoutRaw $
    (\(PrintContext arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_context_create_pango_layout argPtr1)
{-# LINE 192 "./Graphics/UI/Gtk/Printing/PrintContext.chs" #-}
    (toPrintContext self)
  ps <- makeNewPangoString (""::DefaultGlibString)
  psRef <- newIORef ps
  return (PangoLayout psRef pl)


printContextGetHardMargins :: PrintContextClass self => self
                           -> IO (Maybe (Double, Double, Double, Double))
                             -- ^ returns @(top, bottom, left, right)@
                             -- @top@ top hardware printer margin
                             -- @bottom@ bottom hardware printer margin
                             -- @left@ left hardware printer margin
                             -- @right@ right hardware printer margin
printContextGetHardMargins self =
  alloca $ \ topPtr ->
  alloca $ \ bottomPtr ->
  alloca $ \ leftPtr ->
  alloca $ \ rightPtr -> do
  success <- liftM toBool $ (\(PrintContext arg1) arg2 arg3 arg4 arg5 -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_context_get_hard_margins argPtr1 arg2 arg3 arg4 arg5)
{-# LINE 211 "./Graphics/UI/Gtk/Printing/PrintContext.chs" #-}
               (toPrintContext self)
               topPtr
               bottomPtr
               leftPtr
               rightPtr
  if success
     then do
       top <- liftM realToFrac $ peek topPtr
       bottom <- liftM realToFrac $ peek bottomPtr
       left <- liftM realToFrac $ peek leftPtr
       right <- liftM realToFrac $ peek rightPtr
       return $ Just (top, bottom, left, right)
     else return Nothing

foreign import ccall safe "gtk_print_context_get_cairo_context"
  gtk_print_context_get_cairo_context :: ((Ptr PrintContext) -> (IO (Ptr Cairo)))

foreign import ccall safe "gtk_print_context_set_cairo_context"
  gtk_print_context_set_cairo_context :: ((Ptr PrintContext) -> ((Ptr Cairo) -> (CDouble -> (CDouble -> (IO ())))))

foreign import ccall safe "gtk_print_context_get_page_setup"
  gtk_print_context_get_page_setup :: ((Ptr PrintContext) -> (IO (Ptr PageSetup)))

foreign import ccall safe "gtk_print_context_get_width"
  gtk_print_context_get_width :: ((Ptr PrintContext) -> (IO CDouble))

foreign import ccall safe "gtk_print_context_get_height"
  gtk_print_context_get_height :: ((Ptr PrintContext) -> (IO CDouble))

foreign import ccall safe "gtk_print_context_get_dpi_x"
  gtk_print_context_get_dpi_x :: ((Ptr PrintContext) -> (IO CDouble))

foreign import ccall safe "gtk_print_context_get_dpi_y"
  gtk_print_context_get_dpi_y :: ((Ptr PrintContext) -> (IO CDouble))

foreign import ccall safe "gtk_print_context_get_pango_fontmap"
  gtk_print_context_get_pango_fontmap :: ((Ptr PrintContext) -> (IO (Ptr FontMap)))

foreign import ccall safe "gtk_print_context_create_pango_context"
  gtk_print_context_create_pango_context :: ((Ptr PrintContext) -> (IO (Ptr PangoContext)))

foreign import ccall safe "gtk_print_context_create_pango_layout"
  gtk_print_context_create_pango_layout :: ((Ptr PrintContext) -> (IO (Ptr PangoLayoutRaw)))

foreign import ccall safe "gtk_print_context_get_hard_margins"
  gtk_print_context_get_hard_margins :: ((Ptr PrintContext) -> ((Ptr CDouble) -> ((Ptr CDouble) -> ((Ptr CDouble) -> ((Ptr CDouble) -> (IO CInt))))))
