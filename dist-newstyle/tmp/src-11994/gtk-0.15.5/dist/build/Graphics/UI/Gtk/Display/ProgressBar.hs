
{-# LINE 2 "./Graphics/UI/Gtk/Display/ProgressBar.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget ProgressBar
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
-- A widget which indicates progress visually
--
module Graphics.UI.Gtk.Display.ProgressBar (
-- * Detail
--
-- | The 'ProgressBar' is typically used to display the progress of a long
-- running operation. It provides a visual clue that processing is underway.
-- The 'ProgressBar' can be used in two different modes: percentage mode and
-- activity mode.
--
-- When an application can determine how much work needs to take place (e.g.
-- read a fixed number of bytes from a file) and can monitor its progress, it
-- can use the 'ProgressBar' in percentage mode and the user sees a growing bar
-- indicating the percentage of the work that has been completed. In this mode,
-- the application is required to call 'progressBarSetFraction' periodically to
-- update the progress bar.
--
-- When an application has no accurate way of knowing the amount of work to
-- do, it can use the 'ProgressBar' in activity mode, which shows activity by a
-- block moving back and forth within the progress area. In this mode, the
-- application is required to call 'progressBarPulse' perodically to update the
-- progress bar.
--
-- There is quite a bit of flexibility provided to control the appearance of
-- the 'ProgressBar'. Functions are provided to control the orientation of the
-- bar, optional text can be displayed along with the bar, and the step size
-- used in activity mode can be set.

-- * Class Hierarchy
-- |
-- @
-- | 'GObject'
-- | +----'Object'
-- | +----'Widget'
-- | +----ProgressBar
-- @

-- * Types
  ProgressBar,
  ProgressBarClass,
  castToProgressBar, gTypeProgressBar,
  toProgressBar,

-- * Constructors
  progressBarNew,

-- * Methods
  progressBarPulse,
  progressBarSetText,
  progressBarSetFraction,
  progressBarSetPulseStep,
  progressBarGetFraction,
  progressBarGetPulseStep,
  progressBarGetText,

  ProgressBarOrientation(..),
  progressBarSetOrientation,
  progressBarGetOrientation,


  progressBarSetEllipsize,
  progressBarGetEllipsize,


-- * Attributes

  progressBarOrientation,

  progressBarDiscreteBlocks,
  progressBarFraction,
  progressBarPulseStep,
  progressBarText,

  progressBarEllipsize,

  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.UI.Gtk.Types
{-# LINE 111 "./Graphics/UI/Gtk/Display/ProgressBar.chs" #-}

import Graphics.UI.Gtk.General.Enums (ProgressBarOrientation(..))


import Graphics.Rendering.Pango.Enums (EllipsizeMode(..))



{-# LINE 119 "./Graphics/UI/Gtk/Display/ProgressBar.chs" #-}

--------------------
-- Constructors

-- | Creates a new 'ProgressBar'.
--
progressBarNew :: IO ProgressBar
progressBarNew =
  makeNewObject mkProgressBar $
  liftM (castPtr :: Ptr Widget -> Ptr ProgressBar) $
  gtk_progress_bar_new
{-# LINE 130 "./Graphics/UI/Gtk/Display/ProgressBar.chs" #-}

--------------------
-- Methods

-- | Indicates that some progress is made, but you don't know how much. Causes
-- the progress bar to enter \"activity mode\", where a block bounces back and
-- forth. Each call to 'progressBarPulse' causes the block to move by a little
-- bit (the amount of movement per pulse is determined by
-- 'progressBarSetPulseStep').
--
progressBarPulse :: ProgressBarClass self => self -> IO ()
progressBarPulse self =
  (\(ProgressBar arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_progress_bar_pulse argPtr1)
{-# LINE 143 "./Graphics/UI/Gtk/Display/ProgressBar.chs" #-}
    (toProgressBar self)

-- | Causes the given @text@ to appear superimposed on the progress bar.
--
progressBarSetText :: (ProgressBarClass self, GlibString string) => self -> string -> IO ()
progressBarSetText self text =
  withUTFString text $ \textPtr ->
  (\(ProgressBar arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_progress_bar_set_text argPtr1 arg2)
{-# LINE 151 "./Graphics/UI/Gtk/Display/ProgressBar.chs" #-}
    (toProgressBar self)
    textPtr

-- | Causes the progress bar to \"fill in\" the given fraction of the bar. The
-- fraction should be between 0.0 and 1.0, inclusive.
--
progressBarSetFraction :: ProgressBarClass self => self
 -> Double -- ^ @fraction@ - fraction of the task that's been completed
 -> IO ()
progressBarSetFraction self fraction =
  (\(ProgressBar arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_progress_bar_set_fraction argPtr1 arg2)
{-# LINE 162 "./Graphics/UI/Gtk/Display/ProgressBar.chs" #-}
    (toProgressBar self)
    (realToFrac fraction)

-- | Sets the fraction of total progress bar length to move the bouncing block
-- for each call to 'progressBarPulse'.
--
progressBarSetPulseStep :: ProgressBarClass self => self
 -> Double -- ^ @fraction@ - fraction between 0.0 and 1.0
 -> IO ()
progressBarSetPulseStep self fraction =
  (\(ProgressBar arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_progress_bar_set_pulse_step argPtr1 arg2)
{-# LINE 173 "./Graphics/UI/Gtk/Display/ProgressBar.chs" #-}
    (toProgressBar self)
    (realToFrac fraction)

-- | Returns the current fraction of the task that's been completed.
--
progressBarGetFraction :: ProgressBarClass self => self
 -> IO Double -- ^ returns a fraction from 0.0 to 1.0
progressBarGetFraction self =
  liftM realToFrac $
  (\(ProgressBar arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_progress_bar_get_fraction argPtr1)
{-# LINE 183 "./Graphics/UI/Gtk/Display/ProgressBar.chs" #-}
    (toProgressBar self)

-- | Retrieves the pulse step set with 'progressBarSetPulseStep'
--
progressBarGetPulseStep :: ProgressBarClass self => self
 -> IO Double -- ^ returns a fraction from 0.0 to 1.0
progressBarGetPulseStep self =
  liftM realToFrac $
  (\(ProgressBar arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_progress_bar_get_pulse_step argPtr1)
{-# LINE 192 "./Graphics/UI/Gtk/Display/ProgressBar.chs" #-}
    (toProgressBar self)

-- | Retrieves the text displayed superimposed on the progress bar, if any,
-- otherwise @Nothing@.
--
progressBarGetText :: (ProgressBarClass self, GlibString string) => self
 -> IO (Maybe string) -- ^ returns text, or @Nothing@
progressBarGetText self =
  (\(ProgressBar arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_progress_bar_get_text argPtr1)
{-# LINE 201 "./Graphics/UI/Gtk/Display/ProgressBar.chs" #-}
    (toProgressBar self)
  >>= maybePeek peekUTFString


-- | Causes the progress bar to switch to a different orientation
-- (left-to-right, right-to-left, top-to-bottom, or bottom-to-top).
--
progressBarSetOrientation :: ProgressBarClass self => self -> ProgressBarOrientation -> IO ()
progressBarSetOrientation self orientation =
  (\(ProgressBar arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_progress_bar_set_orientation argPtr1 arg2)
{-# LINE 211 "./Graphics/UI/Gtk/Display/ProgressBar.chs" #-}
    (toProgressBar self)
    ((fromIntegral . fromEnum) orientation)

-- | Retrieves the current progress bar orientation.
--
progressBarGetOrientation :: ProgressBarClass self => self -> IO ProgressBarOrientation
progressBarGetOrientation self =
  liftM (toEnum . fromIntegral) $
  (\(ProgressBar arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_progress_bar_get_orientation argPtr1)
{-# LINE 220 "./Graphics/UI/Gtk/Display/ProgressBar.chs" #-}
    (toProgressBar self)



-- | Sets the mode used to ellipsize (add an ellipsis: \"...\") the text if
-- there is not enough space to render the entire string.
--
-- * Available since Gtk+ version 2.6
--
progressBarSetEllipsize :: ProgressBarClass self => self -> EllipsizeMode -> IO ()
progressBarSetEllipsize self mode =
  (\(ProgressBar arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_progress_bar_set_ellipsize argPtr1 arg2)
{-# LINE 232 "./Graphics/UI/Gtk/Display/ProgressBar.chs" #-}
    (toProgressBar self)
    ((fromIntegral . fromEnum) mode)

-- | Returns the ellipsizing position of the progressbar. See
-- 'progressBarSetEllipsize'.
--
-- * Available since Gtk+ version 2.6
--
progressBarGetEllipsize :: ProgressBarClass self => self -> IO EllipsizeMode
progressBarGetEllipsize self =
  liftM (toEnum . fromIntegral) $
  (\(ProgressBar arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_progress_bar_get_ellipsize argPtr1)
{-# LINE 244 "./Graphics/UI/Gtk/Display/ProgressBar.chs" #-}
    (toProgressBar self)


--------------------
-- Attributes

-- | Orientation and growth direction of the progress bar.
--
-- Default value: 'ProgressLeftToRight'
--
-- Removed in Gtk3.
progressBarOrientation :: ProgressBarClass self => Attr self ProgressBarOrientation
progressBarOrientation = newAttr
  progressBarGetOrientation
  progressBarSetOrientation


-- | The number of discrete blocks in a progress bar (when shown in the
-- discrete style).
--
-- Allowed values: >= 2
--
-- Default value: 10
--
progressBarDiscreteBlocks :: ProgressBarClass self => Attr self Int
progressBarDiscreteBlocks = newAttrFromUIntProperty "discrete-blocks"

-- | The fraction of total work that has been completed.
--
-- Allowed values: [0,1]
--
-- Default value: 0
--
progressBarFraction :: ProgressBarClass self => Attr self Double
progressBarFraction = newAttr
  progressBarGetFraction
  progressBarSetFraction

-- | The fraction of total progress to move the bouncing block when pulsed.
--
-- Allowed values: [0,1]
--
-- Default value: 0.1
--
progressBarPulseStep :: ProgressBarClass self => Attr self Double
progressBarPulseStep = newAttr
  progressBarGetPulseStep
  progressBarSetPulseStep

-- | Text to be displayed in the progress bar.
--
-- Default value: \"%P %%\"
--
progressBarText :: (ProgressBarClass self, GlibString string) => ReadWriteAttr self (Maybe string) string
progressBarText = newAttr
  progressBarGetText
  progressBarSetText


-- | The preferred place to ellipsize the string, if the progressbar does not
-- have enough room to display the entire string, specified as a
-- 'EllipsizeMode'.
--
-- Note that setting this property to a value other than 'EllipsizeNone' has
-- the side-effect that the progressbar requests only enough space to display
-- the ellipsis \"...\". Another means to set a progressbar's width is
-- 'Graphics.UI.Gtk.Abstract.Widget.widgetSetSizeRequest'.
--
-- Default value: 'EllipsizeNone'
--
progressBarEllipsize :: ProgressBarClass self => Attr self EllipsizeMode
progressBarEllipsize = newAttr
  progressBarGetEllipsize
  progressBarSetEllipsize

foreign import ccall unsafe "gtk_progress_bar_new"
  gtk_progress_bar_new :: (IO (Ptr Widget))

foreign import ccall unsafe "gtk_progress_bar_pulse"
  gtk_progress_bar_pulse :: ((Ptr ProgressBar) -> (IO ()))

foreign import ccall unsafe "gtk_progress_bar_set_text"
  gtk_progress_bar_set_text :: ((Ptr ProgressBar) -> ((Ptr CChar) -> (IO ())))

foreign import ccall unsafe "gtk_progress_bar_set_fraction"
  gtk_progress_bar_set_fraction :: ((Ptr ProgressBar) -> (CDouble -> (IO ())))

foreign import ccall unsafe "gtk_progress_bar_set_pulse_step"
  gtk_progress_bar_set_pulse_step :: ((Ptr ProgressBar) -> (CDouble -> (IO ())))

foreign import ccall unsafe "gtk_progress_bar_get_fraction"
  gtk_progress_bar_get_fraction :: ((Ptr ProgressBar) -> (IO CDouble))

foreign import ccall unsafe "gtk_progress_bar_get_pulse_step"
  gtk_progress_bar_get_pulse_step :: ((Ptr ProgressBar) -> (IO CDouble))

foreign import ccall unsafe "gtk_progress_bar_get_text"
  gtk_progress_bar_get_text :: ((Ptr ProgressBar) -> (IO (Ptr CChar)))

foreign import ccall safe "gtk_progress_bar_set_orientation"
  gtk_progress_bar_set_orientation :: ((Ptr ProgressBar) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_progress_bar_get_orientation"
  gtk_progress_bar_get_orientation :: ((Ptr ProgressBar) -> (IO CInt))

foreign import ccall safe "gtk_progress_bar_set_ellipsize"
  gtk_progress_bar_set_ellipsize :: ((Ptr ProgressBar) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_progress_bar_get_ellipsize"
  gtk_progress_bar_get_ellipsize :: ((Ptr ProgressBar) -> (IO CInt))
