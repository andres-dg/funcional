
{-# LINE 2 "./Graphics/UI/Gtk/Display/Spinner.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget Spinner
--
-- Author : Andy Stewart
--
-- Created: 17 Aug 2010
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
-- Report messages of minor importance to the user
--
module Graphics.UI.Gtk.Display.Spinner (

-- * Detail
--
-- | A 'Spinner' widget displays an icon-size spinning animation. It is often used as an alternative to
-- a 'ProgressBar' for displaying indefinite activity, instead of actual progress.
--
-- To start the animation, use 'spinnerStart'.

-- * Types
   Spinner,
   SpinnerClass,
   castToSpinner, gTypeSpinner,
   toSpinner,

-- * Constructors
   spinnerNew,

-- * Methods
   spinnerStart,
   spinnerStop,

-- * Attributes
   spinnerActive,

) where

import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.UI.Gtk.Types
{-# LINE 62 "./Graphics/UI/Gtk/Display/Spinner.chs" #-}


{-# LINE 64 "./Graphics/UI/Gtk/Display/Spinner.chs" #-}


--------------------
-- Constructors

-- | Returns a new spinner widget. Not yet started.
spinnerNew :: IO Spinner
spinnerNew =
  makeNewObject mkSpinner $
  liftM (castPtr :: Ptr Widget -> Ptr Spinner) $
  gtk_spinner_new
{-# LINE 75 "./Graphics/UI/Gtk/Display/Spinner.chs" #-}

--------------------
-- Methods

-- | Starts the animation of the spinner.
spinnerStart :: SpinnerClass spinner => spinner -> IO ()
spinnerStart spinner =
  (\(Spinner arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_spinner_start argPtr1)
{-# LINE 83 "./Graphics/UI/Gtk/Display/Spinner.chs" #-}
    (toSpinner spinner)

-- | Stops the animation of the spinner.
spinnerStop :: SpinnerClass spinner => spinner -> IO ()
spinnerStop spinner =
  (\(Spinner arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_spinner_stop argPtr1)
{-# LINE 89 "./Graphics/UI/Gtk/Display/Spinner.chs" #-}
    (toSpinner spinner)

--------------------
-- Attributes

-- | Whether the spinner is active.
--
-- Default value: 'False'
spinnerActive :: SpinnerClass spinner => Attr spinner Bool
spinnerActive = newAttrFromBoolProperty "active"

foreign import ccall unsafe "gtk_spinner_new"
  gtk_spinner_new :: (IO (Ptr Widget))

foreign import ccall safe "gtk_spinner_start"
  gtk_spinner_start :: ((Ptr Spinner) -> (IO ()))

foreign import ccall safe "gtk_spinner_stop"
  gtk_spinner_stop :: ((Ptr Spinner) -> (IO ()))
