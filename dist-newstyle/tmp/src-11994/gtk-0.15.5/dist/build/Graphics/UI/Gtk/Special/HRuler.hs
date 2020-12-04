
{-# LINE 2 "./Graphics/UI/Gtk/Special/HRuler.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget HRuler
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
-- A horizontal ruler
--
-- * Rulers are removed in Gtk3 and thus this module is blank. There is no
-- replacement
module Graphics.UI.Gtk.Special.HRuler (

-- * Detail
--
-- | The 'HRuler' widget is a widget arranged horizontally creating a ruler that
-- is utilized around other widgets such as a text widget. The ruler is used to
-- show the location of the mouse on the window and to show the size of the
-- window in specified units. The available units of measurement are 'Pixels',
-- 'Inches' and 'Centimeters'. 'Pixels' is the default. rulers.

-- * Class Hierarchy
--
-- |
-- @
-- | 'GObject'
-- | +----'Object'
-- | +----'Widget'
-- | +----'Ruler'
-- | +----HRuler
-- @

-- * Types
  HRuler,
  HRulerClass,
  castToHRuler,
  toHRuler,

-- * Constructors
  hRulerNew,

  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.UI.Gtk.Types
{-# LINE 67 "./Graphics/UI/Gtk/Special/HRuler.chs" #-}


{-# LINE 69 "./Graphics/UI/Gtk/Special/HRuler.chs" #-}

--------------------
-- Constructors

-- | Creates a new horizontal ruler.
--
hRulerNew :: IO HRuler
hRulerNew =
  makeNewObject mkHRuler $
  liftM (castPtr :: Ptr Widget -> Ptr HRuler) $
  gtk_hruler_new
{-# LINE 80 "./Graphics/UI/Gtk/Special/HRuler.chs" #-}

foreign import ccall safe "gtk_hruler_new"
  gtk_hruler_new :: (IO (Ptr Widget))
