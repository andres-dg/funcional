
{-# LINE 2 "./Graphics/UI/Gtk/MenuComboToolbar/SeparatorMenuItem.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget SeparatorMenuItem
--
-- Author : Duncan Coutts
--
-- Created: 7 April 2005
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
-- |
-- Maintainer : gtk2hs-users@lists.sourceforge.net
-- Stability : provisional
-- Portability : portable (depends on GHC)
--
-- A separator used in menus
--
module Graphics.UI.Gtk.MenuComboToolbar.SeparatorMenuItem (
-- * Detail
--
-- | The 'SeparatorMenuItem' is a separator used to group items within a menu.
-- It displays a horizontal line with a shadow to make it appear sunken into
-- the interface.

-- * Class Hierarchy
-- |
-- @
-- | 'GObject'
-- | +----'Object'
-- | +----'Widget'
-- | +----'Container'
-- | +----'Bin'
-- | +----'Item'
-- | +----'MenuItem'
-- | +----SeparatorMenuItem
-- @

-- * Types
  SeparatorMenuItem,
  SeparatorMenuItemClass,
  castToSeparatorMenuItem, gTypeSeparatorMenuItem,
  toSeparatorMenuItem,

-- * Constructors
  separatorMenuItemNew,
  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.UI.Gtk.Types
{-# LINE 63 "./Graphics/UI/Gtk/MenuComboToolbar/SeparatorMenuItem.chs" #-}


{-# LINE 65 "./Graphics/UI/Gtk/MenuComboToolbar/SeparatorMenuItem.chs" #-}

--------------------
-- Constructors

-- | Creates a new 'SeparatorMenuItem'.
--
separatorMenuItemNew :: IO SeparatorMenuItem
separatorMenuItemNew =
  makeNewObject mkSeparatorMenuItem $
  liftM (castPtr :: Ptr Widget -> Ptr SeparatorMenuItem) $
  gtk_separator_menu_item_new
{-# LINE 76 "./Graphics/UI/Gtk/MenuComboToolbar/SeparatorMenuItem.chs" #-}

foreign import ccall safe "gtk_separator_menu_item_new"
  gtk_separator_menu_item_new :: (IO (Ptr Widget))
