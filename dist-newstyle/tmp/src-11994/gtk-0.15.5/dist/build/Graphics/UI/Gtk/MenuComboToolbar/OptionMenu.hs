
{-# LINE 2 "./Graphics/UI/Gtk/MenuComboToolbar/OptionMenu.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget OptionMenu
--
-- Author : Axel Simon
--
-- Created: 15 May 2001
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
-- A widget used to choose from a list of valid choices
--
-- * Warning: this module is deprecated and should not be used in
-- newly-written code. It is empty in Gtk3.
--
module Graphics.UI.Gtk.MenuComboToolbar.OptionMenu (
-- * Detail
--
-- | A 'OptionMenu' is a widget that allows the user to choose from a list of
-- valid choices. The 'OptionMenu' displays the selected choice. When activated
-- the 'OptionMenu' displays a popup 'Menu' which allows the user to make a new
-- choice.
--
-- Using a 'OptionMenu' is simple; build a 'Menu', by calling
-- 'Graphics.UI.Gtk.MenuComboToolbar.Menu.menuNew', then appending menu items
-- to it with 'Graphics.UI.Gtk.MenuComboToolbar.MenuShell.menuShellAppend'.
-- Set that menu on the option menu with 'optionMenuSetMenu'. Set the selected
-- menu item with 'optionMenuSetHistory'; connect to the \"changed\" signal on
-- the option menu; in the \"changed\" signal, check the new selected menu
-- item with 'optionMenuGetHistory'.
--
-- As of Gtk+ 2.4, 'OptionMenu' has been deprecated in favor of 'ComboBox'.

-- * Class Hierarchy
-- |
-- @
-- | 'GObject'
-- | +----'Object'
-- | +----'Widget'
-- | +----'Container'
-- | +----'Bin'
-- | +----'Button'
-- | +----OptionMenu
-- @



-- * Types
  OptionMenu,
  OptionMenuClass,
  castToOptionMenu, gTypeOptionMenu,
  toOptionMenu,

-- * Constructors
  optionMenuNew,

-- * Methods
  optionMenuGetMenu,
  optionMenuSetMenu,
  optionMenuRemoveMenu,
  optionMenuSetHistory,
  optionMenuGetHistory,

-- * Attributes
  optionMenuMenu,

-- * Signals
  onOMChanged,
  afterOMChanged


  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.UI.Gtk.Types
{-# LINE 96 "./Graphics/UI/Gtk/MenuComboToolbar/OptionMenu.chs" #-}
import Graphics.UI.Gtk.Signals
{-# LINE 97 "./Graphics/UI/Gtk/MenuComboToolbar/OptionMenu.chs" #-}


{-# LINE 99 "./Graphics/UI/Gtk/MenuComboToolbar/OptionMenu.chs" #-}



--------------------
-- Constructors

-- | Create a new option menu.
--
optionMenuNew :: IO OptionMenu
optionMenuNew =
  makeNewObject mkOptionMenu $
  liftM castPtr
  gtk_option_menu_new
{-# LINE 112 "./Graphics/UI/Gtk/MenuComboToolbar/OptionMenu.chs" #-}

--------------------
-- Methods

-- | Returns the 'Menu' associated with the 'OptionMenu'.
--
optionMenuGetMenu :: OptionMenuClass self => self -> IO Menu
optionMenuGetMenu self =
  makeNewObject mkMenu $
  liftM castPtr $
  throwIfNull "optionMenuGetMenu: no menu associated with this option menu." $
  (\(OptionMenu arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_option_menu_get_menu argPtr1)
{-# LINE 124 "./Graphics/UI/Gtk/MenuComboToolbar/OptionMenu.chs" #-}
    (toOptionMenu self)

-- | Provides the 'Menu' that is popped up to allow the user to choose a new
-- value. You should provide a simple menu avoiding the use of tearoff menu
-- items, submenus, and accelerators.
--
optionMenuSetMenu :: (OptionMenuClass self, MenuClass menu) => self -> menu -> IO ()
optionMenuSetMenu self menu =
  (\(OptionMenu arg1) (Widget arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_option_menu_set_menu argPtr1 argPtr2)
{-# LINE 133 "./Graphics/UI/Gtk/MenuComboToolbar/OptionMenu.chs" #-}
    (toOptionMenu self)
    (toWidget menu)

-- | Removes the menu from the option menu.
--
optionMenuRemoveMenu :: OptionMenuClass self => self -> IO ()
optionMenuRemoveMenu self =
  (\(OptionMenu arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_option_menu_remove_menu argPtr1)
{-# LINE 141 "./Graphics/UI/Gtk/MenuComboToolbar/OptionMenu.chs" #-}
    (toOptionMenu self)

-- | Selects the menu item specified by @index@ making it the newly selected
-- value for the option menu.
--
optionMenuSetHistory :: OptionMenuClass self => self
 -> Int -- ^ @index@ - the index of the menu item to select. Index values
          -- are from 0 to n-1.
 -> IO ()
optionMenuSetHistory self index =
  (\(OptionMenu arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_option_menu_set_history argPtr1 arg2)
{-# LINE 152 "./Graphics/UI/Gtk/MenuComboToolbar/OptionMenu.chs" #-}
    (toOptionMenu self)
    (fromIntegral index)

-- | Retrieves the index of the currently selected menu item. The menu items
-- are numbered from top to bottom, starting with 0.
--
optionMenuGetHistory :: OptionMenuClass self => self
 -> IO Int -- ^ returns index of the selected menu item, or -1 if there are no
           -- menu items
optionMenuGetHistory self =
  liftM fromIntegral $
  (\(OptionMenu arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_option_menu_get_history argPtr1)
{-# LINE 164 "./Graphics/UI/Gtk/MenuComboToolbar/OptionMenu.chs" #-}
    (toOptionMenu self)

--------------------
-- Attributes

-- | The menu of options.
--
optionMenuMenu :: (OptionMenuClass self, MenuClass menu) => ReadWriteAttr self Menu menu
optionMenuMenu = newAttrFromObjectProperty "menu"
  gtk_menu_get_type
{-# LINE 174 "./Graphics/UI/Gtk/MenuComboToolbar/OptionMenu.chs" #-}

--------------------
-- Signals

-- | This signal is called if the selected option has changed.
--
onOMChanged, afterOMChanged :: OptionMenuClass self => self
 -> IO ()
 -> IO (ConnectId self)
onOMChanged = connect_NONE__NONE "changed" False
afterOMChanged = connect_NONE__NONE "changed" True

foreign import ccall unsafe "gtk_option_menu_new"
  gtk_option_menu_new :: (IO (Ptr Widget))

foreign import ccall unsafe "gtk_option_menu_get_menu"
  gtk_option_menu_get_menu :: ((Ptr OptionMenu) -> (IO (Ptr Widget)))

foreign import ccall safe "gtk_option_menu_set_menu"
  gtk_option_menu_set_menu :: ((Ptr OptionMenu) -> ((Ptr Widget) -> (IO ())))

foreign import ccall unsafe "gtk_option_menu_remove_menu"
  gtk_option_menu_remove_menu :: ((Ptr OptionMenu) -> (IO ()))

foreign import ccall safe "gtk_option_menu_set_history"
  gtk_option_menu_set_history :: ((Ptr OptionMenu) -> (CUInt -> (IO ())))

foreign import ccall unsafe "gtk_option_menu_get_history"
  gtk_option_menu_get_history :: ((Ptr OptionMenu) -> (IO CInt))

foreign import ccall unsafe "gtk_menu_get_type"
  gtk_menu_get_type :: CULong
