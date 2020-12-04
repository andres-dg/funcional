
{-# LINE 2 "./Graphics/UI/Gtk/MenuComboToolbar/RadioMenuItem.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget RadioMenuItem
--
-- Author : Axel Simon
--
-- Created: 21 May 2001
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
-- Note: These are not the original Gtk functions as they involve handling a
-- Gtk owned GList. The interface is rather oriented towards the RadioButton
-- widget interface.
--
-- |
-- Maintainer : gtk2hs-users@lists.sourceforge.net
-- Stability : provisional
-- Portability : portable (depends on GHC)
--
-- A choice from multiple check menu items
--
module Graphics.UI.Gtk.MenuComboToolbar.RadioMenuItem (
-- * Detail
--
-- | A radio menu item is a check menu item that belongs to a group. At each
-- instant exactly one of the radio menu items from a group is selected.

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
-- | +----'CheckMenuItem'
-- | +----RadioMenuItem
-- @

-- * Types
  RadioMenuItem,
  RadioMenuItemClass,
  castToRadioMenuItem, gTypeRadioMenuItem,
  toRadioMenuItem,

-- * Constructors
  radioMenuItemNew,
  radioMenuItemNewWithLabel,
  radioMenuItemNewWithMnemonic,
  radioMenuItemNewFromWidget,
  radioMenuItemNewWithLabelFromWidget,
  radioMenuItemNewWithMnemonicFromWidget,

  -- * Compatibilty aliases
  radioMenuItemNewJoinGroup,
  radioMenuItemNewJoinGroupWithLabel,
  radioMenuItemNewJoinGroupWithMnemonic,
  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.UI.Gtk.Types
{-# LINE 78 "./Graphics/UI/Gtk/MenuComboToolbar/RadioMenuItem.chs" #-}


{-# LINE 80 "./Graphics/UI/Gtk/MenuComboToolbar/RadioMenuItem.chs" #-}

--------------------
-- Constructors

-- | Creates a new 'RadioMenuItem'.
--
radioMenuItemNew :: IO RadioMenuItem
radioMenuItemNew =
  makeNewObject mkRadioMenuItem $
  liftM (castPtr :: Ptr Widget -> Ptr RadioMenuItem) $
  gtk_radio_menu_item_new
{-# LINE 91 "./Graphics/UI/Gtk/MenuComboToolbar/RadioMenuItem.chs" #-}
    nullPtr

-- | Creates a new 'RadioMenuItem' whose child is a simple 'Label'.
--
radioMenuItemNewWithLabel :: GlibString string => string -> IO RadioMenuItem
radioMenuItemNewWithLabel label =
  makeNewObject mkRadioMenuItem $
  liftM (castPtr :: Ptr Widget -> Ptr RadioMenuItem) $
  withUTFString label $ \labelPtr ->
  gtk_radio_menu_item_new_with_label
{-# LINE 101 "./Graphics/UI/Gtk/MenuComboToolbar/RadioMenuItem.chs" #-}
    nullPtr
    labelPtr

-- | Creates a new 'RadioMenuItem' containing a label. The label will be
-- created using 'labelNewWithMnemonic', so underscores in @label@ indicate the
-- mnemonic for the menu item.
--
radioMenuItemNewWithMnemonic :: GlibString string => string -> IO RadioMenuItem
radioMenuItemNewWithMnemonic label =
  makeNewObject mkRadioMenuItem $
  liftM (castPtr :: Ptr Widget -> Ptr RadioMenuItem) $
  withUTFString label $ \labelPtr ->
  gtk_radio_menu_item_new_with_mnemonic
{-# LINE 114 "./Graphics/UI/Gtk/MenuComboToolbar/RadioMenuItem.chs" #-}
    nullPtr
    labelPtr

-- | Create a new radio button, adding it to the same group as the group to
-- which @groupMember@ belongs.
--
radioMenuItemNewFromWidget ::
    RadioMenuItem -- ^ @groupMember@ - a member of an existing radio button
                     -- group, to which the new radio button will be added.
 -> IO RadioMenuItem
radioMenuItemNewFromWidget groupMember =
  (\(RadioMenuItem arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_radio_menu_item_get_group argPtr1) groupMember >>= \groupPtr ->
  makeNewObject mkRadioMenuItem $
  liftM (castPtr :: Ptr Widget -> Ptr RadioMenuItem) $
  gtk_radio_menu_item_new
{-# LINE 129 "./Graphics/UI/Gtk/MenuComboToolbar/RadioMenuItem.chs" #-}
    groupPtr

-- | Create a new radio button with a label, adding it to the same group as the
-- group to which @groupMember@ belongs.
--
radioMenuItemNewWithLabelFromWidget :: GlibString string
 => RadioMenuItem -- ^ @groupMember@ - a member of an existing radio button
                     -- group, to which the new radio button will be added.
 -> string
 -> IO RadioMenuItem
radioMenuItemNewWithLabelFromWidget groupMember label =
  (\(RadioMenuItem arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_radio_menu_item_get_group argPtr1) groupMember >>= \groupPtr ->
  withUTFString label $ \strPtr ->
  makeNewObject mkRadioMenuItem $
  liftM (castPtr :: Ptr Widget -> Ptr RadioMenuItem) $
  gtk_radio_menu_item_new_with_label
{-# LINE 145 "./Graphics/UI/Gtk/MenuComboToolbar/RadioMenuItem.chs" #-}
    groupPtr
    strPtr

-- | Create a new radio button with a label and attach it to the group of
-- another radio button. Underscores in the label string indicate the mnemonic
-- for the menu item.
--
radioMenuItemNewWithMnemonicFromWidget :: GlibString string => RadioMenuItem
 -> string
 -> IO RadioMenuItem
radioMenuItemNewWithMnemonicFromWidget groupMember label =
  (\(RadioMenuItem arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_radio_menu_item_get_group argPtr1) groupMember >>= \groupPtr ->
  withUTFString label $ \strPtr ->
  makeNewObject mkRadioMenuItem $
  liftM (castPtr :: Ptr Widget -> Ptr RadioMenuItem) $
  gtk_radio_menu_item_new_with_mnemonic
{-# LINE 161 "./Graphics/UI/Gtk/MenuComboToolbar/RadioMenuItem.chs" #-}
    groupPtr
    strPtr

-- These were added in gtk 2.4, the above Join methods simulate them in earlier
-- versions. These aliases are here for compatibility.

-- | Alias for 'radioMenuItemNewFromWidget'.
radioMenuItemNewJoinGroup = radioMenuItemNewFromWidget

-- | Alias for 'radioMenuItemNewWithLabelFromWidget'.
radioMenuItemNewJoinGroupWithLabel :: GlibString string => RadioMenuItem -> string -> IO RadioMenuItem
radioMenuItemNewJoinGroupWithLabel = radioMenuItemNewWithLabelFromWidget

-- | Alias for 'radioMenuItemNewWithMnemonicFromWidget'.
radioMenuItemNewJoinGroupWithMnemonic :: GlibString string => RadioMenuItem -> string -> IO RadioMenuItem
radioMenuItemNewJoinGroupWithMnemonic = radioMenuItemNewWithMnemonicFromWidget

foreign import ccall unsafe "gtk_radio_menu_item_new"
  gtk_radio_menu_item_new :: ((Ptr ()) -> (IO (Ptr Widget)))

foreign import ccall unsafe "gtk_radio_menu_item_new_with_label"
  gtk_radio_menu_item_new_with_label :: ((Ptr ()) -> ((Ptr CChar) -> (IO (Ptr Widget))))

foreign import ccall unsafe "gtk_radio_menu_item_new_with_mnemonic"
  gtk_radio_menu_item_new_with_mnemonic :: ((Ptr ()) -> ((Ptr CChar) -> (IO (Ptr Widget))))

foreign import ccall unsafe "gtk_radio_menu_item_get_group"
  gtk_radio_menu_item_get_group :: ((Ptr RadioMenuItem) -> (IO (Ptr ())))
