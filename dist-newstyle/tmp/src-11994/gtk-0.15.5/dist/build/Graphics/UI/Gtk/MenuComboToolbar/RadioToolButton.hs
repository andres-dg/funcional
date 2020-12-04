
{-# LINE 2 "./Graphics/UI/Gtk/MenuComboToolbar/RadioToolButton.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget RadioToolButton
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
-- A toolbar item that contains a radio button
--
-- * Module available since Gtk+ version 2.4
--
module Graphics.UI.Gtk.MenuComboToolbar.RadioToolButton (
-- * Detail
--
-- | A 'RadioToolButton' is a 'ToolItem' that contains a radio button, that
-- is, a button that is part of a group of toggle buttons where only one button
-- can be active at a time.
--
-- Use 'radioToolButtonNew' to create a new 'RadioToolButton'. use
-- 'radioToolButtonNewFromWidget' to create a new 'RadioToolButton' that is
-- part of the same group as an existing 'RadioToolButton'. Use
-- 'radioToolButtonNewFromStock' or 'radioToolButtonNewWithStockFromWidget' to
-- create a new 'RadioToolButton' containing a stock item.

-- * Class Hierarchy
-- |
-- @
-- | 'GObject'
-- | +----'Object'
-- | +----'Widget'
-- | +----'Container'
-- | +----'Bin'
-- | +----'ToolItem'
-- | +----'ToolButton'
-- | +----'ToggleToolButton'
-- | +----RadioToolButton
-- @


-- * Types
  RadioToolButton,
  RadioToolButtonClass,
  castToRadioToolButton, gTypeRadioToolButton,
  toRadioToolButton,

-- * Constructors
  radioToolButtonNew,
  radioToolButtonNewFromStock,
  radioToolButtonNewFromWidget,
  radioToolButtonNewWithStockFromWidget,

-- * Methods
  radioToolButtonGetGroup,
  radioToolButtonSetGroup,

-- * Attributes
  radioToolButtonGroup,

  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.GList
import System.Glib.Attributes
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.UI.Gtk.Types
{-# LINE 87 "./Graphics/UI/Gtk/MenuComboToolbar/RadioToolButton.chs" #-}
import Graphics.UI.Gtk.General.StockItems


{-# LINE 90 "./Graphics/UI/Gtk/MenuComboToolbar/RadioToolButton.chs" #-}


--------------------
-- Constructors

-- | Creates a new 'RadioToolButton', creating a new group.
--
radioToolButtonNew :: IO RadioToolButton
radioToolButtonNew =
  makeNewObject mkRadioToolButton $
  liftM (castPtr :: Ptr ToolItem -> Ptr RadioToolButton) $
  gtk_radio_tool_button_new
{-# LINE 102 "./Graphics/UI/Gtk/MenuComboToolbar/RadioToolButton.chs" #-}
    nullPtr

-- | Creates a new 'RadioToolButton', creating a new group. The new
-- 'RadioToolButton' will contain an icon and label from the stock item
-- indicated by @stockId@.
--
radioToolButtonNewFromStock ::
    StockId -- ^ @stockId@ - the name of a stock item
 -> IO RadioToolButton
radioToolButtonNewFromStock stockId =
  makeNewObject mkRadioToolButton $
  liftM (castPtr :: Ptr ToolItem -> Ptr RadioToolButton) $
  withUTFString stockId $ \stockIdPtr ->
  gtk_radio_tool_button_new_from_stock
{-# LINE 116 "./Graphics/UI/Gtk/MenuComboToolbar/RadioToolButton.chs" #-}
    nullPtr
    stockIdPtr

-- | Creates a new 'RadioToolButton' adding it to the same group as
-- the group to which @groupMember@ belongs.
--
radioToolButtonNewFromWidget :: RadioToolButtonClass groupMember =>
    groupMember -- ^ @groupMember@ - a member of an existing radio group,
                       -- to which the new radio tool button will be added.
 -> IO RadioToolButton
radioToolButtonNewFromWidget group =
  makeNewObject mkRadioToolButton $
  liftM (castPtr :: Ptr ToolItem -> Ptr RadioToolButton) $
  (\(RadioToolButton arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_radio_tool_button_new_from_widget argPtr1)
{-# LINE 130 "./Graphics/UI/Gtk/MenuComboToolbar/RadioToolButton.chs" #-}
    (toRadioToolButton group)

-- | Creates a new 'RadioToolButton' adding it to the same group as the group
-- to which @groupMember@ belongs. The new 'RadioToolButton' will contain an
-- icon and label from the stock item indicated by @stockId@.
--
radioToolButtonNewWithStockFromWidget :: RadioToolButtonClass groupMember =>
    groupMember -- ^ @groupMember@ - a member of an existing radio group,
                       -- to which the new radio tool button will be added.
 -> StockId -- ^ @stockId@ - the name of a stock item
 -> IO RadioToolButton
radioToolButtonNewWithStockFromWidget group stockId =
  makeNewObject mkRadioToolButton $
  liftM (castPtr :: Ptr ToolItem -> Ptr RadioToolButton) $
  withUTFString stockId $ \stockIdPtr ->
  (\(RadioToolButton arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_radio_tool_button_new_with_stock_from_widget argPtr1 arg2)
{-# LINE 146 "./Graphics/UI/Gtk/MenuComboToolbar/RadioToolButton.chs" #-}
    (toRadioToolButton group)
    stockIdPtr

--------------------
-- Methods

-- | Returns the radio button group @button@ belongs to.
--
radioToolButtonGetGroup :: RadioToolButtonClass self => self
 -> IO [RadioToolButton] -- ^ returns the group the button belongs to.
radioToolButtonGetGroup self =
  (\(RadioToolButton arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_radio_tool_button_get_group argPtr1)
{-# LINE 158 "./Graphics/UI/Gtk/MenuComboToolbar/RadioToolButton.chs" #-}
    (toRadioToolButton self)
  >>= readGSList
  >>= mapM (\elemPtr -> makeNewObject mkRadioToolButton (return elemPtr))

-- | Adds @button@ to @group@, removing it from the group it belonged to
-- before.
--
radioToolButtonSetGroup :: RadioToolButtonClass self => self
 -> RadioToolButton -- ^ @groupMember@ - a member of an existing radio group,
                    -- to which the radio tool button will be added.
 -> IO ()
radioToolButtonSetGroup self group =
  (\(RadioToolButton arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_radio_tool_button_get_group argPtr1) group >>= \groupGSList ->
  (\(RadioToolButton arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_radio_tool_button_set_group argPtr1 arg2)
{-# LINE 172 "./Graphics/UI/Gtk/MenuComboToolbar/RadioToolButton.chs" #-}
    (toRadioToolButton self)
    groupGSList

--------------------
-- Properties

-- | Sets a new group for a radio tool button.
--
radioToolButtonGroup :: RadioToolButtonClass self => ReadWriteAttr self [RadioToolButton] RadioToolButton
radioToolButtonGroup = newAttr
  radioToolButtonGetGroup
  radioToolButtonSetGroup

foreign import ccall safe "gtk_radio_tool_button_new"
  gtk_radio_tool_button_new :: ((Ptr ()) -> (IO (Ptr ToolItem)))

foreign import ccall safe "gtk_radio_tool_button_new_from_stock"
  gtk_radio_tool_button_new_from_stock :: ((Ptr ()) -> ((Ptr CChar) -> (IO (Ptr ToolItem))))

foreign import ccall safe "gtk_radio_tool_button_new_from_widget"
  gtk_radio_tool_button_new_from_widget :: ((Ptr RadioToolButton) -> (IO (Ptr ToolItem)))

foreign import ccall safe "gtk_radio_tool_button_new_with_stock_from_widget"
  gtk_radio_tool_button_new_with_stock_from_widget :: ((Ptr RadioToolButton) -> ((Ptr CChar) -> (IO (Ptr ToolItem))))

foreign import ccall unsafe "gtk_radio_tool_button_get_group"
  gtk_radio_tool_button_get_group :: ((Ptr RadioToolButton) -> (IO (Ptr ())))

foreign import ccall safe "gtk_radio_tool_button_set_group"
  gtk_radio_tool_button_set_group :: ((Ptr RadioToolButton) -> ((Ptr ()) -> (IO ())))
