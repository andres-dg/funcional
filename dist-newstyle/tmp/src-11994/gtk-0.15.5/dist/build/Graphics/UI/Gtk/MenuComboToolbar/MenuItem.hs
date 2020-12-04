
{-# LINE 2 "./Graphics/UI/Gtk/MenuComboToolbar/MenuItem.chs" #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LINE 3 "./Graphics/UI/Gtk/MenuComboToolbar/MenuItem.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget MenuItem
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
-- NOTES
--
-- This widget derives from 'Item'. Since CList and CTree are deprecated, it
-- is the only child of that widget. The three signals defined by Item are
-- therefore bound in this module.
--
-- TODO
--
-- figure out what the signals \"toggle-size-allocate\" and
-- \"toggle-size-request\" are good for and bind them if useful
--
-- figure out if the connectToToggle signal is useful at all
--
-- |
-- Maintainer : gtk2hs-users@lists.sourceforge.net
-- Stability : provisional
-- Portability : portable (depends on GHC)
--
-- The widget used for item in menus
--
module Graphics.UI.Gtk.MenuComboToolbar.MenuItem (
-- * Detail
--
-- | The 'MenuItem' widget and the derived widgets are the only valid childs
-- for menus. Their function is to correctly handle highlighting, alignment,
-- events and submenus.
--
-- As it derives from 'Bin' it can hold any valid child widget, altough only
-- a few are really useful.

-- * Class Hierarchy
-- |
-- @
-- | 'GObject'
-- | +----'Object'
-- | +----'Widget'
-- | +----'Container'
-- | +----'Bin'
-- | +----'Item'
-- | +----MenuItem
-- | +----'CheckMenuItem'
-- | +----'ImageMenuItem'
-- | +----'SeparatorMenuItem'
-- | +----'TearoffMenuItem'
-- @

-- * Types
  MenuItem,
  MenuItemClass,
  castToMenuItem, gTypeMenuItem,
  toMenuItem,

-- * Constructors
  menuItemNew,
  menuItemNewWithLabel,
  menuItemNewWithMnemonic,

-- * Methods

  menuItemSetLabel,
  menuItemGetLabel,
  menuItemSetUseUnderline,
  menuItemGetUseUnderline,

  menuItemSetSubmenu,
  menuItemGetSubmenu,
  menuItemRemoveSubmenu,
  menuItemEmitSelect,
  menuItemEmitDeselect,
  menuItemEmitActivate,
  menuItemSetRightJustified,
  menuItemGetRightJustified,
  menuItemSetAccelPath,

-- * Attributes
  menuItemSubmenu,
  menuItemRightJustified,

  menuItemLabel,
  menuItemUseUnderline,


-- * Signals
  menuItemActivatedItem,
  menuItemActivated,
  menuItemActivateItem,
  menuItemActivate,
  menuItemSelect,
  menuItemDeselect,
  menuItemToggle,



-- * Deprecated
  onActivateItem,
  afterActivateItem,
  onActivateLeaf,
  afterActivateLeaf,
  onSelect,
  afterSelect,
  onDeselect,
  afterDeselect,
  onToggle,
  afterToggle


  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.UI.Gtk.Types
{-# LINE 137 "./Graphics/UI/Gtk/MenuComboToolbar/MenuItem.chs" #-}
import Graphics.UI.Gtk.Signals
{-# LINE 138 "./Graphics/UI/Gtk/MenuComboToolbar/MenuItem.chs" #-}


{-# LINE 140 "./Graphics/UI/Gtk/MenuComboToolbar/MenuItem.chs" #-}

--------------------
-- Constructors

-- | Creates a new 'MenuItem'.
--
menuItemNew :: IO MenuItem
menuItemNew =
  makeNewObject mkMenuItem $
  liftM (castPtr :: Ptr Widget -> Ptr MenuItem) $
  gtk_menu_item_new
{-# LINE 151 "./Graphics/UI/Gtk/MenuComboToolbar/MenuItem.chs" #-}

-- | Creates a new 'MenuItem' whose child is a 'Label'.
--
menuItemNewWithLabel :: GlibString string
 => string -- ^ @label@ - the text for the label
 -> IO MenuItem
menuItemNewWithLabel label =
  makeNewObject mkMenuItem $
  liftM (castPtr :: Ptr Widget -> Ptr MenuItem) $
  withUTFString label $ \labelPtr ->
  gtk_menu_item_new_with_label
{-# LINE 162 "./Graphics/UI/Gtk/MenuComboToolbar/MenuItem.chs" #-}
    labelPtr

-- | Creates a new 'MenuItem' containing a label. The label will be created
-- using 'labelNewWithMnemonic', so underscores in @label@ indicate the
-- mnemonic for the menu item.
--
menuItemNewWithMnemonic :: GlibString string
 => string -- ^ @label@ - The text of the label, with an underscore in
                -- front of the mnemonic character
 -> IO MenuItem
menuItemNewWithMnemonic label =
  makeNewObject mkMenuItem $
  liftM (castPtr :: Ptr Widget -> Ptr MenuItem) $
  withUTFString label $ \labelPtr ->
  gtk_menu_item_new_with_mnemonic
{-# LINE 177 "./Graphics/UI/Gtk/MenuComboToolbar/MenuItem.chs" #-}
    labelPtr

--------------------
-- Methods

-- | Sets text on the MenuItem label

menuItemSetLabel :: (MenuItemClass self, GlibString string) => self -> string -> IO ()
menuItemSetLabel self label =
  withUTFString label $ (\(MenuItem arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_menu_item_set_label argPtr1 arg2) (toMenuItem self)

-- | Gets text on the MenuItem label
menuItemGetLabel :: (MenuItemClass self, GlibString string) => self -> IO string
menuItemGetLabel self =
  (\(MenuItem arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_menu_item_get_label argPtr1)
{-# LINE 192 "./Graphics/UI/Gtk/MenuComboToolbar/MenuItem.chs" #-}
    (toMenuItem self)
  >>= \strPtr -> if strPtr == nullPtr
                   then return ""
                   else peekUTFString strPtr

-- | If True, an underline in the text indicates the next character should be used for the mnemonic accelerator key.
--
menuItemSetUseUnderline :: (MenuItemClass self) => self -> Bool -> IO ()
menuItemSetUseUnderline self =
  (\(MenuItem arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_menu_item_set_use_underline argPtr1 arg2) (toMenuItem self) . fromBool

-- | Checks if an underline in the text indicates the next character should be used for the mnemonic accelerator key.
--
menuItemGetUseUnderline :: (MenuItemClass self) => self -> IO Bool
menuItemGetUseUnderline self =
  liftM toBool $ (\(MenuItem arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_menu_item_get_use_underline argPtr1)
{-# LINE 208 "./Graphics/UI/Gtk/MenuComboToolbar/MenuItem.chs" #-}
    (toMenuItem self)


-- | Sets the item's submenu, or changes it.
--
menuItemSetSubmenu :: (MenuItemClass self, MenuClass submenu) => self -> submenu -> IO ()
menuItemSetSubmenu self submenu =
  (\(MenuItem arg1) (Widget arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_menu_item_set_submenu argPtr1 argPtr2)
{-# LINE 216 "./Graphics/UI/Gtk/MenuComboToolbar/MenuItem.chs" #-}
    (toMenuItem self)
    (toWidget submenu)

-- | Gets the submenu underneath this menu item, if any. See
-- 'menuItemSetSubmenu'.
--
menuItemGetSubmenu :: MenuItemClass self => self
 -> IO (Maybe Widget) -- ^ returns submenu for this menu item, or @Nothing@ if
                      -- none.
menuItemGetSubmenu self =
  maybeNull (makeNewObject mkWidget) $
  (\(MenuItem arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_menu_item_get_submenu argPtr1)
{-# LINE 228 "./Graphics/UI/Gtk/MenuComboToolbar/MenuItem.chs" #-}
    (toMenuItem self)

-- | Removes the item's submenu.
--
menuItemRemoveSubmenu :: MenuItemClass self => self -> IO ()
menuItemRemoveSubmenu self =
  (\(MenuItem arg1) (Widget arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_menu_item_set_submenu argPtr1 argPtr2)
{-# LINE 235 "./Graphics/UI/Gtk/MenuComboToolbar/MenuItem.chs" #-}
    (toMenuItem self)
    (Widget $ unsafePerformIO $ newForeignPtr_ nullPtr)

-- | Select the menu item. Emits the \"select\" signal on the item.
--
menuItemEmitSelect :: MenuItemClass self => self -> IO ()
menuItemEmitSelect self =
  (\(MenuItem arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_menu_item_select argPtr1)
{-# LINE 243 "./Graphics/UI/Gtk/MenuComboToolbar/MenuItem.chs" #-}
    (toMenuItem self)

-- | Deselect the menu item. Emits the \"deselect\" signal on the item.
--
menuItemEmitDeselect :: MenuItemClass self => self -> IO ()
menuItemEmitDeselect self =
  (\(MenuItem arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_menu_item_deselect argPtr1)
{-# LINE 250 "./Graphics/UI/Gtk/MenuComboToolbar/MenuItem.chs" #-}
    (toMenuItem self)

-- | Simulate a click on the menu item. Emits the \"activate\" signal on the item.
--
menuItemEmitActivate :: MenuItemClass self => self -> IO ()
menuItemEmitActivate self =
  (\(MenuItem arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_menu_item_activate argPtr1)
{-# LINE 257 "./Graphics/UI/Gtk/MenuComboToolbar/MenuItem.chs" #-}
    (toMenuItem self)

-- | Sets whether the menu item appears justified at the right side of a menu
-- bar. This was traditionally done for \"Help\" menu items, but is now
-- considered a bad idea. (If the widget layout is reversed for a right-to-left
-- language like Hebrew or Arabic, right-justified-menu-items appear at the
-- left.)
--
menuItemSetRightJustified :: MenuItemClass self => self
 -> Bool -- ^ @rightJustified@ - if @True@ the menu item will appear at the
          -- far right if added to a menu bar.
 -> IO ()
menuItemSetRightJustified self rightJustified =
  (\(MenuItem arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_menu_item_set_right_justified argPtr1 arg2)
{-# LINE 271 "./Graphics/UI/Gtk/MenuComboToolbar/MenuItem.chs" #-}
    (toMenuItem self)
    (fromBool rightJustified)

-- | Gets whether the menu item appears justified at the right side of the
-- menu bar.
--
menuItemGetRightJustified :: MenuItemClass self => self -> IO Bool
menuItemGetRightJustified self =
  liftM toBool $
  (\(MenuItem arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_menu_item_get_right_justified argPtr1)
{-# LINE 281 "./Graphics/UI/Gtk/MenuComboToolbar/MenuItem.chs" #-}
    (toMenuItem self)

-- | Set the accelerator path on the menu item, through which runtime changes of
-- the menu item's accelerator caused by the user can be identified and saved
-- to persistant storage (see 'accelMapSave' on this). To setup a default
-- accelerator for this menu item, call 'accelMapAddEntry' with the same accel
-- path. See also 'accelMapAddEntry' on the specifics of accelerator paths, and
-- 'menuSetAccelPath' for a more convenient variant of this function.
--
-- This function is basically a convenience wrapper that handles calling
-- 'widgetSetAccelPath' with the appropriate accelerator group for the menu
-- item.
--
-- Note that you do need to set an accelerator on the parent menu with
-- 'menuSetAccelGroup' for this to work.
--
menuItemSetAccelPath :: (MenuItemClass self, GlibString string) => self
 -> Maybe string -- ^ @accelPath@ - accelerator path, corresponding to this
                 -- menu item's functionality, or @Nothing@ to unset the
                 -- current path.
 -> IO ()
menuItemSetAccelPath self accelPath =
  maybeWith withUTFString accelPath $ \accelPathPtr ->
  (\(MenuItem arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_menu_item_set_accel_path argPtr1 arg2)
{-# LINE 305 "./Graphics/UI/Gtk/MenuComboToolbar/MenuItem.chs" #-}
    (toMenuItem self)
    accelPathPtr

--------------------
-- Attributes

-- | \'submenu\' property. See 'menuItemGetSubmenu' and 'menuItemSetSubmenu'
--
menuItemSubmenu :: (MenuItemClass self, MenuClass submenu) => ReadWriteAttr self (Maybe Widget) submenu
menuItemSubmenu = newAttr
  menuItemGetSubmenu
  menuItemSetSubmenu

-- | \'rightJustified\' property. See 'menuItemGetRightJustified' and
-- 'menuItemSetRightJustified'
--
menuItemRightJustified :: MenuItemClass self => Attr self Bool
menuItemRightJustified = newAttr
  menuItemGetRightJustified
  menuItemSetRightJustified


-- | \'label\' property. See 'menuItemSetLabel' and 'menuItemGetLabel'
--
menuItemLabel :: (MenuItemClass self, GlibString string) => Attr self string
menuItemLabel = newAttr
  menuItemGetLabel
  menuItemSetLabel

-- | \'useUnderline\' property. See 'menuItemSetUseUnderline' and
-- 'menuItemGetUseEUnderline'
--
menuItemUseUnderline :: MenuItemClass self => Attr self Bool
menuItemUseUnderline = newAttr
  menuItemGetUseUnderline
  menuItemSetUseUnderline

--------------------
-- Signals

-- | The user has chosen the menu item.
--
-- * This is the only function applications normally connect to.
-- It is not emitted if the item has a submenu.
--
menuItemActivated :: MenuItemClass self => Signal self (IO ())
menuItemActivated = Signal (connect_NONE__NONE "activate")

-- | Deprecated. See 'menuItemActivated'.
menuItemActivate :: MenuItemClass self => Signal self (IO ())
menuItemActivate = menuItemActivated

-- | Emitted when the user chooses a menu item that has a submenu.
--
-- * This signal is not emitted if the menu item does not have a
-- submenu.
--
menuItemActivatedItem :: MenuItemClass self => Signal self (IO ())
menuItemActivatedItem = Signal (connect_NONE__NONE "activate-item")

-- | Deprecated. See 'menuItemActivatedItem'.
menuItemActivateItem :: MenuItemClass self => Signal self (IO ())
menuItemActivateItem = menuItemActivatedItem

-- | This signal is emitted when the item is selected.
--
menuItemSelect :: MenuItemClass i => Signal i (IO ())
menuItemSelect = Signal (connect_NONE__NONE "select")

-- | This signal is emitted when the item is deselected.
--
menuItemDeselect :: MenuItemClass i => Signal i (IO ())
menuItemDeselect = Signal (connect_NONE__NONE "deselect")

-- | This signal is emitted when the item is toggled.
--
menuItemToggle :: MenuItemClass i => Signal i (IO ())
menuItemToggle = Signal (connect_NONE__NONE "toggle")



--------------------
-- Deprecated Signals

onActivateLeaf, afterActivateLeaf :: MenuItemClass self => self
 -> IO ()
 -> IO (ConnectId self)
onActivateLeaf = connect_NONE__NONE "activate" False
afterActivateLeaf = connect_NONE__NONE "activate" True

onActivateItem, afterActivateItem :: MenuItemClass self => self
 -> IO ()
 -> IO (ConnectId self)
onActivateItem = connect_NONE__NONE "activate-item" False
afterActivateItem = connect_NONE__NONE "activate-item" True

onSelect, afterSelect :: ItemClass i => i
 -> IO ()
 -> IO (ConnectId i)
onSelect = connect_NONE__NONE "select" False
afterSelect = connect_NONE__NONE "select" True

onDeselect, afterDeselect :: ItemClass i => i
 -> IO ()
 -> IO (ConnectId i)
onDeselect = connect_NONE__NONE "deselect" False
afterDeselect = connect_NONE__NONE "deselect" True

onToggle, afterToggle :: ItemClass i => i
 -> IO ()
 -> IO (ConnectId i)
onToggle = connect_NONE__NONE "toggle" False
afterToggle = connect_NONE__NONE "toggle" True

foreign import ccall unsafe "gtk_menu_item_new"
  gtk_menu_item_new :: (IO (Ptr Widget))

foreign import ccall unsafe "gtk_menu_item_new_with_label"
  gtk_menu_item_new_with_label :: ((Ptr CChar) -> (IO (Ptr Widget)))

foreign import ccall unsafe "gtk_menu_item_new_with_mnemonic"
  gtk_menu_item_new_with_mnemonic :: ((Ptr CChar) -> (IO (Ptr Widget)))

foreign import ccall safe "gtk_menu_item_set_label"
  gtk_menu_item_set_label :: ((Ptr MenuItem) -> ((Ptr CChar) -> (IO ())))

foreign import ccall safe "gtk_menu_item_get_label"
  gtk_menu_item_get_label :: ((Ptr MenuItem) -> (IO (Ptr CChar)))

foreign import ccall safe "gtk_menu_item_set_use_underline"
  gtk_menu_item_set_use_underline :: ((Ptr MenuItem) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_menu_item_get_use_underline"
  gtk_menu_item_get_use_underline :: ((Ptr MenuItem) -> (IO CInt))

foreign import ccall safe "gtk_menu_item_set_submenu"
  gtk_menu_item_set_submenu :: ((Ptr MenuItem) -> ((Ptr Widget) -> (IO ())))

foreign import ccall unsafe "gtk_menu_item_get_submenu"
  gtk_menu_item_get_submenu :: ((Ptr MenuItem) -> (IO (Ptr Widget)))

foreign import ccall safe "gtk_menu_item_select"
  gtk_menu_item_select :: ((Ptr MenuItem) -> (IO ()))

foreign import ccall safe "gtk_menu_item_deselect"
  gtk_menu_item_deselect :: ((Ptr MenuItem) -> (IO ()))

foreign import ccall safe "gtk_menu_item_activate"
  gtk_menu_item_activate :: ((Ptr MenuItem) -> (IO ()))

foreign import ccall safe "gtk_menu_item_set_right_justified"
  gtk_menu_item_set_right_justified :: ((Ptr MenuItem) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_menu_item_get_right_justified"
  gtk_menu_item_get_right_justified :: ((Ptr MenuItem) -> (IO CInt))

foreign import ccall safe "gtk_menu_item_set_accel_path"
  gtk_menu_item_set_accel_path :: ((Ptr MenuItem) -> ((Ptr CChar) -> (IO ())))
