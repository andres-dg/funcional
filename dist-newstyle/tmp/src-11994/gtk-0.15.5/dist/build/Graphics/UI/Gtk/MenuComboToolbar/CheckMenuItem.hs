
{-# LINE 2 "./Graphics/UI/Gtk/MenuComboToolbar/CheckMenuItem.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget CheckMenuItem
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
-- A menu item with a check box
--
module Graphics.UI.Gtk.MenuComboToolbar.CheckMenuItem (
-- * Detail
--
-- | A 'CheckMenuItem' is a menu item that maintains the state of a boolean
-- value in addition to a 'MenuItem's usual role in activating application
-- code.
--
-- A check box indicating the state of the boolean value is displayed at the
-- left side of the 'MenuItem'. Activating the 'MenuItem' toggles the value.

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
-- | +----CheckMenuItem
-- | +----'RadioMenuItem'
-- @

-- * Types
  CheckMenuItem,
  CheckMenuItemClass,
  castToCheckMenuItem, gTypeCheckMenuItem,
  toCheckMenuItem,

-- * Constructors
  checkMenuItemNew,
  checkMenuItemNewWithLabel,
  checkMenuItemNewWithMnemonic,

-- * Methods
  checkMenuItemSetActive,
  checkMenuItemGetActive,
  checkMenuItemEmitToggled,
  checkMenuItemSetInconsistent,
  checkMenuItemGetInconsistent,

  checkMenuItemGetDrawAsRadio,
  checkMenuItemSetDrawAsRadio,


-- * Attributes
  checkMenuItemActive,
  checkMenuItemInconsistent,

  checkMenuItemDrawAsRadio,


-- * Signals
  checkMenuItemToggled
  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.UI.Gtk.Signals
{-# LINE 92 "./Graphics/UI/Gtk/MenuComboToolbar/CheckMenuItem.chs" #-}
import Graphics.UI.Gtk.Types
{-# LINE 93 "./Graphics/UI/Gtk/MenuComboToolbar/CheckMenuItem.chs" #-}


{-# LINE 95 "./Graphics/UI/Gtk/MenuComboToolbar/CheckMenuItem.chs" #-}

--------------------
-- Constructors

-- | Creates a new 'CheckMenuItem'.
--
checkMenuItemNew :: IO CheckMenuItem
checkMenuItemNew =
  makeNewObject mkCheckMenuItem $
  liftM (castPtr :: Ptr Widget -> Ptr CheckMenuItem) $
  gtk_check_menu_item_new
{-# LINE 106 "./Graphics/UI/Gtk/MenuComboToolbar/CheckMenuItem.chs" #-}

-- | Creates a new 'CheckMenuItem' with a label.
--
checkMenuItemNewWithLabel :: GlibString string
 => string -- ^ @label@ - the string to use for the label.
 -> IO CheckMenuItem
checkMenuItemNewWithLabel label =
  makeNewObject mkCheckMenuItem $
  liftM (castPtr :: Ptr Widget -> Ptr CheckMenuItem) $
  withUTFString label $ \labelPtr ->
  gtk_check_menu_item_new_with_label
{-# LINE 117 "./Graphics/UI/Gtk/MenuComboToolbar/CheckMenuItem.chs" #-}
    labelPtr

-- | Creates a new 'CheckMenuItem' containing a label. The label will be
-- created using 'Graphics.UI.Gtk.Display.Label.labelNewWithMnemonic', so
-- underscores in @label@ indicate the mnemonic for the menu item.
--
checkMenuItemNewWithMnemonic :: GlibString string
 => string -- ^ @label@ - The text of the button, with an underscore
                     -- in front of the mnemonic character
 -> IO CheckMenuItem
checkMenuItemNewWithMnemonic label =
  makeNewObject mkCheckMenuItem $
  liftM (castPtr :: Ptr Widget -> Ptr CheckMenuItem) $
  withUTFString label $ \labelPtr ->
  gtk_check_menu_item_new_with_mnemonic
{-# LINE 132 "./Graphics/UI/Gtk/MenuComboToolbar/CheckMenuItem.chs" #-}
    labelPtr

--------------------
-- Methods

-- | Sets the active state of the menu item's check box.
--
checkMenuItemSetActive :: CheckMenuItemClass self => self -> Bool -> IO ()
checkMenuItemSetActive self isActive =
  (\(CheckMenuItem arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_check_menu_item_set_active argPtr1 arg2)
{-# LINE 142 "./Graphics/UI/Gtk/MenuComboToolbar/CheckMenuItem.chs" #-}
    (toCheckMenuItem self)
    (fromBool isActive)

-- | Returns whether the check menu item is active. See
-- 'checkMenuItemSetActive'.
--
checkMenuItemGetActive :: CheckMenuItemClass self => self -> IO Bool
checkMenuItemGetActive self =
  liftM toBool $
  (\(CheckMenuItem arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_check_menu_item_get_active argPtr1)
{-# LINE 152 "./Graphics/UI/Gtk/MenuComboToolbar/CheckMenuItem.chs" #-}
    (toCheckMenuItem self)

-- | Emits the toggled signal.
--
checkMenuItemEmitToggled :: CheckMenuItemClass self => self -> IO ()
checkMenuItemEmitToggled self =
  (\(CheckMenuItem arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_check_menu_item_toggled argPtr1)
{-# LINE 159 "./Graphics/UI/Gtk/MenuComboToolbar/CheckMenuItem.chs" #-}
    (toCheckMenuItem self)

-- | If the user has selected a range of elements (such as some text or
-- spreadsheet cells) that are affected by a boolean setting, and the current
-- values in that range are inconsistent, you may want to display the check in
-- an \"in between\" state. This function turns on \"in between\" display.
-- Normally you would turn off the inconsistent state again if the user
-- explicitly selects a setting. This has to be done manually,
-- 'checkMenuItemSetInconsistent' only affects visual appearance, it doesn't
-- affect the semantics of the widget.
--
checkMenuItemSetInconsistent :: CheckMenuItemClass self => self -> Bool -> IO ()
checkMenuItemSetInconsistent self setting =
  (\(CheckMenuItem arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_check_menu_item_set_inconsistent argPtr1 arg2)
{-# LINE 173 "./Graphics/UI/Gtk/MenuComboToolbar/CheckMenuItem.chs" #-}
    (toCheckMenuItem self)
    (fromBool setting)

-- | Query if the menu check is drawn as inconsistent (inbetween). See
-- 'checkMenuItemSetInconsistent'.
--
checkMenuItemGetInconsistent :: CheckMenuItemClass self => self -> IO Bool
checkMenuItemGetInconsistent self =
  liftM toBool $
  (\(CheckMenuItem arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_check_menu_item_get_inconsistent argPtr1)
{-# LINE 183 "./Graphics/UI/Gtk/MenuComboToolbar/CheckMenuItem.chs" #-}
    (toCheckMenuItem self)


-- | Sets whether the menu item is drawn like a 'RadioMenuItem'.
--
-- * Available since Gtk+ version 2.4
--
checkMenuItemSetDrawAsRadio :: CheckMenuItemClass self => self -> Bool -> IO ()
checkMenuItemSetDrawAsRadio self drawAsRadio =
  (\(CheckMenuItem arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_check_menu_item_set_draw_as_radio argPtr1 arg2)
{-# LINE 193 "./Graphics/UI/Gtk/MenuComboToolbar/CheckMenuItem.chs" #-}
    (toCheckMenuItem self)
    (fromBool drawAsRadio)

-- | Returns whether the menu item is drawn like a 'RadioMenuItem'.
--
-- * Available since Gtk+ version 2.4
--
checkMenuItemGetDrawAsRadio :: CheckMenuItemClass self => self -> IO Bool
checkMenuItemGetDrawAsRadio self =
  liftM toBool $
  (\(CheckMenuItem arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_check_menu_item_get_draw_as_radio argPtr1)
{-# LINE 204 "./Graphics/UI/Gtk/MenuComboToolbar/CheckMenuItem.chs" #-}
    (toCheckMenuItem self)


--------------------
-- Attributes

-- | Whether the menu item is checked.
--
-- Default value: @False@
--
checkMenuItemActive :: CheckMenuItemClass self => Attr self Bool
checkMenuItemActive = newAttr
  checkMenuItemGetActive
  checkMenuItemSetActive

-- | Whether to display an \"inconsistent\" state.
--
-- Default value: @False@
--
checkMenuItemInconsistent :: CheckMenuItemClass self => Attr self Bool
checkMenuItemInconsistent = newAttr
  checkMenuItemGetInconsistent
  checkMenuItemSetInconsistent


-- | Whether the menu item looks like a radio menu item.
--
-- Default value: @False@
--
checkMenuItemDrawAsRadio :: CheckMenuItemClass self => Attr self Bool
checkMenuItemDrawAsRadio = newAttr
  checkMenuItemGetDrawAsRadio
  checkMenuItemSetDrawAsRadio


-- | This signal is emitted when the state of the check box is changed.
--
checkMenuItemToggled :: CheckMenuItemClass self => Signal self (IO ())
checkMenuItemToggled = Signal (connect_NONE__NONE "toggled")

foreign import ccall unsafe "gtk_check_menu_item_new"
  gtk_check_menu_item_new :: (IO (Ptr Widget))

foreign import ccall unsafe "gtk_check_menu_item_new_with_label"
  gtk_check_menu_item_new_with_label :: ((Ptr CChar) -> (IO (Ptr Widget)))

foreign import ccall unsafe "gtk_check_menu_item_new_with_mnemonic"
  gtk_check_menu_item_new_with_mnemonic :: ((Ptr CChar) -> (IO (Ptr Widget)))

foreign import ccall safe "gtk_check_menu_item_set_active"
  gtk_check_menu_item_set_active :: ((Ptr CheckMenuItem) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_check_menu_item_get_active"
  gtk_check_menu_item_get_active :: ((Ptr CheckMenuItem) -> (IO CInt))

foreign import ccall safe "gtk_check_menu_item_toggled"
  gtk_check_menu_item_toggled :: ((Ptr CheckMenuItem) -> (IO ()))

foreign import ccall safe "gtk_check_menu_item_set_inconsistent"
  gtk_check_menu_item_set_inconsistent :: ((Ptr CheckMenuItem) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_check_menu_item_get_inconsistent"
  gtk_check_menu_item_get_inconsistent :: ((Ptr CheckMenuItem) -> (IO CInt))

foreign import ccall safe "gtk_check_menu_item_set_draw_as_radio"
  gtk_check_menu_item_set_draw_as_radio :: ((Ptr CheckMenuItem) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_check_menu_item_get_draw_as_radio"
  gtk_check_menu_item_get_draw_as_radio :: ((Ptr CheckMenuItem) -> (IO CInt))
