
{-# LINE 2 "./Graphics/UI/Gtk/MenuComboToolbar/MenuToolButton.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget MenuToolButton
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
-- A 'ToolItem' containing a button with an additional dropdown menu
--
-- * Module available since Gtk+ version 2.6
--
module Graphics.UI.Gtk.MenuComboToolbar.MenuToolButton (
-- * Detail
--
-- | A 'MenuToolButton' is a 'ToolItem' that contains a button and a small
-- additional button with an arrow. When clicked, the arrow button pops up a
-- dropdown menu.
--
-- Use 'menuToolButtonNew' to create a new 'MenuToolButton'. Use
-- 'menuToolButtonNewFromStock' to create a new 'MenuToolButton' containing a
-- stock item.

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
-- | +----MenuToolButton
-- @


-- * Types
  MenuToolButton,
  MenuToolButtonClass,
  castToMenuToolButton, gTypeMenuToolButton,
  toMenuToolButton,

-- * Constructors
  menuToolButtonNew,
  menuToolButtonNewFromStock,

-- * Methods
  menuToolButtonSetMenu,
  menuToolButtonGetMenu,

  menuToolButtonSetArrowTooltip,


  menuToolButtonSetArrowTooltipText,
  menuToolButtonSetArrowTooltipMarkup,


-- * Attributes
  menuToolButtonMenu,

-- * Signals
  onShowMenu,
  afterShowMenu,

  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.UI.Gtk.Types
{-# LINE 92 "./Graphics/UI/Gtk/MenuComboToolbar/MenuToolButton.chs" #-}
import Graphics.UI.Gtk.Signals
{-# LINE 93 "./Graphics/UI/Gtk/MenuComboToolbar/MenuToolButton.chs" #-}
import Graphics.UI.Gtk.General.StockItems


{-# LINE 96 "./Graphics/UI/Gtk/MenuComboToolbar/MenuToolButton.chs" #-}


--------------------
-- Constructors

-- | Creates a new 'MenuToolButton' using @iconWidget@ as icon and @label@ as
-- label.
--
menuToolButtonNew :: (WidgetClass iconWidget, GlibString string) =>
    Maybe iconWidget -- ^ @iconWidget@ - a widget that will be used as icon
                      -- widget, or @Nothing@
 -> Maybe string -- ^ @label@ - a string that will be used as label, or
                      -- @Nothing@
 -> IO MenuToolButton
menuToolButtonNew iconWidget label =
  makeNewObject mkMenuToolButton $
  liftM (castPtr :: Ptr ToolItem -> Ptr MenuToolButton) $
  maybeWith withUTFString label $ \labelPtr ->
  (\(Widget arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_menu_tool_button_new argPtr1 arg2)
{-# LINE 115 "./Graphics/UI/Gtk/MenuComboToolbar/MenuToolButton.chs" #-}
    (maybe (Widget nullForeignPtr) toWidget iconWidget)
    labelPtr

-- | Creates a new 'MenuToolButton'. The new 'MenuToolButton' will contain an
-- icon and label from the stock item indicated by @stockId@.
--
menuToolButtonNewFromStock ::
    StockId -- ^ @stockId@ - the name of a stock item
 -> IO MenuToolButton
menuToolButtonNewFromStock stockId =
  makeNewObject mkMenuToolButton $
  liftM (castPtr :: Ptr ToolItem -> Ptr MenuToolButton) $
  withUTFString stockId $ \stockIdPtr ->
  gtk_menu_tool_button_new_from_stock
{-# LINE 129 "./Graphics/UI/Gtk/MenuComboToolbar/MenuToolButton.chs" #-}
    stockIdPtr

--------------------
-- Methods

-- | Sets the 'Menu' that is popped up when the user clicks on the arrow. If
-- @menu@ is @Nothing@, the arrow button becomes insensitive.
--
menuToolButtonSetMenu :: (MenuToolButtonClass self, MenuClass menu) => self
 -> Maybe menu -- ^ @menu@ - the 'Menu' associated with 'MenuToolButton'
 -> IO ()
menuToolButtonSetMenu self menu =
  (\(MenuToolButton arg1) (Widget arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_menu_tool_button_set_menu argPtr1 argPtr2)
{-# LINE 142 "./Graphics/UI/Gtk/MenuComboToolbar/MenuToolButton.chs" #-}
    (toMenuToolButton self)
    (maybe (Widget nullForeignPtr) toWidget menu)

-- | Gets the 'Menu' associated with 'MenuToolButton'.
--
menuToolButtonGetMenu :: MenuToolButtonClass self => self -> IO (Maybe Menu)
menuToolButtonGetMenu self =
  maybeNull (makeNewObject mkMenu) $
  liftM (castPtr :: Ptr Widget -> Ptr Menu) $
  (\(MenuToolButton arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_menu_tool_button_get_menu argPtr1)
{-# LINE 152 "./Graphics/UI/Gtk/MenuComboToolbar/MenuToolButton.chs" #-}
    (toMenuToolButton self)


-- | Sets the 'Tooltips' object to be used for arrow button which pops up the
-- menu. See 'Graphics.UI.Gtk.MenuComboToolbar.ToolItem.toolItemSetTooltip'
-- for setting a tooltip on the whole 'MenuToolButton'.
--
menuToolButtonSetArrowTooltip :: (MenuToolButtonClass self, GlibString string) => self
 -> Tooltips -- ^ @tooltips@ - the 'Tooltips' object to be used
 -> string -- ^ @tipText@ - text to be used as tooltip text for tool item
 -> string -- ^ @tipPrivate@ - text to be used as private tooltip text
 -> IO ()
menuToolButtonSetArrowTooltip self tooltips tipText tipPrivate =
  withUTFString tipPrivate $ \tipPrivatePtr ->
  withUTFString tipText $ \tipTextPtr ->
  (\(MenuToolButton arg1) (Tooltips arg2) arg3 arg4 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_menu_tool_button_set_arrow_tooltip argPtr1 argPtr2 arg3 arg4)
{-# LINE 168 "./Graphics/UI/Gtk/MenuComboToolbar/MenuToolButton.chs" #-}
    (toMenuToolButton self)
    tooltips
    tipTextPtr
    tipPrivatePtr



-- | Sets the tooltip text to be used as tooltip for the arrow button which
-- pops up the menu. See 'toolItemSetTooltip' for setting a tooltip on the
-- whole 'MenuToolButton'.
--
-- * Available since Gtk+ version 2.12
--
menuToolButtonSetArrowTooltipText :: (MenuToolButtonClass self, GlibString string) => self
 -> string -- ^ @text@ - text to be used as tooltip text for button's arrow
           -- button
 -> IO ()
menuToolButtonSetArrowTooltipText self text =
  withUTFString text $ \textPtr ->
  (\(MenuToolButton arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_menu_tool_button_set_arrow_tooltip_text argPtr1 arg2)
{-# LINE 188 "./Graphics/UI/Gtk/MenuComboToolbar/MenuToolButton.chs" #-}
    (toMenuToolButton self)
    textPtr

-- | Sets the tooltip markup text to be used as tooltip for the arrow button
-- which pops up the menu. See 'toolItemSetTooltip' for setting a tooltip on
-- the whole 'MenuToolButton'.
--
-- * Available since Gtk+ version 2.12
--
menuToolButtonSetArrowTooltipMarkup :: (MenuToolButtonClass self, GlibString markup) => self
 -> markup -- ^ @markup@ - markup text to be used as tooltip text for button's
           -- arrow button
 -> IO ()
menuToolButtonSetArrowTooltipMarkup self markup =
  withUTFString markup $ \markupPtr ->
  (\(MenuToolButton arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_menu_tool_button_set_arrow_tooltip_markup argPtr1 arg2)
{-# LINE 204 "./Graphics/UI/Gtk/MenuComboToolbar/MenuToolButton.chs" #-}
    (toMenuToolButton self)
    markupPtr


--------------------
-- Attributes

-- | The dropdown menu.
--
menuToolButtonMenu :: (MenuToolButtonClass self, MenuClass menu) => ReadWriteAttr self (Maybe Menu) (Maybe menu)
menuToolButtonMenu = newAttr
  menuToolButtonGetMenu
  menuToolButtonSetMenu

--------------------
-- Signals

-- |
--
onShowMenu, afterShowMenu :: MenuToolButtonClass self => self
 -> IO ()
 -> IO (ConnectId self)
onShowMenu = connect_NONE__NONE "show-menu" False
afterShowMenu = connect_NONE__NONE "show-menu" True

foreign import ccall safe "gtk_menu_tool_button_new"
  gtk_menu_tool_button_new :: ((Ptr Widget) -> ((Ptr CChar) -> (IO (Ptr ToolItem))))

foreign import ccall safe "gtk_menu_tool_button_new_from_stock"
  gtk_menu_tool_button_new_from_stock :: ((Ptr CChar) -> (IO (Ptr ToolItem)))

foreign import ccall safe "gtk_menu_tool_button_set_menu"
  gtk_menu_tool_button_set_menu :: ((Ptr MenuToolButton) -> ((Ptr Widget) -> (IO ())))

foreign import ccall safe "gtk_menu_tool_button_get_menu"
  gtk_menu_tool_button_get_menu :: ((Ptr MenuToolButton) -> (IO (Ptr Widget)))

foreign import ccall safe "gtk_menu_tool_button_set_arrow_tooltip"
  gtk_menu_tool_button_set_arrow_tooltip :: ((Ptr MenuToolButton) -> ((Ptr Tooltips) -> ((Ptr CChar) -> ((Ptr CChar) -> (IO ())))))

foreign import ccall safe "gtk_menu_tool_button_set_arrow_tooltip_text"
  gtk_menu_tool_button_set_arrow_tooltip_text :: ((Ptr MenuToolButton) -> ((Ptr CChar) -> (IO ())))

foreign import ccall safe "gtk_menu_tool_button_set_arrow_tooltip_markup"
  gtk_menu_tool_button_set_arrow_tooltip_markup :: ((Ptr MenuToolButton) -> ((Ptr CChar) -> (IO ())))
