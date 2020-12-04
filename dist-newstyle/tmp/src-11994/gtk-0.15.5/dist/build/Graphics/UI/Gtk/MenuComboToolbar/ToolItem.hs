
{-# LINE 2 "./Graphics/UI/Gtk/MenuComboToolbar/ToolItem.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget ToolItem
--
-- Author : Duncan Coutts
--
-- Created: 1 August 2004
--
-- Copyright (C) 2004-2005 Duncan Coutts
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
-- The base class of widgets that can be added to 'Toolbar'
--
-- * Module available since Gtk+ version 2.4
--
module Graphics.UI.Gtk.MenuComboToolbar.ToolItem (
-- * Detail
--
-- | 'ToolItem's are widgets that can appear on a toolbar. To create a toolbar
-- item that contain something else than a button, use 'toolItemNew'. Use
-- 'containerAdd' to add a child widget to the tool item.
--
-- For toolbar items that contain buttons, see the 'ToolButton',
-- 'ToggleToolButton' and 'RadioToolButton' classes.

-- * Class Hierarchy
-- |
-- @
-- | 'GObject'
-- | +----'Object'
-- | +----'Widget'
-- | +----'Container'
-- | +----'Bin'
-- | +----ToolItem
-- | +----'ToolButton'
-- | +----'SeparatorToolItem'
-- @


-- * Types
  ToolItem,
  ToolItemClass,
  castToToolItem, gTypeToolItem,
  toToolItem,

-- * Constructors
  toolItemNew,

-- * Methods
  toolItemSetHomogeneous,
  toolItemGetHomogeneous,
  toolItemSetExpand,
  toolItemGetExpand,

  toolItemSetTooltip,

  toolItemSetUseDragWindow,
  toolItemGetUseDragWindow,
  toolItemSetVisibleHorizontal,
  toolItemGetVisibleHorizontal,
  toolItemSetVisibleVertical,
  toolItemGetVisibleVertical,
  toolItemSetIsImportant,
  toolItemGetIsImportant,
  IconSize,
  toolItemGetIconSize,
  Orientation(..),
  toolItemGetOrientation,
  ToolbarStyle(..),
  toolItemGetToolbarStyle,
  ReliefStyle(..),
  toolItemGetReliefStyle,
  toolItemRetrieveProxyMenuItem,
  toolItemGetProxyMenuItem,
  toolItemSetProxyMenuItem,


  toolItemGetEllipsizeMode,
  toolItemGetTextAlignment,
  toolItemGetTextOrientation,
  toolItemGetTextSizeGroup,


-- * Attributes
  toolItemVisibleHorizontal,
  toolItemVisibleVertical,
  toolItemIsImportant,
  toolItemExpand,
  toolItemHomogeneous,
  toolItemUseDragWindow,

  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.Rendering.Pango.Enums (EllipsizeMode (..))
import Graphics.UI.Gtk.Misc.SizeGroup
import Graphics.UI.Gtk.Types
{-# LINE 117 "./Graphics/UI/Gtk/MenuComboToolbar/ToolItem.chs" #-}
import Graphics.UI.Gtk.General.Structs (IconSize)
import Graphics.UI.Gtk.General.Enums (Orientation(..), ToolbarStyle(..), ReliefStyle(..))


{-# LINE 121 "./Graphics/UI/Gtk/MenuComboToolbar/ToolItem.chs" #-}


--------------------
-- Constructors

-- | Creates a new 'ToolItem'
--
toolItemNew :: IO ToolItem
toolItemNew =
  makeNewObject mkToolItem $
  gtk_tool_item_new
{-# LINE 132 "./Graphics/UI/Gtk/MenuComboToolbar/ToolItem.chs" #-}

--------------------
-- Methods

-- | Sets whether the tool item is to be allocated the same size as other
-- homogeneous items. The effect is that all homogeneous items will have the
-- same width as the widest of the items.
--
toolItemSetHomogeneous :: ToolItemClass self => self
 -> Bool -- ^ @homogeneous@ - whether @toolItem@ is the same size as other
          -- homogeneous items
 -> IO ()
toolItemSetHomogeneous self homogeneous =
  (\(ToolItem arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tool_item_set_homogeneous argPtr1 arg2)
{-# LINE 146 "./Graphics/UI/Gtk/MenuComboToolbar/ToolItem.chs" #-}
    (toToolItem self)
    (fromBool homogeneous)

-- | Returns whether the tool item is the same size as other homogeneous items.
-- See 'toolItemSetHomogeneous'.
--
toolItemGetHomogeneous :: ToolItemClass self => self -> IO Bool
toolItemGetHomogeneous self =
  liftM toBool $
  (\(ToolItem arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tool_item_get_homogeneous argPtr1)
{-# LINE 156 "./Graphics/UI/Gtk/MenuComboToolbar/ToolItem.chs" #-}
    (toToolItem self)

-- | Sets whether the tool item is allocated extra space when there is more room
-- on the toolbar then needed for the items. The effect is that the item gets
-- bigger when the toolbar gets bigger and smaller when the toolbar gets
-- smaller.
--
toolItemSetExpand :: ToolItemClass self => self -> Bool -> IO ()
toolItemSetExpand self expand =
  (\(ToolItem arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tool_item_set_expand argPtr1 arg2)
{-# LINE 166 "./Graphics/UI/Gtk/MenuComboToolbar/ToolItem.chs" #-}
    (toToolItem self)
    (fromBool expand)

-- | Returns whether the tool item is allocated extra space. See
-- 'toolItemSetExpand'.
--
toolItemGetExpand :: ToolItemClass self => self -> IO Bool
toolItemGetExpand self =
  liftM toBool $
  (\(ToolItem arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tool_item_get_expand argPtr1)
{-# LINE 176 "./Graphics/UI/Gtk/MenuComboToolbar/ToolItem.chs" #-}
    (toToolItem self)


-- | Sets the 'Tooltips' object to be used for the tool item, the text to be
-- displayed as tooltip on the item and the private text to be used. See
-- 'tooltipsSetTip'.
--
-- Removed in Gtk3.
toolItemSetTooltip :: (ToolItemClass self, GlibString string) => self
 -> Tooltips -- ^ @tooltips@ - The 'Tooltips' object to be used
 -> string -- ^ @tipText@ - text to be used as tooltip text for @toolItem@
 -> string -- ^ @tipPrivate@ - text to be used as private tooltip text
 -> IO ()
toolItemSetTooltip self tooltips tipText tipPrivate =
  withUTFString tipPrivate $ \tipPrivatePtr ->
  withUTFString tipText $ \tipTextPtr ->
  (\(ToolItem arg1) (Tooltips arg2) arg3 arg4 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_tool_item_set_tooltip argPtr1 argPtr2 arg3 arg4)
{-# LINE 193 "./Graphics/UI/Gtk/MenuComboToolbar/ToolItem.chs" #-}
    (toToolItem self)
    tooltips
    tipTextPtr
    tipPrivatePtr


-- | Sets whether toolitem has a drag window. When @True@ the tool item can be
-- used as a drag source through 'dragSourceSet'. When the tool item has a drag
-- window it will intercept all events, even those that would otherwise be sent
-- to a child widget.
--
toolItemSetUseDragWindow :: ToolItemClass self => self -> Bool -> IO ()
toolItemSetUseDragWindow self useDragWindow =
  (\(ToolItem arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tool_item_set_use_drag_window argPtr1 arg2)
{-# LINE 207 "./Graphics/UI/Gtk/MenuComboToolbar/ToolItem.chs" #-}
    (toToolItem self)
    (fromBool useDragWindow)

-- | Returns whether the tool item has a drag window. See
-- 'toolItemSetUseDragWindow'.
--
toolItemGetUseDragWindow :: ToolItemClass self => self -> IO Bool
toolItemGetUseDragWindow self =
  liftM toBool $
  (\(ToolItem arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tool_item_get_use_drag_window argPtr1)
{-# LINE 217 "./Graphics/UI/Gtk/MenuComboToolbar/ToolItem.chs" #-}
    (toToolItem self)

-- | Sets whether the tool item is visible when the toolbar is docked
-- horizontally.
--
toolItemSetVisibleHorizontal :: ToolItemClass self => self -> Bool -> IO ()
toolItemSetVisibleHorizontal self visibleHorizontal =
  (\(ToolItem arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tool_item_set_visible_horizontal argPtr1 arg2)
{-# LINE 225 "./Graphics/UI/Gtk/MenuComboToolbar/ToolItem.chs" #-}
    (toToolItem self)
    (fromBool visibleHorizontal)

-- | Returns whether the tool item is visible on toolbars that are docked
-- horizontally.
--
toolItemGetVisibleHorizontal :: ToolItemClass self => self -> IO Bool
toolItemGetVisibleHorizontal self =
  liftM toBool $
  (\(ToolItem arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tool_item_get_visible_horizontal argPtr1)
{-# LINE 235 "./Graphics/UI/Gtk/MenuComboToolbar/ToolItem.chs" #-}
    (toToolItem self)

-- | Sets whether the tool item is visible when the toolbar is docked vertically.
-- Some tool items, such as text entries, are too wide to be useful on a
-- vertically docked toolbar. If @False@ the tool item will
-- not appear on toolbars that are docked vertically.
--
toolItemSetVisibleVertical :: ToolItemClass self => self -> Bool -> IO ()
toolItemSetVisibleVertical self visibleVertical =
  (\(ToolItem arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tool_item_set_visible_vertical argPtr1 arg2)
{-# LINE 245 "./Graphics/UI/Gtk/MenuComboToolbar/ToolItem.chs" #-}
    (toToolItem self)
    (fromBool visibleVertical)

-- | Returns whether the tool item is visible when the toolbar is docked
-- vertically. See 'toolItemSetVisibleVertical'.
--
toolItemGetVisibleVertical :: ToolItemClass self => self -> IO Bool
toolItemGetVisibleVertical self =
  liftM toBool $
  (\(ToolItem arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tool_item_get_visible_vertical argPtr1)
{-# LINE 255 "./Graphics/UI/Gtk/MenuComboToolbar/ToolItem.chs" #-}
    (toToolItem self)

-- | Sets whether the tool item should be considered important. The "ToolButton"
-- class uses this property to determine whether to show or hide its label when
-- the toolbar style is 'ToolbarBothHoriz'. The result is that only tool
-- buttons with the \"is important\" property set have labels, an effect known
-- as \"priority text\".
--
toolItemSetIsImportant :: ToolItemClass self => self -> Bool -> IO ()
toolItemSetIsImportant self isImportant =
  (\(ToolItem arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tool_item_set_is_important argPtr1 arg2)
{-# LINE 266 "./Graphics/UI/Gtk/MenuComboToolbar/ToolItem.chs" #-}
    (toToolItem self)
    (fromBool isImportant)

-- | Returns whether the tool item is considered important. See
-- 'toolItemSetIsImportant'
--
toolItemGetIsImportant :: ToolItemClass self => self -> IO Bool
toolItemGetIsImportant self =
  liftM toBool $
  (\(ToolItem arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tool_item_get_is_important argPtr1)
{-# LINE 276 "./Graphics/UI/Gtk/MenuComboToolbar/ToolItem.chs" #-}
    (toToolItem self)

-- | Returns the icon size used for the tool item.
--
toolItemGetIconSize :: ToolItemClass self => self -> IO IconSize
toolItemGetIconSize self =
  liftM (toEnum . fromIntegral) $
  (\(ToolItem arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tool_item_get_icon_size argPtr1)
{-# LINE 284 "./Graphics/UI/Gtk/MenuComboToolbar/ToolItem.chs" #-}
    (toToolItem self)

-- | Returns the orientation used for the tool item.
--
toolItemGetOrientation :: ToolItemClass self => self -> IO Orientation
toolItemGetOrientation self =
  liftM (toEnum . fromIntegral) $
  (\(ToolItem arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tool_item_get_orientation argPtr1)
{-# LINE 292 "./Graphics/UI/Gtk/MenuComboToolbar/ToolItem.chs" #-}
    (toToolItem self)

-- | Returns the toolbar style used for the tool item.
--
-- Possibilities are:
-- ['ToolbarBoth'] meaning the tool item should show both an icon and a label,
-- stacked vertically
-- ['ToolbarIcons'] meaning the toolbar shows only icons
-- ['ToolbarText'] meaning the tool item should only show text
-- ['ToolbarBothHoriz'] meaning the tool item should show both an icon and a
-- label, arranged horizontally
--
toolItemGetToolbarStyle :: ToolItemClass self => self -> IO ToolbarStyle
toolItemGetToolbarStyle self =
  liftM (toEnum . fromIntegral) $
  (\(ToolItem arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tool_item_get_toolbar_style argPtr1)
{-# LINE 308 "./Graphics/UI/Gtk/MenuComboToolbar/ToolItem.chs" #-}
    (toToolItem self)

-- | Returns the relief style of the tool item. See 'buttonSetReliefStyle'.
--
toolItemGetReliefStyle :: ToolItemClass self => self -> IO ReliefStyle
toolItemGetReliefStyle self =
  liftM (toEnum . fromIntegral) $
  (\(ToolItem arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tool_item_get_relief_style argPtr1)
{-# LINE 316 "./Graphics/UI/Gtk/MenuComboToolbar/ToolItem.chs" #-}
    (toToolItem self)

-- | Returns the 'MenuItem' that was last set by 'toolItemSetProxyMenuItem',
-- ie. the 'MenuItem' that is going to appear in the overflow menu.
--
toolItemRetrieveProxyMenuItem :: ToolItemClass self => self -> IO (Maybe Widget)
toolItemRetrieveProxyMenuItem self =
  maybeNull (makeNewObject mkWidget) $
  (\(ToolItem arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tool_item_retrieve_proxy_menu_item argPtr1)
{-# LINE 325 "./Graphics/UI/Gtk/MenuComboToolbar/ToolItem.chs" #-}
    (toToolItem self)

-- | If @menuItemId@ matches the string passed to 'toolItemSetProxyMenuItem'
-- return the corresponding 'MenuItem'.
--
toolItemGetProxyMenuItem :: (ToolItemClass self, GlibString string) => self
 -> string -- ^ @menuItemId@ - a string used to identify the menu
                      -- item
 -> IO (Maybe Widget) -- ^ returns The 'MenuItem' passed to
                      -- 'toolItemSetProxyMenuItem', if the @menuItemId@s
                      -- match.
toolItemGetProxyMenuItem self menuItemId =
  maybeNull (makeNewObject mkWidget) $
  withUTFString menuItemId $ \menuItemIdPtr ->
  (\(ToolItem arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tool_item_get_proxy_menu_item argPtr1 arg2)
{-# LINE 340 "./Graphics/UI/Gtk/MenuComboToolbar/ToolItem.chs" #-}
    (toToolItem self)
    menuItemIdPtr

-- | Sets the 'MenuItem' used in the toolbar overflow menu. The @menuItemId@
-- is used to identify the caller of this function and should also be used with
-- 'toolItemGetProxyMenuItem'.
--
toolItemSetProxyMenuItem :: (ToolItemClass self, MenuItemClass menuItem, GlibString string) => self
 -> string -- ^ @menuItemId@ - a string used to identify @menuItem@
 -> menuItem -- ^ @menuItem@ - a 'MenuItem' to be used in the overflow menu
 -> IO ()
toolItemSetProxyMenuItem self menuItemId menuItem =
  withUTFString menuItemId $ \menuItemIdPtr ->
  (\(ToolItem arg1) arg2 (Widget arg3) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg3 $ \argPtr3 ->gtk_tool_item_set_proxy_menu_item argPtr1 arg2 argPtr3)
{-# LINE 354 "./Graphics/UI/Gtk/MenuComboToolbar/ToolItem.chs" #-}
    (toToolItem self)
    menuItemIdPtr
    (toWidget menuItem)


-- | Returns the ellipsize mode used for @toolItem@. Custom subclasses of 'ToolItem' should call this
-- function to find out how text should be ellipsized.
--
-- * Available since Gtk+ version 2.20
--
toolItemGetEllipsizeMode :: ToolItemClass item => item
                         -> IO EllipsizeMode -- ^ returns a PangoEllipsizeMode indicating how text in @toolItem@ should be ellipsized.
toolItemGetEllipsizeMode item =
  liftM (toEnum . fromIntegral) $
  (\(ToolItem arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tool_item_get_ellipsize_mode argPtr1)
{-# LINE 369 "./Graphics/UI/Gtk/MenuComboToolbar/ToolItem.chs" #-}
    (toToolItem item)

-- | Returns the text alignment used for @toolItem@. Custom subclasses of 'ToolItem' should call this
-- function to find out how text should be aligned.
toolItemGetTextAlignment :: ToolItemClass item => item
                         -> IO Double -- ^ returns a gfloat indicating the horizontal text alignment used for @toolItem@
toolItemGetTextAlignment item =
  liftM realToFrac $
  (\(ToolItem arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tool_item_get_text_alignment argPtr1)
{-# LINE 378 "./Graphics/UI/Gtk/MenuComboToolbar/ToolItem.chs" #-}
     (toToolItem item)

-- | Returns the text orientation used for @toolItem@. Custom subclasses of 'ToolItem' should call this
-- function to find out how text should be orientated.
toolItemGetTextOrientation :: ToolItemClass item => item
                           -> IO Orientation -- ^ returns a 'Orientation' indicating the orientation used for @toolItem@
toolItemGetTextOrientation item =
  liftM (toEnum . fromIntegral) $
  (\(ToolItem arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tool_item_get_text_orientation argPtr1)
{-# LINE 387 "./Graphics/UI/Gtk/MenuComboToolbar/ToolItem.chs" #-}
     (toToolItem item)

-- | Returns the size group used for labels in @toolItem@. Custom subclasses of 'ToolItem' should call
-- this function and use the size group for labels.
toolItemGetTextSizeGroup :: ToolItemClass item => item
                         -> IO SizeGroup
toolItemGetTextSizeGroup item =
  makeNewGObject mkSizeGroup $
  (\(ToolItem arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tool_item_get_text_size_group argPtr1)
{-# LINE 396 "./Graphics/UI/Gtk/MenuComboToolbar/ToolItem.chs" #-}
     (toToolItem item)


--------------------
-- Attributes

-- | Whether the toolbar item is visible when the toolbar is in a horizontal
-- orientation.
--
-- Default value: @True@
--
toolItemVisibleHorizontal :: ToolItemClass self => Attr self Bool
toolItemVisibleHorizontal = newAttr
  toolItemGetVisibleHorizontal
  toolItemSetVisibleHorizontal

-- | Whether the toolbar item is visible when the toolbar is in a vertical
-- orientation.
--
-- Default value: @True@
--
toolItemVisibleVertical :: ToolItemClass self => Attr self Bool
toolItemVisibleVertical = newAttr
  toolItemGetVisibleVertical
  toolItemSetVisibleVertical

-- | Whether the toolbar item is considered important. When @True@, toolbar
-- buttons show text in 'ToolbarBothHoriz' mode.
--
-- Default value: @False@
--
toolItemIsImportant :: ToolItemClass self => Attr self Bool
toolItemIsImportant = newAttr
  toolItemGetIsImportant
  toolItemSetIsImportant

-- | \'expand\' property. See 'toolItemGetExpand' and 'toolItemSetExpand'
--
toolItemExpand :: ToolItemClass self => Attr self Bool
toolItemExpand = newAttr
  toolItemGetExpand
  toolItemSetExpand

-- | \'homogeneous\' property. See 'toolItemGetHomogeneous' and
-- 'toolItemSetHomogeneous'
--
toolItemHomogeneous :: ToolItemClass self => Attr self Bool
toolItemHomogeneous = newAttr
  toolItemGetHomogeneous
  toolItemSetHomogeneous

-- | \'useDragWindow\' property. See 'toolItemGetUseDragWindow' and
-- 'toolItemSetUseDragWindow'
--
toolItemUseDragWindow :: ToolItemClass self => Attr self Bool
toolItemUseDragWindow = newAttr
  toolItemGetUseDragWindow
  toolItemSetUseDragWindow

foreign import ccall unsafe "gtk_tool_item_new"
  gtk_tool_item_new :: (IO (Ptr ToolItem))

foreign import ccall safe "gtk_tool_item_set_homogeneous"
  gtk_tool_item_set_homogeneous :: ((Ptr ToolItem) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_tool_item_get_homogeneous"
  gtk_tool_item_get_homogeneous :: ((Ptr ToolItem) -> (IO CInt))

foreign import ccall safe "gtk_tool_item_set_expand"
  gtk_tool_item_set_expand :: ((Ptr ToolItem) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_tool_item_get_expand"
  gtk_tool_item_get_expand :: ((Ptr ToolItem) -> (IO CInt))

foreign import ccall safe "gtk_tool_item_set_tooltip"
  gtk_tool_item_set_tooltip :: ((Ptr ToolItem) -> ((Ptr Tooltips) -> ((Ptr CChar) -> ((Ptr CChar) -> (IO ())))))

foreign import ccall safe "gtk_tool_item_set_use_drag_window"
  gtk_tool_item_set_use_drag_window :: ((Ptr ToolItem) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_tool_item_get_use_drag_window"
  gtk_tool_item_get_use_drag_window :: ((Ptr ToolItem) -> (IO CInt))

foreign import ccall safe "gtk_tool_item_set_visible_horizontal"
  gtk_tool_item_set_visible_horizontal :: ((Ptr ToolItem) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_tool_item_get_visible_horizontal"
  gtk_tool_item_get_visible_horizontal :: ((Ptr ToolItem) -> (IO CInt))

foreign import ccall safe "gtk_tool_item_set_visible_vertical"
  gtk_tool_item_set_visible_vertical :: ((Ptr ToolItem) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_tool_item_get_visible_vertical"
  gtk_tool_item_get_visible_vertical :: ((Ptr ToolItem) -> (IO CInt))

foreign import ccall safe "gtk_tool_item_set_is_important"
  gtk_tool_item_set_is_important :: ((Ptr ToolItem) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_tool_item_get_is_important"
  gtk_tool_item_get_is_important :: ((Ptr ToolItem) -> (IO CInt))

foreign import ccall unsafe "gtk_tool_item_get_icon_size"
  gtk_tool_item_get_icon_size :: ((Ptr ToolItem) -> (IO CInt))

foreign import ccall unsafe "gtk_tool_item_get_orientation"
  gtk_tool_item_get_orientation :: ((Ptr ToolItem) -> (IO CInt))

foreign import ccall unsafe "gtk_tool_item_get_toolbar_style"
  gtk_tool_item_get_toolbar_style :: ((Ptr ToolItem) -> (IO CInt))

foreign import ccall unsafe "gtk_tool_item_get_relief_style"
  gtk_tool_item_get_relief_style :: ((Ptr ToolItem) -> (IO CInt))

foreign import ccall unsafe "gtk_tool_item_retrieve_proxy_menu_item"
  gtk_tool_item_retrieve_proxy_menu_item :: ((Ptr ToolItem) -> (IO (Ptr Widget)))

foreign import ccall unsafe "gtk_tool_item_get_proxy_menu_item"
  gtk_tool_item_get_proxy_menu_item :: ((Ptr ToolItem) -> ((Ptr CChar) -> (IO (Ptr Widget))))

foreign import ccall safe "gtk_tool_item_set_proxy_menu_item"
  gtk_tool_item_set_proxy_menu_item :: ((Ptr ToolItem) -> ((Ptr CChar) -> ((Ptr Widget) -> (IO ()))))

foreign import ccall safe "gtk_tool_item_get_ellipsize_mode"
  gtk_tool_item_get_ellipsize_mode :: ((Ptr ToolItem) -> (IO CInt))

foreign import ccall safe "gtk_tool_item_get_text_alignment"
  gtk_tool_item_get_text_alignment :: ((Ptr ToolItem) -> (IO CFloat))

foreign import ccall safe "gtk_tool_item_get_text_orientation"
  gtk_tool_item_get_text_orientation :: ((Ptr ToolItem) -> (IO CInt))

foreign import ccall safe "gtk_tool_item_get_text_size_group"
  gtk_tool_item_get_text_size_group :: ((Ptr ToolItem) -> (IO (Ptr SizeGroup)))
