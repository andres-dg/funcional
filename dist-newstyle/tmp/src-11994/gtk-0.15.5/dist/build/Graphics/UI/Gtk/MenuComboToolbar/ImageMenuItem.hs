
{-# LINE 2 "./Graphics/UI/Gtk/MenuComboToolbar/ImageMenuItem.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget ImageMenuItem
--
-- Author : Jonas Svensson
--
-- Created: 12 Aug 2002
--
-- Copyright (C) 2002 Jonas Svensson
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
-- TODO
--
-- imageMenuItemNewFromSock should also have a AccelGroup argument
--
-- |
-- Maintainer : gtk2hs-users@lists.sourceforge.net
-- Stability : provisional
-- Portability : portable (depends on GHC)
--
-- A menu item with an icon
--
module Graphics.UI.Gtk.MenuComboToolbar.ImageMenuItem (
-- * Detail
--
-- | A 'ImageMenuItem' is a menu item which has an icon next to the text
-- label.
--
-- Note that the user can disable display of menu icons, so make sure to
-- still fill in the text label.

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
-- | +----ImageMenuItem
-- @

-- * Types
  ImageMenuItem,
  ImageMenuItemClass,
  castToImageMenuItem, gTypeImageMenuItem,
  toImageMenuItem,

-- * Constructors
  imageMenuItemNew,
  imageMenuItemNewFromStock,
  imageMenuItemNewWithLabel,
  imageMenuItemNewWithMnemonic,

-- * Methods
  imageMenuItemSetImage,
  imageMenuItemGetImage,

-- * Attributes
  imageMenuItemImage,
  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.UI.Gtk.Types
{-# LINE 81 "./Graphics/UI/Gtk/MenuComboToolbar/ImageMenuItem.chs" #-}
import Graphics.UI.Gtk.General.StockItems


{-# LINE 84 "./Graphics/UI/Gtk/MenuComboToolbar/ImageMenuItem.chs" #-}

--------------------
-- Constructors

-- | Creates a new 'ImageMenuItem' with an empty label.
--
imageMenuItemNew :: IO ImageMenuItem
imageMenuItemNew =
  makeNewObject mkImageMenuItem $
  liftM (castPtr :: Ptr Widget -> Ptr ImageMenuItem) $
  gtk_image_menu_item_new
{-# LINE 95 "./Graphics/UI/Gtk/MenuComboToolbar/ImageMenuItem.chs" #-}

-- | Creates a new 'ImageMenuItem' containing the image and text from a stock
-- item.
--
imageMenuItemNewFromStock ::
    StockId -- ^ @stockId@ - the name of the stock item.
 -> IO ImageMenuItem
imageMenuItemNewFromStock stockId =
  makeNewObject mkImageMenuItem $
  liftM (castPtr :: Ptr Widget -> Ptr ImageMenuItem) $
  withUTFString stockId $ \stockIdPtr ->
  (\arg1 (AccelGroup arg2) -> withForeignPtr arg2 $ \argPtr2 ->gtk_image_menu_item_new_from_stock arg1 argPtr2)
{-# LINE 107 "./Graphics/UI/Gtk/MenuComboToolbar/ImageMenuItem.chs" #-}
    stockIdPtr
    (AccelGroup nullForeignPtr)

-- | Creates a new 'ImageMenuItem' containing a label.
--
imageMenuItemNewWithLabel :: GlibString string
 => string -- ^ @label@ - the text of the menu item.
 -> IO ImageMenuItem
imageMenuItemNewWithLabel label =
  makeNewObject mkImageMenuItem $
  liftM (castPtr :: Ptr Widget -> Ptr ImageMenuItem) $
  withUTFString label $ \labelPtr ->
  gtk_image_menu_item_new_with_label
{-# LINE 120 "./Graphics/UI/Gtk/MenuComboToolbar/ImageMenuItem.chs" #-}
    labelPtr

-- | Creates a new 'ImageMenuItem' containing a label. The label will be
-- created using 'Graphics.UI.Gtk.Display.Label.labelNewWithMnemonic', so
-- underscores in @label@ indicate the mnemonic for the menu item.
--
imageMenuItemNewWithMnemonic :: GlibString string
 => string -- ^ @label@ - the text of the menu item, with an
                     -- underscore in front of the mnemonic character
 -> IO ImageMenuItem
imageMenuItemNewWithMnemonic label =
  makeNewObject mkImageMenuItem $
  liftM (castPtr :: Ptr Widget -> Ptr ImageMenuItem) $
  withUTFString label $ \labelPtr ->
  gtk_image_menu_item_new_with_mnemonic
{-# LINE 135 "./Graphics/UI/Gtk/MenuComboToolbar/ImageMenuItem.chs" #-}
    labelPtr

--------------------
-- Methods

-- | Sets the image of the image menu item to the given widget. Note that it
-- depends on the \"show-menu-images\" setting whether the image will be
-- displayed or not.
--
imageMenuItemSetImage :: (ImageMenuItemClass self, WidgetClass image) => self
 -> image -- ^ @image@ - a widget to set as the image for the menu item.
 -> IO ()
imageMenuItemSetImage self image =
  (\(ImageMenuItem arg1) (Widget arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_image_menu_item_set_image argPtr1 argPtr2)
{-# LINE 149 "./Graphics/UI/Gtk/MenuComboToolbar/ImageMenuItem.chs" #-}
    (toImageMenuItem self)
    (toWidget image)

-- | Gets the widget that is currently set as the image.
-- See 'imageMenuItemSetImage'.
--
imageMenuItemGetImage :: ImageMenuItemClass self => self
 -> IO (Maybe Widget) -- ^ returns the widget set as image of or @Nothing@ if
                      -- none has been set.
imageMenuItemGetImage self =
  maybeNull (makeNewObject mkWidget) $
  (\(ImageMenuItem arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_image_menu_item_get_image argPtr1)
{-# LINE 161 "./Graphics/UI/Gtk/MenuComboToolbar/ImageMenuItem.chs" #-}
    (toImageMenuItem self)

--------------------
-- Attributes

-- | Child widget to appear next to the menu text.
--
imageMenuItemImage :: (ImageMenuItemClass self, WidgetClass image) => ReadWriteAttr self (Maybe Widget) image
imageMenuItemImage = newAttr
  imageMenuItemGetImage
  imageMenuItemSetImage

foreign import ccall unsafe "gtk_image_menu_item_new"
  gtk_image_menu_item_new :: (IO (Ptr Widget))

foreign import ccall unsafe "gtk_image_menu_item_new_from_stock"
  gtk_image_menu_item_new_from_stock :: ((Ptr CChar) -> ((Ptr AccelGroup) -> (IO (Ptr Widget))))

foreign import ccall unsafe "gtk_image_menu_item_new_with_label"
  gtk_image_menu_item_new_with_label :: ((Ptr CChar) -> (IO (Ptr Widget)))

foreign import ccall unsafe "gtk_image_menu_item_new_with_mnemonic"
  gtk_image_menu_item_new_with_mnemonic :: ((Ptr CChar) -> (IO (Ptr Widget)))

foreign import ccall unsafe "gtk_image_menu_item_set_image"
  gtk_image_menu_item_set_image :: ((Ptr ImageMenuItem) -> ((Ptr Widget) -> (IO ())))

foreign import ccall unsafe "gtk_image_menu_item_get_image"
  gtk_image_menu_item_get_image :: ((Ptr ImageMenuItem) -> (IO (Ptr Widget)))
