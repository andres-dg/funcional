
{-# LINE 2 "./Graphics/UI/Gtk/ModelView/CellView.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget CellView
--
-- Author : Duncan Coutts
--
-- Created: 4 April 2005
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
-- A widget displaying a single row of a 'TreeModel'
--
-- * Module available since Gtk+ version 2.6
--
module Graphics.UI.Gtk.ModelView.CellView (
-- * Detail
--
-- | A 'CellView' displays a single row of a 'TreeModel', using cell renderers
-- just like 'TreeView'. 'CellView' doesn't support some of the more complex
-- features of 'TreeView', like cell editing and drag and drop.

-- * Class Hierarchy
-- |
-- @
-- | 'GObject'
-- | +----'Object'
-- | +----'Widget'
-- | +----CellView
-- @


-- * Types
  CellView,
  CellViewClass,
  castToCellView, gTypeCellView,
  toCellView,

-- * Constructors
  cellViewNew,
  cellViewNewWithMarkup,
  cellViewNewWithPixbuf,
  cellViewNewWithText,

-- * Methods
  cellViewSetModel,
  cellViewGetSizeOfRow,
  cellViewSetBackgroundColor,

  cellViewGetCellRenderers,


-- * Attributes
  cellViewBackground

  ) where

import Control.Monad (liftM)
import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import System.Glib.Properties (writeAttrFromStringProperty)

import System.Glib.GList
{-# LINE 79 "./Graphics/UI/Gtk/ModelView/CellView.chs" #-}

import Graphics.UI.Gtk.Types
{-# LINE 81 "./Graphics/UI/Gtk/ModelView/CellView.chs" #-}
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.UI.Gtk.ModelView.Types
{-# LINE 83 "./Graphics/UI/Gtk/ModelView/CellView.chs" #-}
import Graphics.UI.Gtk.General.Structs (Color, Requisition)


{-# LINE 86 "./Graphics/UI/Gtk/ModelView/CellView.chs" #-}


--------------------
-- Constructors

-- | Creates a new 'CellView' widget.
--
cellViewNew :: IO CellView
cellViewNew =
  makeNewObject mkCellView $
  liftM (castPtr :: Ptr Widget -> Ptr CellView) $
  gtk_cell_view_new
{-# LINE 98 "./Graphics/UI/Gtk/ModelView/CellView.chs" #-}

-- | Creates a new 'CellView' widget, adds a 'CellRendererText' to it, and
-- makes its show @markup@. The text can be marked up with the Pango
-- text markup language.
--
cellViewNewWithMarkup :: GlibString string
 => string -- ^ @markup@ - the text to display in the cell view
 -> IO CellView
cellViewNewWithMarkup markup =
  makeNewObject mkCellView $
  liftM (castPtr :: Ptr Widget -> Ptr CellView) $
  withUTFString markup $ \markupPtr ->
  gtk_cell_view_new_with_markup
{-# LINE 111 "./Graphics/UI/Gtk/ModelView/CellView.chs" #-}
    markupPtr

-- | Creates a new 'CellView' widget, adds a 'CellRendererPixbuf' to it, and
-- makes its show @pixbuf@.
--
cellViewNewWithPixbuf ::
    Pixbuf -- ^ @pixbuf@ - the image to display in the cell view
 -> IO CellView
cellViewNewWithPixbuf pixbuf =
  makeNewObject mkCellView $
  liftM (castPtr :: Ptr Widget -> Ptr CellView) $
  (\(Pixbuf arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_cell_view_new_with_pixbuf argPtr1)
{-# LINE 123 "./Graphics/UI/Gtk/ModelView/CellView.chs" #-}
    pixbuf

-- | Creates a new 'CellView' widget, adds a 'CellRendererText' to it, and
-- makes its show @text@.
--
cellViewNewWithText :: GlibString string
 => string -- ^ @text@ - the text to display in the cell view
 -> IO CellView
cellViewNewWithText text =
  makeNewObject mkCellView $
  liftM (castPtr :: Ptr Widget -> Ptr CellView) $
  withUTFString text $ \textPtr ->
  gtk_cell_view_new_with_text
{-# LINE 136 "./Graphics/UI/Gtk/ModelView/CellView.chs" #-}
    textPtr

--------------------
-- Methods

-- | Sets the model for @cellView@. If @cellView@ already has a model set, it
-- will remove it before setting the new model. If @model@ is @Nothing@, then
-- it will unset the old model.
--
cellViewSetModel :: (CellViewClass self, TreeModelClass model) => self
 -> Maybe model -- ^ @model@ - a 'TreeModel'
 -> IO ()
cellViewSetModel self model =
  (\(CellView arg1) (TreeModel arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_cell_view_set_model argPtr1 argPtr2)
{-# LINE 150 "./Graphics/UI/Gtk/ModelView/CellView.chs" #-}
    (toCellView self)
    (maybe (TreeModel nullForeignPtr) toTreeModel model)

-- | Returns the size needed by the cell view to display the model
-- row pointed to by @path@.
--
cellViewGetSizeOfRow :: CellViewClass self => self
 -> TreePath -- ^ @path@ - a 'TreePath'
 -> IO Requisition -- ^ returns the size requisition
cellViewGetSizeOfRow self path =
  alloca $ \requisitionPtr ->
  withTreePath path $ \path -> do
  (\(CellView arg1) (NativeTreePath arg2) arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_cell_view_get_size_of_row argPtr1 arg2 arg3)
{-# LINE 163 "./Graphics/UI/Gtk/ModelView/CellView.chs" #-}
    (toCellView self)
    path
    (castPtr requisitionPtr)
  peek requisitionPtr

-- | Sets the background color of @view@.
--
cellViewSetBackgroundColor :: CellViewClass self => self
 -> Color -- ^ @color@ - the new background color
 -> IO ()
cellViewSetBackgroundColor self color =
  with color $ \colorPtr ->
  (\(CellView arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_cell_view_set_background_color argPtr1 arg2)
{-# LINE 176 "./Graphics/UI/Gtk/ModelView/CellView.chs" #-}
    (toCellView self)
    (castPtr colorPtr)


-- | Returns the cell renderers which have been added to @cellView@.
--
-- Removed in Gtk3.
cellViewGetCellRenderers :: CellViewClass self => self -> IO [CellRenderer]
cellViewGetCellRenderers self =
  (\(CellView arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_cell_view_get_cell_renderers argPtr1)
{-# LINE 186 "./Graphics/UI/Gtk/ModelView/CellView.chs" #-}
    (toCellView self)
  >>= fromGList
  >>= mapM (\elemPtr -> makeNewObject mkCellRenderer (return elemPtr))


--------------------
-- Attributes

-- | Background color as a string.
--
-- Default value: @\"\"@
--
cellViewBackground :: (CellViewClass self, GlibString string) => WriteAttr self string
cellViewBackground = writeAttrFromStringProperty "background"

foreign import ccall safe "gtk_cell_view_new"
  gtk_cell_view_new :: (IO (Ptr Widget))

foreign import ccall safe "gtk_cell_view_new_with_markup"
  gtk_cell_view_new_with_markup :: ((Ptr CChar) -> (IO (Ptr Widget)))

foreign import ccall safe "gtk_cell_view_new_with_pixbuf"
  gtk_cell_view_new_with_pixbuf :: ((Ptr Pixbuf) -> (IO (Ptr Widget)))

foreign import ccall safe "gtk_cell_view_new_with_text"
  gtk_cell_view_new_with_text :: ((Ptr CChar) -> (IO (Ptr Widget)))

foreign import ccall safe "gtk_cell_view_set_model"
  gtk_cell_view_set_model :: ((Ptr CellView) -> ((Ptr TreeModel) -> (IO ())))

foreign import ccall safe "gtk_cell_view_get_size_of_row"
  gtk_cell_view_get_size_of_row :: ((Ptr CellView) -> ((Ptr NativeTreePath) -> ((Ptr ()) -> (IO CInt))))

foreign import ccall safe "gtk_cell_view_set_background_color"
  gtk_cell_view_set_background_color :: ((Ptr CellView) -> ((Ptr ()) -> (IO ())))

foreign import ccall safe "gtk_cell_view_get_cell_renderers"
  gtk_cell_view_get_cell_renderers :: ((Ptr CellView) -> (IO (Ptr ())))
