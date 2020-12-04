
{-# LINE 2 "./Graphics/UI/Gtk/ModelView/CellLayout.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Interface CellLayout
--
-- Author : Axel Simon
--
-- Created: 23 January 2006
--
-- Copyright (C) 2006 Axel Simon
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
-- An interface for packing cells
--
-- * Module available since Gtk+ version 2.4
--
module Graphics.UI.Gtk.ModelView.CellLayout (
-- * Detail
--
-- | 'CellLayout' is an interface which is implemented by all objects which
-- provide a 'TreeViewColumn' API for packing cells, setting attributes and data funcs.

-- * Class Hierarchy
-- |
-- @
-- | Interface CellLayout
-- | +----'TreeViewColumn'
-- | +----'CellView'
-- | +----'IconView'
-- | +----'EntryCompletion'
-- | +----'ComboBox'
-- | +----'ComboBoxEntry'
-- @


-- * Types
  CellLayoutClass,
  toCellLayout,

-- * Methods
  cellLayoutPackStart,
  cellLayoutPackEnd,
  cellLayoutReorder,
  cellLayoutClear,
  cellLayoutClearAttributes,

  cellLayoutGetCells,

  cellLayoutAddColumnAttribute,
  cellLayoutSetAttributes,
  cellLayoutSetAttributeFunc,

  ) where

import System.Glib.FFI
import System.Glib.GList
import System.Glib.Attributes
import System.Glib.GType
import Graphics.UI.Gtk.Types
{-# LINE 73 "./Graphics/UI/Gtk/ModelView/CellLayout.chs" #-}
import Graphics.UI.Gtk.ModelView.Types
{-# LINE 74 "./Graphics/UI/Gtk/ModelView/CellLayout.chs" #-}
import Graphics.UI.Gtk.ModelView.TreeModel
{-# LINE 75 "./Graphics/UI/Gtk/ModelView/CellLayout.chs" #-}
import Graphics.UI.Gtk.ModelView.CustomStore (treeModelGetRow)


{-# LINE 78 "./Graphics/UI/Gtk/ModelView/CellLayout.chs" #-}




instance CellLayoutClass CellView
instance CellLayoutClass IconView


instance CellLayoutClass EntryCompletion
instance CellLayoutClass TreeViewColumn
instance CellLayoutClass ComboBox

instance CellLayoutClass ComboBoxEntry


--------------------
-- Methods

-- | Packs the @cell@ into the beginning of the cell layout. If @expand@ is
-- @False@, then the @cell@ is allocated no more space than it needs. Any
-- unused space is divided evenly between cells for which @expand@ is @True@.
--
-- Note that reusing the same cell renderer is not supported.
--
cellLayoutPackStart :: (CellLayoutClass self, CellRendererClass cell) => self
 -> cell -- ^ @cell@ - A 'CellRenderer'.
 -> Bool -- ^ @expand@ - @True@ if @cell@ is to be given extra space
          -- allocated to @cellLayout@.
 -> IO ()
cellLayoutPackStart self cell expand =
  (\(CellLayout arg1) (CellRenderer arg2) arg3 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_cell_layout_pack_start argPtr1 argPtr2 arg3)
{-# LINE 109 "./Graphics/UI/Gtk/ModelView/CellLayout.chs" #-}
    (toCellLayout self)
    (toCellRenderer cell)
    (fromBool expand)

-- | Adds the @cell@ to the end of @cellLayout@. If @expand@ is @False@, then
-- the @cell@ is allocated no more space than it needs. Any unused space is
-- divided evenly between cells for which @expand@ is @True@.
--
-- Note that reusing the same cell renderer is not supported.
--
cellLayoutPackEnd :: (CellLayoutClass self, CellRendererClass cell) => self
 -> cell -- ^ @cell@ - A 'CellRenderer'.
 -> Bool -- ^ @expand@ - @True@ if @cell@ is to be given extra space
          -- allocated to @cellLayout@.
 -> IO ()
cellLayoutPackEnd self cell expand =
  (\(CellLayout arg1) (CellRenderer arg2) arg3 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_cell_layout_pack_end argPtr1 argPtr2 arg3)
{-# LINE 126 "./Graphics/UI/Gtk/ModelView/CellLayout.chs" #-}
    (toCellLayout self)
    (toCellRenderer cell)
    (fromBool expand)

-- | Re-inserts @cell@ at @position@. Note that @cell@ has already to be
-- packed into @cellLayout@ for this to function properly.
--
cellLayoutReorder :: (CellLayoutClass self, CellRendererClass cell) => self
 -> cell -- ^ @cell@ - A 'CellRenderer' to reorder.
 -> Int -- ^ @position@ - New position to insert @cell@ at.
 -> IO ()
cellLayoutReorder self cell position =
  (\(CellLayout arg1) (CellRenderer arg2) arg3 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_cell_layout_reorder argPtr1 argPtr2 arg3)
{-# LINE 139 "./Graphics/UI/Gtk/ModelView/CellLayout.chs" #-}
    (toCellLayout self)
    (toCellRenderer cell)
    (fromIntegral position)

-- | Remove all renderers from the cell layout.
--
cellLayoutClear :: CellLayoutClass self => self -> IO ()
cellLayoutClear self =
  (\(CellLayout arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_cell_layout_clear argPtr1)
{-# LINE 148 "./Graphics/UI/Gtk/ModelView/CellLayout.chs" #-}
    (toCellLayout self)


-- | Returns the cell renderers which have been added to @cellLayout@.
--
-- * Available since Gtk+ version 2.12
--
cellLayoutGetCells :: CellLayoutClass self => self
 -> IO [CellRenderer] -- ^ returns a list of cell renderers
cellLayoutGetCells self =
  (\(CellLayout arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_cell_layout_get_cells argPtr1)
{-# LINE 159 "./Graphics/UI/Gtk/ModelView/CellLayout.chs" #-}
    (toCellLayout self)
  >>= fromGList
  >>= mapM (makeNewGObject mkCellRenderer . return)


-- | Adds an attribute mapping to the renderer @cell@. The @column@ is
-- the 'ColumnId' of the model to get a value from, and the @attribute@ is the
-- parameter on @cell@ to be set from the value. So for example if column 2 of
-- the model contains strings, you could have the \"text\" attribute of a
-- 'CellRendererText' get its values from column 2.
--
cellLayoutAddColumnAttribute :: (CellLayoutClass self, CellRendererClass cell) => self
 -> cell -- ^ @cell@ - A 'CellRenderer'.
 -> ReadWriteAttr cell a v -- ^ @attribute@ - An attribute of a renderer.
 -> ColumnId row v -- ^ @column@ - The virtual column of the model from which to
                      -- retrieve the attribute.
 -> IO ()
cellLayoutAddColumnAttribute self cell attr column =
  withCString (show attr) $ \attributePtr ->
  (\(CellLayout arg1) (CellRenderer arg2) arg3 arg4 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_cell_layout_add_attribute argPtr1 argPtr2 arg3 arg4)
{-# LINE 179 "./Graphics/UI/Gtk/ModelView/CellLayout.chs" #-}
    (toCellLayout self)
    (toCellRenderer cell)
    attributePtr
    (fromIntegral (columnIdToNumber column))


-- | Specify how a row of the @model@ defines the
-- attributes of the 'CellRenderer' @cell@. This is a convenience wrapper
-- around 'cellLayoutSetAttributeFunc' in that it sets the cells of the @cell@
-- with the data retrieved from the model.
--
-- * Note on using 'Graphics.UI.Gtk.ModelView.TreeModelSort.TreeModelSort' and
-- 'Graphics.UI.Gtk.ModelView.TreeModelFilter.TreeModelFilter': These two models
-- wrap another model, the so-called child model, instead of storing their own
-- data. This raises the problem that the data of cell renderers must be set
-- using the child model, while the 'TreeIter's that the view works with refer to
-- the model that encapsulates the child model. For convenience, this function
-- transparently translates an iterator to the child model before extracting the
-- data using e.g. 'Graphics.UI.Gtk.TreeModel.TreeModelSort.treeModelSortConvertIterToChildIter'.
-- Hence, it is possible to install the encapsulating model in the view and to
-- pass the child model to this function.
--
cellLayoutSetAttributes :: (CellLayoutClass self,
                             CellRendererClass cell,
                             TreeModelClass (model row),
                             TypedTreeModelClass model)
 => self
 -> cell -- ^ @cell@ - A 'CellRenderer'.
 -> model row -- ^ @model@ - A model containing rows of type @row@.
 -> (row -> [AttrOp cell]) -- ^ Function to set attributes on the cell renderer.
 -> IO ()
cellLayoutSetAttributes self cell model attributes =
  cellLayoutSetAttributeFunc self cell model $ \iter -> do
    row <- treeModelGetRow model iter
    set cell (attributes row)

-- | Install a function that looks up a row in the model and sets the
-- attributes of the 'CellRenderer' @cell@ using the row's content.
--
cellLayoutSetAttributeFunc :: (CellLayoutClass self,
                               CellRendererClass cell,
                               TreeModelClass model)
 => self
 -> cell -- ^ @cell@ - A 'CellRenderer'.
 -> model -- ^ @model@ - A model from which to draw data.
 -> (TreeIter -> IO ()) -- ^ Function to set attributes on the cell renderer.
 -> IO ()
cellLayoutSetAttributeFunc self cell model func = do
  fPtr <- mkSetAttributeFunc $ \_ cellPtr' modelPtr' iterPtr _ -> do
    iter <- convertIterFromParentToChildModel iterPtr modelPtr'
      (toTreeModel model)
    let (CellRenderer cellPtr) = toCellRenderer cell
    if unsafeForeignPtrToPtr cellPtr /= cellPtr' then
      error ("cellLayoutSetAttributeFunc: attempt to set attributes of "++
             "a different CellRenderer.")
      else func iter
  (\(CellLayout arg1) (CellRenderer arg2) arg3 arg4 arg5 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_cell_layout_set_cell_data_func argPtr1 argPtr2 arg3 arg4 arg5) (toCellLayout self)
    (toCellRenderer cell) fPtr (castFunPtrToPtr fPtr) destroyFunPtr

type CellLayoutDataFunc = FunPtr (((Ptr CellLayout) -> ((Ptr CellRenderer) -> ((Ptr TreeModel) -> ((Ptr TreeIter) -> ((Ptr ()) -> (IO ())))))))
{-# LINE 239 "./Graphics/UI/Gtk/ModelView/CellLayout.chs" #-}

foreign import ccall "wrapper" mkSetAttributeFunc ::
  (Ptr CellLayout -> Ptr CellRenderer -> Ptr TreeModel -> Ptr TreeIter ->
   Ptr () -> IO ()) -> IO CellLayoutDataFunc

-- Given a 'TreeModelFilter' or a 'TreeModelSort' and a 'TreeIter', get the
-- child model of these models and convert the iter to an iter of the child
-- model. This is an ugly internal function that is needed for some widgets
-- which pass iterators to the callback function of set_cell_data_func that
-- refer to some internal TreeModelFilter models that they create around the
-- user model. This is a bug but since C programs mostly use the columns
-- rather than the cell_layout way to extract attributes, this bug does not
-- show up in many programs. Reported in the case of EntryCompletion as bug
-- \#551202.
--
convertIterFromParentToChildModel ::
     Ptr TreeIter -- ^ the iterator
  -> Ptr TreeModel -- ^ the model that we got from the all back
  -> TreeModel -- ^ the model that we actually want
  -> IO TreeIter
convertIterFromParentToChildModel iterPtr parentModelPtr childModel =
  let (TreeModel modelFPtr) = childModel
      modelPtr = unsafeForeignPtrToPtr modelFPtr in
  if modelPtr==parentModelPtr then peek iterPtr else
  if typeInstanceIsA (castPtr parentModelPtr) gTypeTreeModelFilter then
    alloca $ \childIterPtr -> do
      treeModelFilterConvertIterToChildIter parentModelPtr childIterPtr iterPtr
      childPtr <- treeModelFilterGetModel parentModelPtr
      if childPtr==modelPtr then peek childIterPtr else
        convertIterFromParentToChildModel childIterPtr childPtr childModel
  else if typeInstanceIsA (castPtr parentModelPtr) gTypeTreeModelSort then
    alloca $ \childIterPtr -> do
      treeModelSortConvertIterToChildIter parentModelPtr childIterPtr iterPtr
      childPtr <- treeModelSortGetModel parentModelPtr
      if childPtr==modelPtr then peek childIterPtr else
        convertIterFromParentToChildModel childIterPtr childPtr childModel
  else do
    iter <- peek iterPtr
    error ("CellLayout: don't know how to convert iter "++show iter++
           " from model "++show parentModelPtr++" to model "++
           show modelPtr++". Is it possible that you are setting the "++
           "attributes of a CellRenderer using a different model than "++
           "that which was set in the view?")

foreign import ccall unsafe "gtk_tree_model_filter_get_model"
  treeModelFilterGetModel :: Ptr TreeModel -> IO (Ptr TreeModel)

foreign import ccall safe "gtk_tree_model_filter_convert_iter_to_child_iter"
  treeModelFilterConvertIterToChildIter :: Ptr TreeModel -> Ptr TreeIter ->
    Ptr TreeIter -> IO ()

foreign import ccall unsafe "gtk_tree_model_sort_get_model"
  treeModelSortGetModel :: Ptr TreeModel -> IO (Ptr TreeModel)

foreign import ccall safe "gtk_tree_model_sort_convert_iter_to_child_iter"
  treeModelSortConvertIterToChildIter :: Ptr TreeModel -> Ptr TreeIter ->
    Ptr TreeIter -> IO ()

-- | Clears all existing attributes previously set with
-- 'cellLayoutSetAttributes'.
--
cellLayoutClearAttributes :: (CellLayoutClass self, CellRendererClass cell) => self
 -> cell -- ^ @cell@ - A 'CellRenderer' to clear the attribute mapping on.
 -> IO ()
cellLayoutClearAttributes self cell =
  (\(CellLayout arg1) (CellRenderer arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_cell_layout_clear_attributes argPtr1 argPtr2)
{-# LINE 305 "./Graphics/UI/Gtk/ModelView/CellLayout.chs" #-}
    (toCellLayout self)
    (toCellRenderer cell)

foreign import ccall safe "gtk_cell_layout_pack_start"
  gtk_cell_layout_pack_start :: ((Ptr CellLayout) -> ((Ptr CellRenderer) -> (CInt -> (IO ()))))

foreign import ccall safe "gtk_cell_layout_pack_end"
  gtk_cell_layout_pack_end :: ((Ptr CellLayout) -> ((Ptr CellRenderer) -> (CInt -> (IO ()))))

foreign import ccall safe "gtk_cell_layout_reorder"
  gtk_cell_layout_reorder :: ((Ptr CellLayout) -> ((Ptr CellRenderer) -> (CInt -> (IO ()))))

foreign import ccall safe "gtk_cell_layout_clear"
  gtk_cell_layout_clear :: ((Ptr CellLayout) -> (IO ()))

foreign import ccall safe "gtk_cell_layout_get_cells"
  gtk_cell_layout_get_cells :: ((Ptr CellLayout) -> (IO (Ptr ())))

foreign import ccall safe "gtk_cell_layout_add_attribute"
  gtk_cell_layout_add_attribute :: ((Ptr CellLayout) -> ((Ptr CellRenderer) -> ((Ptr CChar) -> (CInt -> (IO ())))))

foreign import ccall safe "gtk_cell_layout_set_cell_data_func"
  gtk_cell_layout_set_cell_data_func :: ((Ptr CellLayout) -> ((Ptr CellRenderer) -> ((FunPtr ((Ptr CellLayout) -> ((Ptr CellRenderer) -> ((Ptr TreeModel) -> ((Ptr TreeIter) -> ((Ptr ()) -> (IO ()))))))) -> ((Ptr ()) -> ((FunPtr ((Ptr ()) -> (IO ()))) -> (IO ()))))))

foreign import ccall safe "gtk_cell_layout_clear_attributes"
  gtk_cell_layout_clear_attributes :: ((Ptr CellLayout) -> ((Ptr CellRenderer) -> (IO ())))
