
{-# LINE 2 "./Graphics/UI/Gtk/ModelView/TreeModelSort.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) TreeModelSort
--
-- Author : Duncan Coutts
--
-- Created: 4 August 2004
--
-- Copyright (C) 2004-2005 Duncan Coutts, Axel Simon
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
-- A 'TreeModel' which makes an underlying tree model sortable
--
module Graphics.UI.Gtk.ModelView.TreeModelSort (
-- * Detail
--
-- | The 'TreeModelSort' is a model which implements the 'TreeSortable'
-- interface. It does not hold any data itself, but rather is created with a
-- child model and proxies its data. It has identical rows to its
-- child model, and the changes in the child are propagated. The primary
-- purpose of this model is to provide a way to sort a model without
-- modifying it.

-- * Class Hierarchy
-- |
-- @
-- | 'GObject'
-- | +----TreeModelSort
-- @

-- * Types
  TreeModelSort,
  TreeModelSortClass,
  castToTreeModelSort, gTypeTreeModelSort,
  toTreeModelSort,

  TypedTreeModelSort,

-- * Constructors
  treeModelSortNewWithModel,

-- * Methods
  treeModelSortGetModel,
  treeModelSortConvertChildPathToPath,
  treeModelSortConvertPathToChildPath,
  treeModelSortConvertChildIterToIter,
  treeModelSortConvertIterToChildIter,
  treeModelSortResetDefaultSortFunc,
  treeModelSortClearCache,

  treeModelSortIterIsValid,

  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import Graphics.UI.Gtk.Types
{-# LINE 73 "./Graphics/UI/Gtk/ModelView/TreeModelSort.chs" #-}
import Graphics.UI.Gtk.ModelView.TreeModel
{-# LINE 74 "./Graphics/UI/Gtk/ModelView/TreeModelSort.chs" #-}
import Graphics.UI.Gtk.ModelView.Types
{-# LINE 75 "./Graphics/UI/Gtk/ModelView/TreeModelSort.chs" #-}


{-# LINE 77 "./Graphics/UI/Gtk/ModelView/TreeModelSort.chs" #-}

instance TreeModelClass (TypedTreeModelSort a)
instance TreeModelSortClass (TypedTreeModelSort a)
instance GObjectClass (TypedTreeModelSort a) where
  toGObject (TypedTreeModelSort tm) = GObject (castForeignPtr tm)
  unsafeCastGObject = TypedTreeModelSort . castForeignPtr . unGObject
instance TreeSortableClass TreeModelSort
instance TreeSortableClass (TypedTreeModelSort row)

--------------------
-- Constructors

-- | Creates a new 'TreeModelSort', that will be a sorted view of the given
-- model.
--
treeModelSortNewWithModel :: (TreeModelClass (childModel row),
                              TypedTreeModelClass childModel) =>
                              childModel row -> IO (TypedTreeModelSort row)
treeModelSortNewWithModel childModel = liftM unsafeTreeModelSortToGeneric $
  wrapNewGObject mkTreeModelSort $
  liftM (castPtr :: Ptr TreeModel -> Ptr TreeModelSort) $
  (\(TreeModel arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_model_sort_new_with_model argPtr1)
{-# LINE 99 "./Graphics/UI/Gtk/ModelView/TreeModelSort.chs" #-}
    (toTreeModel childModel)

--------------------
-- Methods

-- | Returns the underlying model the 'TreeModelSort' is sorting.
--
treeModelSortGetModel :: TreeModelSortClass self => self -> IO TreeModel
treeModelSortGetModel self =
  makeNewGObject mkTreeModel $
  (\(TreeModelSort arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_model_sort_get_model argPtr1)
{-# LINE 110 "./Graphics/UI/Gtk/ModelView/TreeModelSort.chs" #-}
    (toTreeModelSort self)

-- | Converts the given path to a path relative to the given sorted model.
--
-- * The given path points to a row in the child model. The returned path will
-- point to the same row in the sorted model.
--
treeModelSortConvertChildPathToPath :: TreeModelSortClass self => self
 -> TreePath
 -> IO TreePath
treeModelSortConvertChildPathToPath self [] = return []
treeModelSortConvertChildPathToPath self childPath =
  withTreePath childPath $ \childPath ->
  (\(TreeModelSort arg1) (NativeTreePath arg2) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_model_sort_convert_child_path_to_path argPtr1 arg2)
{-# LINE 124 "./Graphics/UI/Gtk/ModelView/TreeModelSort.chs" #-}
    (toTreeModelSort self)
    childPath
  >>= fromTreePath

-- | Converts path in the sorted model to a path on the unsorted model on which
-- the given 'TreeModelSort' is based. That is, the given path points to a
-- location in the given 'TreeModelSort'. The returned path will point to the
-- same location in the underlying unsorted model.
--
treeModelSortConvertPathToChildPath :: TreeModelSortClass self => self
 -> TreePath
 -> IO TreePath
treeModelSortConvertPathToChildPath self [] = return []
treeModelSortConvertPathToChildPath self sortedPath =
  withTreePath sortedPath $ \sortedPath ->
  (\(TreeModelSort arg1) (NativeTreePath arg2) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_model_sort_convert_path_to_child_path argPtr1 arg2)
{-# LINE 140 "./Graphics/UI/Gtk/ModelView/TreeModelSort.chs" #-}
    (toTreeModelSort self)
    sortedPath
  >>= fromTreePath

-- | Return an iterator in the sorted model that points to the row pointed to
-- by the given iter from the unsorted model.
--
treeModelSortConvertChildIterToIter :: TreeModelSortClass self => self
 -> TreeIter
 -> IO TreeIter
treeModelSortConvertChildIterToIter self childIter =
  with childIter $ \childIterPtr ->
  alloca $ \sortIterPtr -> do
  (\(TreeModelSort arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_model_sort_convert_child_iter_to_iter argPtr1 arg2 arg3)
{-# LINE 154 "./Graphics/UI/Gtk/ModelView/TreeModelSort.chs" #-}
    (toTreeModelSort self)
    sortIterPtr
    childIterPtr
  peek sortIterPtr

-- | Return an iterator in the unsorted model that points to the row pointed to
-- by the given iter from the sorted model.
--
treeModelSortConvertIterToChildIter :: TreeModelSortClass self => self
 -> TreeIter
 -> IO TreeIter
treeModelSortConvertIterToChildIter self sortedIter =
  with sortedIter $ \sortedIterPtr ->
  alloca $ \childIterPtr -> do
  (\(TreeModelSort arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_model_sort_convert_iter_to_child_iter argPtr1 arg2 arg3)
{-# LINE 169 "./Graphics/UI/Gtk/ModelView/TreeModelSort.chs" #-}
    (toTreeModelSort self)
    childIterPtr
    sortedIterPtr
  peek childIterPtr

-- | This resets the default sort function. As a consequence, the order of
-- this model will be the same order as that of the child model.
--
treeModelSortResetDefaultSortFunc :: TreeModelSortClass self => self -> IO ()
treeModelSortResetDefaultSortFunc self =
  (\(TreeModelSort arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_model_sort_reset_default_sort_func argPtr1)
{-# LINE 180 "./Graphics/UI/Gtk/ModelView/TreeModelSort.chs" #-}
    (toTreeModelSort self)

-- | Clear the cache of unref'd iterators.
--
-- * This function should almost never be called. It clears the
-- 'TreeModelSort' of any cached iterators that haven't been reffed with
-- 'treeModelRefNode'. This might be useful if the child model being sorted is
-- static (and doesn't change often) and there has been a lot of unreffed
-- access to nodes. As a side effect of this function, all unreffed iters will
-- be invalid.
--
treeModelSortClearCache :: TreeModelSortClass self => self -> IO ()
treeModelSortClearCache self =
  (\(TreeModelSort arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_model_sort_clear_cache argPtr1)
{-# LINE 194 "./Graphics/UI/Gtk/ModelView/TreeModelSort.chs" #-}
    (toTreeModelSort self)


-- | Checks if the given iter is a valid iter for this 'TreeModelSort'.
--
-- * WARNING: This function is slow. Only use it for debugging and\/or testing
-- purposes.
--
-- * Available since Gtk+ version 2.2
--
treeModelSortIterIsValid :: TreeModelSortClass self => self
 -> TreeIter -- ^ @iter@ - A 'TreeIter'.
 -> IO Bool -- ^ returns @True@ if the iter is valid, @False@ if the iter is
             -- invalid.
treeModelSortIterIsValid self iter =
  liftM toBool $
  with iter $ \iterPtr ->
  (\(TreeModelSort arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_model_sort_iter_is_valid argPtr1 arg2)
{-# LINE 212 "./Graphics/UI/Gtk/ModelView/TreeModelSort.chs" #-}
    (toTreeModelSort self)
    iterPtr

foreign import ccall safe "gtk_tree_model_sort_new_with_model"
  gtk_tree_model_sort_new_with_model :: ((Ptr TreeModel) -> (IO (Ptr TreeModel)))

foreign import ccall safe "gtk_tree_model_sort_get_model"
  gtk_tree_model_sort_get_model :: ((Ptr TreeModelSort) -> (IO (Ptr TreeModel)))

foreign import ccall safe "gtk_tree_model_sort_convert_child_path_to_path"
  gtk_tree_model_sort_convert_child_path_to_path :: ((Ptr TreeModelSort) -> ((Ptr NativeTreePath) -> (IO (Ptr NativeTreePath))))

foreign import ccall safe "gtk_tree_model_sort_convert_path_to_child_path"
  gtk_tree_model_sort_convert_path_to_child_path :: ((Ptr TreeModelSort) -> ((Ptr NativeTreePath) -> (IO (Ptr NativeTreePath))))

foreign import ccall safe "gtk_tree_model_sort_convert_child_iter_to_iter"
  gtk_tree_model_sort_convert_child_iter_to_iter :: ((Ptr TreeModelSort) -> ((Ptr TreeIter) -> ((Ptr TreeIter) -> (IO CInt))))

foreign import ccall safe "gtk_tree_model_sort_convert_iter_to_child_iter"
  gtk_tree_model_sort_convert_iter_to_child_iter :: ((Ptr TreeModelSort) -> ((Ptr TreeIter) -> ((Ptr TreeIter) -> (IO ()))))

foreign import ccall safe "gtk_tree_model_sort_reset_default_sort_func"
  gtk_tree_model_sort_reset_default_sort_func :: ((Ptr TreeModelSort) -> (IO ()))

foreign import ccall safe "gtk_tree_model_sort_clear_cache"
  gtk_tree_model_sort_clear_cache :: ((Ptr TreeModelSort) -> (IO ()))

foreign import ccall safe "gtk_tree_model_sort_iter_is_valid"
  gtk_tree_model_sort_iter_is_valid :: ((Ptr TreeModelSort) -> ((Ptr TreeIter) -> (IO CInt)))
