
{-# LINE 2 "./Graphics/UI/Gtk/ModelView/TreeRowReference.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Class TreeRowReference
--
-- Author : Duncan Coutts
--
-- Created: 14 April 2005
--
-- Copyright (C) 2005 Axel Simon, Duncan Coutts
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
-- A persistent index into a tree model.
--
module Graphics.UI.Gtk.ModelView.TreeRowReference (
-- * Detail
--
-- | A 'RowReference' is an index into a
-- 'Graphics.UI.Gtk.ModelView.TreeModel.TreeModel' that is persistent even if
-- rows are inserted, deleted or reordered.
--

-- * Types
  TreeRowReference,

-- * Constructors
  treeRowReferenceNew,

-- * Methods
  treeRowReferenceGetPath,
  treeRowReferenceValid,
  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import Graphics.UI.Gtk.Types
{-# LINE 51 "./Graphics/UI/Gtk/ModelView/TreeRowReference.chs" #-}
import Graphics.UI.Gtk.ModelView.Types
{-# LINE 52 "./Graphics/UI/Gtk/ModelView/TreeRowReference.chs" #-}


{-# LINE 54 "./Graphics/UI/Gtk/ModelView/TreeRowReference.chs" #-}

-- | Tree Row Reference : like a 'TreePath' it points to a subtree or node, but
-- it is persistent. It identifies the same node (so long as it exists) even
-- when items are added, removed, or reordered.
--
newtype TreeRowReference = TreeRowReference (ForeignPtr (TreeRowReference))
{-# LINE 60 "./Graphics/UI/Gtk/ModelView/TreeRowReference.chs" #-}

--------------------
-- Constructors

-- | Creates a row reference based on a path. This reference will keep pointing
-- to the node pointed to by the given path, so long as it exists. Returns @Nothing@ if there is no node at the given path.
--
treeRowReferenceNew :: TreeModelClass self => self
 -> TreePath
 -> IO (Maybe TreeRowReference)
treeRowReferenceNew self path = withTreePath path $ \path -> do
  rowRefPtr <-
    (\(TreeModel arg1) (NativeTreePath arg2) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_row_reference_new argPtr1 arg2) (toTreeModel self) path
  if rowRefPtr==nullPtr then return Nothing else
    liftM (Just . TreeRowReference) $
    newForeignPtr rowRefPtr tree_row_reference_free

--------------------
-- Methods

-- | Returns a path that the row reference currently points to.
--
-- * The returned path may be the empty list if the reference was invalid.
--
treeRowReferenceGetPath :: TreeRowReference -> IO TreePath
treeRowReferenceGetPath ref =
  (\(TreeRowReference arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_row_reference_get_path argPtr1) ref
  >>= fromTreePath -- path must be freed

-- | Returns True if the reference refers to a current valid path.
--
treeRowReferenceValid :: TreeRowReference -> IO Bool
treeRowReferenceValid self =
  liftM toBool $
  (\(TreeRowReference arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_row_reference_valid argPtr1)
{-# LINE 95 "./Graphics/UI/Gtk/ModelView/TreeRowReference.chs" #-}
    self

foreign import ccall unsafe "&gtk_tree_row_reference_free"
  tree_row_reference_free :: FinalizerPtr TreeRowReference

foreign import ccall safe "gtk_tree_row_reference_new"
  gtk_tree_row_reference_new :: ((Ptr TreeModel) -> ((Ptr NativeTreePath) -> (IO (Ptr TreeRowReference))))

foreign import ccall unsafe "gtk_tree_row_reference_get_path"
  gtk_tree_row_reference_get_path :: ((Ptr TreeRowReference) -> (IO (Ptr NativeTreePath)))

foreign import ccall unsafe "gtk_tree_row_reference_valid"
  gtk_tree_row_reference_valid :: ((Ptr TreeRowReference) -> (IO CInt))
