
{-# LINE 2 "./Graphics/UI/Gtk/ModelView/TreeSelection.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget TreeSelection
--
-- Author : Axel Simon
--
-- Created: 8 May 2001
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
-- The selection object for 'TreeView'
--
module Graphics.UI.Gtk.ModelView.TreeSelection (
-- * Detail
--
-- | The 'TreeSelection' object is a helper object to manage the selection for
-- a 'TreeView' widget. The 'TreeSelection' object is automatically created
-- when a new 'TreeView' widget is created, and cannot exist independentally of
-- this widget. The primary reason the 'TreeSelection' objects exists is for
-- cleanliness of code and API. That is, there is no conceptual reason all
-- these functions could not be methods on the 'TreeView' widget instead of a
-- separate function.
--
-- The 'TreeSelection' object is gotten from a 'TreeView' by calling
-- 'treeViewGetSelection'. It can be
-- manipulated to check the selection status of the tree, as well as select
-- and deselect individual rows. Selection is done completely on the
-- 'TreeView' side. As a result, multiple views of the same model can
-- have completely different selections. Additionally, you cannot change the
-- selection of a row on the model that is not currently displayed by the view
-- without expanding its parents first.
--
-- One of the important things to remember when monitoring the selection of
-- a view is that the \"changed\" signal is mostly a hint. That is, it may only
-- emit one signal when a range of rows is selected. Additionally, it may on
-- occasion emit a \"changed\" signal when nothing has happened (mostly as a
-- result of programmers calling select_row on an already selected row).

-- * Class Hierarchy
-- |
-- @
-- | 'GObject'
-- | +----TreeSelection
-- @

-- * Types
  TreeSelection,
  TreeSelectionClass,
  castToTreeSelection, gTypeTreeSelection,
  toTreeSelection,
  SelectionMode(..),
  TreeSelectionCB,
  TreeSelectionForeachCB,

-- * Methods
  treeSelectionSetMode,
  treeSelectionGetMode,
  treeSelectionSetSelectFunction,
  treeSelectionGetTreeView,
  treeSelectionGetSelected,
  treeSelectionSelectedForeach,

  treeSelectionGetSelectedRows,
  treeSelectionCountSelectedRows,

  treeSelectionSelectPath,
  treeSelectionUnselectPath,
  treeSelectionPathIsSelected,
  treeSelectionSelectIter,
  treeSelectionUnselectIter,
  treeSelectionIterIsSelected,
  treeSelectionSelectAll,
  treeSelectionUnselectAll,
  treeSelectionSelectRange,

  treeSelectionUnselectRange,


-- * Attributes
  treeSelectionMode,

-- * Signals
  treeSelectionSelectionChanged,


-- * Deprecated
  onSelectionChanged,
  afterSelectionChanged

  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.GList (fromGList)
import System.Glib.Attributes
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.UI.Gtk.Types
{-# LINE 114 "./Graphics/UI/Gtk/ModelView/TreeSelection.chs" #-}
import Graphics.UI.Gtk.Signals
{-# LINE 115 "./Graphics/UI/Gtk/ModelView/TreeSelection.chs" #-}
import Graphics.UI.Gtk.General.Enums (SelectionMode(..))
import Graphics.UI.Gtk.ModelView.TreeModel
{-# LINE 117 "./Graphics/UI/Gtk/ModelView/TreeSelection.chs" #-}
import Graphics.UI.Gtk.ModelView.Types
{-# LINE 118 "./Graphics/UI/Gtk/ModelView/TreeSelection.chs" #-}


{-# LINE 120 "./Graphics/UI/Gtk/ModelView/TreeSelection.chs" #-}

--------------------
-- Methods

-- | Set single or multiple choice.
--
treeSelectionSetMode :: TreeSelectionClass self => self
 -> SelectionMode
 -> IO ()
treeSelectionSetMode self type_ =
  (\(TreeSelection arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_selection_set_mode argPtr1 arg2)
{-# LINE 131 "./Graphics/UI/Gtk/ModelView/TreeSelection.chs" #-}
    (toTreeSelection self)
    ((fromIntegral . fromEnum) type_)

-- | Gets the selection mode.
--
treeSelectionGetMode :: TreeSelectionClass self => self
 -> IO SelectionMode
treeSelectionGetMode self =
  liftM (toEnum . fromIntegral) $
  (\(TreeSelection arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_selection_get_mode argPtr1)
{-# LINE 141 "./Graphics/UI/Gtk/ModelView/TreeSelection.chs" #-}
    (toTreeSelection self)

-- | Set a callback function if selection changes.
--
-- * If set, this function is called before any
-- node is selected or unselected, giving some control over which nodes are
-- selected. The select function should return @True@ if the state of the node
-- may be toggled, and @False@ if the state of the node should be left
-- unchanged.
treeSelectionSetSelectFunction :: TreeSelectionClass self => self
 -> TreeSelectionCB -> IO ()
treeSelectionSetSelectFunction ts fun = do
  fPtr <- mkTreeSelectionFunc (\_ _ tp _ _ -> do
    path <- peekTreePath (castPtr tp)
    liftM fromBool $ fun path
    )
  (\(TreeSelection arg1) arg2 arg3 arg4 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_selection_set_select_function argPtr1 arg2 arg3 arg4)
{-# LINE 158 "./Graphics/UI/Gtk/ModelView/TreeSelection.chs" #-}
    (toTreeSelection ts)
    fPtr
    (castFunPtrToPtr fPtr)
    destroyFunPtr

-- | Callback type for a function that is called everytime the selection
-- changes. This function is set with 'treeSelectionSetSelectFunction'.
--
type TreeSelectionCB = TreePath -> IO Bool
type TreeSelectionFunc = FunPtr (((Ptr TreeSelection) -> ((Ptr TreeModel) -> ((Ptr NativeTreePath) -> (CInt -> ((Ptr ()) -> (IO CInt)))))))
{-# LINE 168 "./Graphics/UI/Gtk/ModelView/TreeSelection.chs" #-}

foreign import ccall "wrapper" mkTreeSelectionFunc ::
  (Ptr TreeSelection -> Ptr TreeModel -> Ptr NativeTreePath -> (CInt) -> Ptr () -> IO CInt)->
  IO TreeSelectionFunc

-- | Retrieve the 'TreeView' widget that this 'TreeSelection' works on.
--
treeSelectionGetTreeView :: TreeSelectionClass self => self -> IO TreeView
treeSelectionGetTreeView self =
  makeNewObject mkTreeView $
  (\(TreeSelection arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_selection_get_tree_view argPtr1)
{-# LINE 179 "./Graphics/UI/Gtk/ModelView/TreeSelection.chs" #-}
    (toTreeSelection self)

-- | Retrieves the selection of a single choice 'TreeSelection'.
--
treeSelectionGetSelected :: TreeSelectionClass self => self ->
                            IO (Maybe TreeIter)
treeSelectionGetSelected self =
  receiveTreeIter $ \iterPtr ->
  (\(TreeSelection arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_selection_get_selected argPtr1 arg2 arg3)
{-# LINE 188 "./Graphics/UI/Gtk/ModelView/TreeSelection.chs" #-}
    (toTreeSelection self)
    nullPtr
    iterPtr

-- | Execute a function for each selected node.
--
-- * Note that you cannot modify the tree or selection from within this
-- function. Hence, 'treeSelectionGetSelectedRows' might be more useful.
--
treeSelectionSelectedForeach :: TreeSelectionClass self => self
 -> TreeSelectionForeachCB
 -> IO ()
treeSelectionSelectedForeach self fun = do
  fPtr <- mkTreeSelectionForeachFunc (\_ _ iterPtr _ -> do
    -- make a deep copy of the iterator. This makes it possible to store this
    -- iterator in Haskell land somewhere. The TreeModel parameter is not
    -- passed to the function due to performance reasons. But since it is
    -- a constant member of Selection this does not matter.
    iter <- peek iterPtr
    fun iter
    )
  (\(TreeSelection arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_selection_selected_foreach argPtr1 arg2 arg3)
{-# LINE 210 "./Graphics/UI/Gtk/ModelView/TreeSelection.chs" #-}
    (toTreeSelection self)
    fPtr
    nullPtr
  freeHaskellFunPtr fPtr

-- | Callback function type for 'treeSelectionSelectedForeach'.
--
type TreeSelectionForeachCB = TreeIter -> IO ()
type TreeSelectionForeachFunc = FunPtr (((Ptr TreeModel) -> ((Ptr NativeTreePath) -> ((Ptr TreeIter) -> ((Ptr ()) -> (IO ()))))))
{-# LINE 219 "./Graphics/UI/Gtk/ModelView/TreeSelection.chs" #-}

foreign import ccall "wrapper" mkTreeSelectionForeachFunc ::
  (Ptr TreeModel -> Ptr NativeTreePath -> Ptr TreeIter -> Ptr () -> IO ()) -> IO TreeSelectionForeachFunc


-- | Creates a list of paths of all selected rows.
--
-- * Additionally, if you are
-- planning on modifying the model after calling this function, you may want to
-- convert the returned list into a list of 'TreeRowReference's. To do this,
-- you can use 'treeRowReferenceNew'.
--
-- * Available since Gtk+ version 2.2
--
treeSelectionGetSelectedRows :: TreeSelectionClass self => self
 -> IO [TreePath] -- ^ returns a list containing a 'TreePath' for
                  -- each selected row.
treeSelectionGetSelectedRows self =
  (\(TreeSelection arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_selection_get_selected_rows argPtr1 arg2)
{-# LINE 238 "./Graphics/UI/Gtk/ModelView/TreeSelection.chs" #-}
    (toTreeSelection self)
    nullPtr
  >>= fromGList
  >>= mapM fromTreePath

-- | Returns the number of rows that are selected.
--
-- * Available since Gtk+ version 2.2
--
treeSelectionCountSelectedRows :: TreeSelectionClass self => self
 -> IO Int -- ^ returns The number of rows selected.
treeSelectionCountSelectedRows self =
  liftM fromIntegral $
  (\(TreeSelection arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_selection_count_selected_rows argPtr1)
{-# LINE 252 "./Graphics/UI/Gtk/ModelView/TreeSelection.chs" #-}
    (toTreeSelection self)


-- | Select a specific item by 'TreePath'.
--
treeSelectionSelectPath :: TreeSelectionClass self => self
 -> TreePath
 -> IO ()
treeSelectionSelectPath self [] = return ()
treeSelectionSelectPath self path =
  withTreePath path $ \path ->
  (\(TreeSelection arg1) (NativeTreePath arg2) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_selection_select_path argPtr1 arg2)
{-# LINE 264 "./Graphics/UI/Gtk/ModelView/TreeSelection.chs" #-}
    (toTreeSelection self)
    path

-- | Deselect a specific item by 'TreePath'.
--
treeSelectionUnselectPath :: TreeSelectionClass self => self
 -> TreePath
 -> IO ()
treeSelectionUnselectPath self path =
  withTreePath path $ \path ->
  (\(TreeSelection arg1) (NativeTreePath arg2) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_selection_unselect_path argPtr1 arg2)
{-# LINE 275 "./Graphics/UI/Gtk/ModelView/TreeSelection.chs" #-}
    (toTreeSelection self)
    path

-- | Returns True if the row at the given path is currently selected.
--
treeSelectionPathIsSelected :: TreeSelectionClass self => self
 -> TreePath -> IO Bool
treeSelectionPathIsSelected self path =
  liftM toBool $
  withTreePath path $ \path ->
  (\(TreeSelection arg1) (NativeTreePath arg2) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_selection_path_is_selected argPtr1 arg2)
{-# LINE 286 "./Graphics/UI/Gtk/ModelView/TreeSelection.chs" #-}
    (toTreeSelection self)
    path

-- | Select a specific item by 'TreeIter'.
--
treeSelectionSelectIter :: TreeSelectionClass self => self -> TreeIter -> IO ()
treeSelectionSelectIter self iter =
  with iter $ \iterPtr ->
  (\(TreeSelection arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_selection_select_iter argPtr1 arg2)
{-# LINE 295 "./Graphics/UI/Gtk/ModelView/TreeSelection.chs" #-}
    (toTreeSelection self)
    iterPtr

-- | Deselect a specific item by 'TreeIter'.
--
treeSelectionUnselectIter :: TreeSelectionClass self => self -> TreeIter -> IO ()
treeSelectionUnselectIter self iter =
  with iter $ \iterPtr ->
  (\(TreeSelection arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_selection_unselect_iter argPtr1 arg2)
{-# LINE 304 "./Graphics/UI/Gtk/ModelView/TreeSelection.chs" #-}
    (toTreeSelection self)
    iterPtr

-- | Returns True if the row at the given iter is currently selected.
--
treeSelectionIterIsSelected :: TreeSelectionClass self => self
 -> TreeIter
 -> IO Bool
treeSelectionIterIsSelected self iter =
  liftM toBool $
  with iter $ \iterPtr ->
  (\(TreeSelection arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_selection_iter_is_selected argPtr1 arg2)
{-# LINE 316 "./Graphics/UI/Gtk/ModelView/TreeSelection.chs" #-}
    (toTreeSelection self)
    iterPtr

-- | Selects all the nodes. The tree selection must be set to
-- 'SelectionMultiple' mode.
--
treeSelectionSelectAll :: TreeSelectionClass self => self -> IO ()
treeSelectionSelectAll self =
  (\(TreeSelection arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_selection_select_all argPtr1)
{-# LINE 325 "./Graphics/UI/Gtk/ModelView/TreeSelection.chs" #-}
    (toTreeSelection self)

-- | Unselects all the nodes.
--
treeSelectionUnselectAll :: TreeSelectionClass self => self -> IO ()
treeSelectionUnselectAll self =
  (\(TreeSelection arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_selection_unselect_all argPtr1)
{-# LINE 332 "./Graphics/UI/Gtk/ModelView/TreeSelection.chs" #-}
    (toTreeSelection self)

-- | Selects a range of nodes, determined by @startPath@ and @endPath@
-- inclusive. @selection@ must be set to 'SelectionMultiple' mode.
--
treeSelectionSelectRange :: TreeSelectionClass self => self
 -> TreePath -- ^ @startPath@ - The initial node of the range.
 -> TreePath -- ^ @endPath@ - The final node of the range.
 -> IO ()
treeSelectionSelectRange self startPath endPath =
  withTreePath endPath $ \endPath ->
  withTreePath startPath $ \startPath ->
  (\(TreeSelection arg1) (NativeTreePath arg2) (NativeTreePath arg3) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_selection_select_range argPtr1 arg2 arg3)
{-# LINE 345 "./Graphics/UI/Gtk/ModelView/TreeSelection.chs" #-}
    (toTreeSelection self)
    startPath
    endPath


-- | Unselects a range of nodes, determined by @startPath@ and @endPath@
-- inclusive.
--
-- * Available since Gtk+ version 2.2
--
treeSelectionUnselectRange :: TreeSelectionClass self => self
 -> TreePath -- ^ @startPath@ - The initial node of the range.
 -> TreePath -- ^ @endPath@ - The initial node of the range.
 -> IO ()
treeSelectionUnselectRange self startPath endPath =
  withTreePath endPath $ \endPath ->
  withTreePath startPath $ \startPath ->
  (\(TreeSelection arg1) (NativeTreePath arg2) (NativeTreePath arg3) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_selection_unselect_range argPtr1 arg2 arg3)
{-# LINE 363 "./Graphics/UI/Gtk/ModelView/TreeSelection.chs" #-}
    (toTreeSelection self)
    startPath
    endPath


--------------------
-- Attributes

-- | \'mode\' property. See 'treeSelectionGetMode' and 'treeSelectionSetMode'
--
treeSelectionMode :: TreeSelectionClass self => Attr self SelectionMode
treeSelectionMode = newAttr
  treeSelectionGetMode
  treeSelectionSetMode

--------------------
-- Signals

-- | Emitted whenever the selection has (possibly) changed. Please note that
-- this signal is mostly a hint. It may only be emitted once when a range of
-- rows are selected, and it may occasionally be emitted when nothing has
-- happened.
--
treeSelectionSelectionChanged :: TreeSelectionClass self => Signal self (IO ())
treeSelectionSelectionChanged = Signal (connect_NONE__NONE "changed")


--------------------
-- Deprecated Signals

onSelectionChanged, afterSelectionChanged :: TreeSelectionClass self => self
 -> IO ()
 -> IO (ConnectId self)
onSelectionChanged = connect_NONE__NONE "changed" False
afterSelectionChanged = connect_NONE__NONE "changed" True

foreign import ccall safe "gtk_tree_selection_set_mode"
  gtk_tree_selection_set_mode :: ((Ptr TreeSelection) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_tree_selection_get_mode"
  gtk_tree_selection_get_mode :: ((Ptr TreeSelection) -> (IO CInt))

foreign import ccall safe "gtk_tree_selection_set_select_function"
  gtk_tree_selection_set_select_function :: ((Ptr TreeSelection) -> ((FunPtr ((Ptr TreeSelection) -> ((Ptr TreeModel) -> ((Ptr NativeTreePath) -> (CInt -> ((Ptr ()) -> (IO CInt))))))) -> ((Ptr ()) -> ((FunPtr ((Ptr ()) -> (IO ()))) -> (IO ())))))

foreign import ccall unsafe "gtk_tree_selection_get_tree_view"
  gtk_tree_selection_get_tree_view :: ((Ptr TreeSelection) -> (IO (Ptr TreeView)))

foreign import ccall safe "gtk_tree_selection_get_selected"
  gtk_tree_selection_get_selected :: ((Ptr TreeSelection) -> ((Ptr TreeModel) -> ((Ptr TreeIter) -> (IO CInt))))

foreign import ccall safe "gtk_tree_selection_selected_foreach"
  gtk_tree_selection_selected_foreach :: ((Ptr TreeSelection) -> ((FunPtr ((Ptr TreeModel) -> ((Ptr NativeTreePath) -> ((Ptr TreeIter) -> ((Ptr ()) -> (IO ())))))) -> ((Ptr ()) -> (IO ()))))

foreign import ccall safe "gtk_tree_selection_get_selected_rows"
  gtk_tree_selection_get_selected_rows :: ((Ptr TreeSelection) -> ((Ptr TreeModel) -> (IO (Ptr ()))))

foreign import ccall safe "gtk_tree_selection_count_selected_rows"
  gtk_tree_selection_count_selected_rows :: ((Ptr TreeSelection) -> (IO CInt))

foreign import ccall safe "gtk_tree_selection_select_path"
  gtk_tree_selection_select_path :: ((Ptr TreeSelection) -> ((Ptr NativeTreePath) -> (IO ())))

foreign import ccall safe "gtk_tree_selection_unselect_path"
  gtk_tree_selection_unselect_path :: ((Ptr TreeSelection) -> ((Ptr NativeTreePath) -> (IO ())))

foreign import ccall safe "gtk_tree_selection_path_is_selected"
  gtk_tree_selection_path_is_selected :: ((Ptr TreeSelection) -> ((Ptr NativeTreePath) -> (IO CInt)))

foreign import ccall safe "gtk_tree_selection_select_iter"
  gtk_tree_selection_select_iter :: ((Ptr TreeSelection) -> ((Ptr TreeIter) -> (IO ())))

foreign import ccall safe "gtk_tree_selection_unselect_iter"
  gtk_tree_selection_unselect_iter :: ((Ptr TreeSelection) -> ((Ptr TreeIter) -> (IO ())))

foreign import ccall safe "gtk_tree_selection_iter_is_selected"
  gtk_tree_selection_iter_is_selected :: ((Ptr TreeSelection) -> ((Ptr TreeIter) -> (IO CInt)))

foreign import ccall safe "gtk_tree_selection_select_all"
  gtk_tree_selection_select_all :: ((Ptr TreeSelection) -> (IO ()))

foreign import ccall safe "gtk_tree_selection_unselect_all"
  gtk_tree_selection_unselect_all :: ((Ptr TreeSelection) -> (IO ()))

foreign import ccall safe "gtk_tree_selection_select_range"
  gtk_tree_selection_select_range :: ((Ptr TreeSelection) -> ((Ptr NativeTreePath) -> ((Ptr NativeTreePath) -> (IO ()))))

foreign import ccall safe "gtk_tree_selection_unselect_range"
  gtk_tree_selection_unselect_range :: ((Ptr TreeSelection) -> ((Ptr NativeTreePath) -> ((Ptr NativeTreePath) -> (IO ()))))
