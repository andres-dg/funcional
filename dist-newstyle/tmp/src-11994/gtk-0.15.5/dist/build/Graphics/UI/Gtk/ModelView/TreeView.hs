
{-# LINE 2 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget TreeView
--
-- Author : Axel Simon
--
-- Created: 9 May 2001
--
-- Copyright (C) 2001-2005 Axel Simon
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
-- gtk_tree_view_get_bin_window is to compare the GDK window from incoming
-- events. We don't marshal that window parameter, so this function is not
-- bound either.
--
-- The following functions related to drag and drop:
-- treeViewSetDragDestRow, treeViewGetDragDestRow, treeViewGetDestRowAtPos
-- these seem to be useful only in cases when the user wants to implement
-- drag and drop himself rather than use the widget's implementation. I
-- think this would be a bad idea in the first place.
--
-- get_search_equal_func is missing: proper memory management is impossible
--
-- gtk_tree_view_set_destroy_count_func is not meant to be useful
--
-- expand-collapse-cursor-row needs to be bound if it is useful to expand
-- and collapse rows in a user-defined manner. Would only work on Gtk 2.2
-- and higher since the return parameter changed
--
-- move_cursor, select_all, select_cursor_parent, select_cursor_row
-- toggle_cursor_row, unselect_all are not bound.
-- These functions are only useful to change the widgets
-- behaviour for these actions. Everything else can be done with
-- cursor_changed and columns_changed
--
-- set_scroll_adjustment makes sense if the user monitors the scroll bars
-- and the scroll bars can be replaced anytime (the latter is odd)
--
-- |
-- Maintainer : gtk2hs-users@lists.sourceforge.net
-- Stability : provisional
-- Portability : portable (depends on GHC)
--
-- A widget for displaying both trees and lists.
--
module Graphics.UI.Gtk.ModelView.TreeView (
-- * Description
--
-- | Widget that displays any object that implements the 'TreeModel'
-- interface.
--
-- The widget supports scrolling natively. This implies that pixel
-- coordinates can be given in two formats: relative to the current view's
-- upper left corner or relative to the whole list's coordinates. The former
-- are called widget coordinates while the letter are called tree
-- coordinates.

-- * Class Hierarchy
-- |
-- @
-- | 'GObject'
-- | +----'Object'
-- | +----'Widget'
-- | +----'Container'
-- | +----TreeView
-- @

-- * Types
  TreeView,
  TreeViewClass,
  castToTreeView, gTypeTreeView,
  toTreeView,
  Point,
  DragAction(..),

  TreeViewGridLines(..),


-- * Constructors
  treeViewNew,
  treeViewNewWithModel,

-- * Methods
  treeViewGetModel,
  treeViewSetModel,
  treeViewGetSelection,
  treeViewGetHAdjustment,
  treeViewSetHAdjustment,
  treeViewGetVAdjustment,
  treeViewSetVAdjustment,
  treeViewGetHeadersVisible,
  treeViewSetHeadersVisible,
  treeViewColumnsAutosize,
  treeViewSetHeadersClickable,
  treeViewGetRulesHint,
  treeViewSetRulesHint,
  treeViewAppendColumn,
  treeViewRemoveColumn,
  treeViewInsertColumn,
  treeViewGetColumn,
  treeViewGetColumns,
  treeViewMoveColumnAfter,
  treeViewMoveColumnFirst,
  treeViewSetExpanderColumn,
  treeViewGetExpanderColumn,
  treeViewSetColumnDragFunction,
  treeViewScrollToPoint,
  treeViewScrollToCell,
  treeViewSetCursor,

  treeViewSetCursorOnCell,

  treeViewGetCursor,
  treeViewRowActivated,
  treeViewExpandAll,
  treeViewCollapseAll,

  treeViewExpandToPath,

  treeViewExpandRow,
  treeViewCollapseRow,
  treeViewMapExpandedRows,
  treeViewRowExpanded,
  treeViewGetReorderable,
  treeViewSetReorderable,
  treeViewGetPathAtPos,
  treeViewGetCellArea,
  treeViewGetBackgroundArea,
  treeViewGetVisibleRect,

  treeViewConvertBinWindowToTreeCoords,
  treeViewConvertBinWindowToWidgetCoords,
  treeViewConvertTreeToBinWindowCoords,
  treeViewConvertTreeToWidgetCoords,
  treeViewConvertWidgetToBinWindowCoords,
  treeViewConvertWidgetToTreeCoords,


  treeViewCreateRowDragIcon,

  treeViewGetEnableSearch,
  treeViewSetEnableSearch,
  treeViewGetSearchColumn,
  treeViewSetSearchColumn,
  treeViewSetSearchEqualFunc,

  treeViewGetFixedHeightMode,
  treeViewSetFixedHeightMode,
  treeViewGetHoverSelection,
  treeViewSetHoverSelection,
  treeViewGetHoverExpand,
  treeViewSetHoverExpand,

  treeViewGetHeadersClickable,



  treeViewGetVisibleRange,


  treeViewEnableModelDragDest,
  treeViewEnableModelDragSource,
  treeViewUnsetRowsDragSource,
  treeViewUnsetRowsDragDest,
  treeViewGetSearchEntry,
  treeViewSetSearchEntry,


  treeViewSetRowSeparatorFunc,

  treeViewGetRubberBanding,
  treeViewSetRubberBanding,
  treeViewGetEnableTreeLines,
  treeViewSetEnableTreeLines,
  treeViewGetGridLines,
  treeViewSetGridLines,



  treeViewSetTooltipRow,
  treeViewSetTooltipCell,
  treeViewGetTooltipContext,


-- * Attributes
  treeViewModel,
  treeViewHAdjustment,
  treeViewVAdjustment,
  treeViewHeadersVisible,
  treeViewHeadersClickable,
  treeViewExpanderColumn,
  treeViewReorderable,
  treeViewRulesHint,
  treeViewEnableSearch,
  treeViewSearchColumn,

  treeViewFixedHeightMode,

  treeViewHoverSelection,
  treeViewHoverExpand,


  treeViewShowExpanders,
  treeViewLevelIndentation,
  treeViewRubberBanding,

  treeViewEnableGridLines,

  treeViewEnableTreeLines,

  treeViewGridLines,
  treeViewSearchEntry,


  treeViewTooltipColumn,


-- * Signals
  columnsChanged,
  cursorChanged,
  rowCollapsed,
  rowExpanded,
  rowActivated,
  testCollapseRow,
  testExpandRow,

-- * Deprecated


  treeViewWidgetToTreeCoords,
  treeViewTreeToWidgetCoords,

  onColumnsChanged,
  afterColumnsChanged,
  onCursorChanged,
  afterCursorChanged,
  onRowActivated,
  afterRowActivated,
  onRowCollapsed,
  afterRowCollapsed,
  onRowExpanded,
  afterRowExpanded,
  onStartInteractiveSearch,
  afterStartInteractiveSearch,
  onTestCollapseRow,
  afterTestCollapseRow,
  onTestExpandRow,
  afterTestExpandRow

  ) where

import Control.Monad (liftM,)
import Data.Maybe (fromMaybe)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.GList (fromGList)
import System.Glib.Flags
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Gdk.Enums (DragAction(..))
import Graphics.UI.Gtk.Gdk.Events (Modifier(..))
import Graphics.UI.Gtk.General.Structs (Point, Rectangle)
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.UI.Gtk.Types
{-# LINE 277 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
import Graphics.UI.Gtk.Signals
{-# LINE 278 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
import Graphics.UI.Gtk.ModelView.TreeModel (columnIdToNumber,
                                            makeColumnIdString)
import Graphics.UI.Gtk.ModelView.Types
{-# LINE 281 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
import Graphics.UI.Gtk.General.DNDTypes (TargetList(..))


{-# LINE 284 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}

--------------------
-- Constructors

-- | Creates a new 'TreeView' widget.
--
treeViewNew :: IO TreeView
treeViewNew =
  makeNewObject mkTreeView $
  liftM (castPtr :: Ptr Widget -> Ptr TreeView) $
  gtk_tree_view_new
{-# LINE 295 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}

-- | Create a new 'TreeView'
-- widget with @model@ as the storage model.
--
treeViewNewWithModel :: TreeModelClass model => model -> IO TreeView
treeViewNewWithModel model =
  makeNewObject mkTreeView $
  liftM (castPtr :: Ptr Widget -> Ptr TreeView) $
  (\(TreeModel arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_new_with_model argPtr1)
{-# LINE 304 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeModel model)

--------------------
-- Methods

-- | Returns the model that supplies the data for
-- this 'TreeView'. Returns @Nothing@ if the model is unset.
--
treeViewGetModel :: TreeViewClass self => self -> IO (Maybe TreeModel)
treeViewGetModel self =
  maybeNull (makeNewGObject mkTreeModel) $
  (\(TreeView arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_get_model argPtr1)
{-# LINE 316 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)

-- | Set the 'TreeModel' for the current View.
--
treeViewSetModel :: (TreeViewClass self, TreeModelClass model) => self
 -> Maybe model
 -> IO ()
treeViewSetModel self model =
  (\(TreeView arg1) (TreeModel arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_tree_view_set_model argPtr1 argPtr2)
{-# LINE 325 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
    (maybe (TreeModel nullForeignPtr) toTreeModel model)

-- | Retrieve a 'TreeSelection' that
-- holds the current selected nodes of the View.
--
treeViewGetSelection :: TreeViewClass self => self -> IO TreeSelection
treeViewGetSelection self =
  makeNewGObject mkTreeSelection $
  (\(TreeView arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_get_selection argPtr1)
{-# LINE 335 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)

-- | Gets the 'Adjustment' currently being used for the horizontal aspect.
--
treeViewGetHAdjustment :: TreeViewClass self => self -> IO (Maybe Adjustment)
treeViewGetHAdjustment self =
  maybeNull (makeNewObject mkAdjustment) $
  (\(TreeView arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_get_hadjustment argPtr1)
{-# LINE 343 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)

-- | Sets the 'Adjustment' for the current horizontal aspect.
--
treeViewSetHAdjustment :: TreeViewClass self => self
 -> Maybe Adjustment -- ^ @adjustment@ - The 'Adjustment' to set, or @Nothing@
 -> IO ()
treeViewSetHAdjustment self adjustment =
  (\(TreeView arg1) (Adjustment arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_tree_view_set_hadjustment argPtr1 argPtr2)
{-# LINE 352 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
    (fromMaybe (Adjustment nullForeignPtr) adjustment)

-- | Gets the 'Adjustment' currently being used for the vertical aspect.
--
treeViewGetVAdjustment :: TreeViewClass self => self -> IO (Maybe Adjustment)
treeViewGetVAdjustment self =
  maybeNull (makeNewObject mkAdjustment) $
  (\(TreeView arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_get_vadjustment argPtr1)
{-# LINE 361 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)

-- | Sets the 'Adjustment' for the current vertical aspect.
--
treeViewSetVAdjustment :: TreeViewClass self => self
 -> Maybe Adjustment -- ^ @adjustment@ - The 'Adjustment' to set, or @Nothing@
 -> IO ()
treeViewSetVAdjustment self adjustment =
  (\(TreeView arg1) (Adjustment arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_tree_view_set_vadjustment argPtr1 argPtr2)
{-# LINE 370 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
    (fromMaybe (Adjustment nullForeignPtr) adjustment)

-- | Query if the column headers are visible.
--
treeViewGetHeadersVisible :: TreeViewClass self => self -> IO Bool
treeViewGetHeadersVisible self =
  liftM toBool $
  (\(TreeView arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_get_headers_visible argPtr1)
{-# LINE 379 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)

-- | Set the visibility state of the column headers.
--
treeViewSetHeadersVisible :: TreeViewClass self => self -> Bool -> IO ()
treeViewSetHeadersVisible self headersVisible =
  (\(TreeView arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_set_headers_visible argPtr1 arg2)
{-# LINE 386 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
    (fromBool headersVisible)

-- | Resize the columns to their optimal size.
--
treeViewColumnsAutosize :: TreeViewClass self => self -> IO ()
treeViewColumnsAutosize self =
  (\(TreeView arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_columns_autosize argPtr1)
{-# LINE 394 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)

-- | Set wether the columns headers are sensitive to mouse clicks.
--
treeViewSetHeadersClickable :: TreeViewClass self => self -> Bool -> IO ()
treeViewSetHeadersClickable self setting =
  (\(TreeView arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_set_headers_clickable argPtr1 arg2)
{-# LINE 401 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
    (fromBool setting)

-- | Query if visual aid for wide columns is turned on.
--
treeViewGetRulesHint :: TreeViewClass self => self -> IO Bool
treeViewGetRulesHint self =
  liftM toBool $
  (\(TreeView arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_get_rules_hint argPtr1)
{-# LINE 410 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)

-- | This function tells Gtk+ that the user interface for your application
-- requires users to read across tree rows and associate cells with one
-- another. By default, Gtk+ will then render the tree with alternating row
-- colors. Do /not/ use it just because you prefer the appearance of the ruled
-- tree; that's a question for the theme. Some themes will draw tree rows in
-- alternating colors even when rules are turned off, and users who prefer that
-- appearance all the time can choose those themes. You should call this
-- function only as a /semantic/ hint to the theme engine that your tree makes
-- alternating colors useful from a functional standpoint (since it has lots of
-- columns, generally).
--
treeViewSetRulesHint :: TreeViewClass self => self -> Bool -> IO ()
treeViewSetRulesHint self setting =
  (\(TreeView arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_set_rules_hint argPtr1 arg2)
{-# LINE 426 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
    (fromBool setting)

-- | Append a new column to the 'TreeView'. Returns the new number of columns.
--
treeViewAppendColumn :: TreeViewClass self => self -> TreeViewColumn -> IO Int
treeViewAppendColumn self column =
  liftM fromIntegral $
  (\(TreeView arg1) (TreeViewColumn arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_tree_view_append_column argPtr1 argPtr2)
{-# LINE 435 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
    column

-- | Remove column @tvc@ from the 'TreeView'
-- widget. The number of remaining columns is returned.
--
treeViewRemoveColumn :: TreeViewClass self => self -> TreeViewColumn -> IO Int
treeViewRemoveColumn self column =
  liftM fromIntegral $
  (\(TreeView arg1) (TreeViewColumn arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_tree_view_remove_column argPtr1 argPtr2)
{-# LINE 445 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
    column

-- | Inserts column @tvc@ into the
-- 'TreeView' widget at the position @pos@. Returns the number of
-- columns after insertion. Specify -1 for @pos@ to insert the column
-- at the end.
--
treeViewInsertColumn :: TreeViewClass self => self
 -> TreeViewColumn
 -> Int
 -> IO Int
treeViewInsertColumn self column position =
  liftM fromIntegral $
  (\(TreeView arg1) (TreeViewColumn arg2) arg3 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_tree_view_insert_column argPtr1 argPtr2 arg3)
{-# LINE 460 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
    column
    (fromIntegral position)

-- | Retrieve a 'TreeViewColumn'.
--
-- * Retrieve the @pos@ th columns of
-- 'TreeView'. If the index is out of range Nothing is returned.
--
treeViewGetColumn :: TreeViewClass self => self -> Int -> IO (Maybe TreeViewColumn)
treeViewGetColumn self pos = do
  tvcPtr <- (\(TreeView arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_get_column argPtr1 arg2) (toTreeView self)
    (fromIntegral pos)
  if tvcPtr==nullPtr then return Nothing else
    liftM Just $ makeNewObject mkTreeViewColumn (return tvcPtr)

-- | Return all 'TreeViewColumn's in this 'TreeView'.
--
treeViewGetColumns :: TreeViewClass self => self -> IO [TreeViewColumn]
treeViewGetColumns self = do
  colsList <- (\(TreeView arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_get_columns argPtr1) (toTreeView self)
  colsPtr <- fromGList colsList
  mapM (makeNewObject mkTreeViewColumn) (map return colsPtr)

-- | Move a specific column.
--
-- * Use 'treeViewMoveColumnToFront' if you want to move the column
-- to the left end of the 'TreeView'.
--
treeViewMoveColumnAfter :: TreeViewClass self => self
 -> TreeViewColumn
 -> TreeViewColumn
 -> IO ()
treeViewMoveColumnAfter self column baseColumn =
  (\(TreeView arg1) (TreeViewColumn arg2) (TreeViewColumn arg3) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->withForeignPtr arg3 $ \argPtr3 ->gtk_tree_view_move_column_after argPtr1 argPtr2 argPtr3)
{-# LINE 495 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
    column
    baseColumn

-- | Move a specific column.
--
-- * Use 'treeViewMoveColumnAfter' if you want to move the column
-- somewhere else than to the leftmost position.
--
treeViewMoveColumnFirst :: TreeViewClass self => self -> TreeViewColumn -> IO ()
treeViewMoveColumnFirst self which =
  (\(TreeView arg1) (TreeViewColumn arg2) (TreeViewColumn arg3) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->withForeignPtr arg3 $ \argPtr3 ->gtk_tree_view_move_column_after argPtr1 argPtr2 argPtr3)
{-# LINE 507 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
    which
    (TreeViewColumn nullForeignPtr)

-- | Set location of hierarchy controls.
--
-- * Sets the column to draw the expander arrow at. If @col@
-- is @Nothing@, then the expander arrow is always at the first
-- visible column.
--
-- If you do not want expander arrow to appear in your tree, set the
-- expander column to a hidden column.
--
treeViewSetExpanderColumn :: TreeViewClass self => self
 -> Maybe TreeViewColumn
 -> IO ()
treeViewSetExpanderColumn self column =
  (\(TreeView arg1) (TreeViewColumn arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_tree_view_set_expander_column argPtr1 argPtr2)
{-# LINE 525 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
    (fromMaybe (TreeViewColumn nullForeignPtr) column)

-- | Get location of hierarchy controls.
--
-- * Gets the column to draw the expander arrow at. If @col@
-- is @Nothing@, then the expander arrow is always at the first
-- visible column.
--
treeViewGetExpanderColumn :: TreeViewClass self => self
 -> IO TreeViewColumn
treeViewGetExpanderColumn self =
  makeNewObject mkTreeViewColumn $
  (\(TreeView arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_get_expander_column argPtr1)
{-# LINE 539 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)

-- | Specify where a column may be dropped.
--
-- * Sets a user function for determining where a column may be dropped when
-- dragged. This function is called on every column pair in turn at the
-- beginning of a column drag to determine where a drop can take place.
--
-- * The callback function take the 'TreeViewColumn' to be moved, the
-- second and third arguments are the columns on the left and right side
-- of the new location. At most one of them might be @Nothing@
-- which indicates that the column is about to be dropped at the left or
-- right end of the 'TreeView'.
--
-- * The predicate @pred@ should return @True@ if it is ok
-- to insert the column at this place.
--
-- * Use @Nothing@ for the predicate if columns can be inserted
-- anywhere.
--
treeViewSetColumnDragFunction :: TreeViewClass self => self
 -> Maybe (TreeViewColumn
        -> Maybe TreeViewColumn
        -> Maybe TreeViewColumn
        -> IO Bool)
 -> IO ()
treeViewSetColumnDragFunction self Nothing =
  (\(TreeView arg1) arg2 arg3 arg4 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_set_column_drag_function argPtr1 arg2 arg3 arg4) (toTreeView self)
    nullFunPtr nullPtr nullFunPtr
treeViewSetColumnDragFunction self (Just pred) = do
  fPtr <- mkTreeViewColumnDropFunc $ \_ target prev next _ -> do
    target' <- makeNewObject mkTreeViewColumn (return target)
    prev' <- if prev==nullPtr then return Nothing else liftM Just $
      makeNewObject mkTreeViewColumn (return prev)
    next' <- if next==nullPtr then return Nothing else liftM Just $
      makeNewObject mkTreeViewColumn (return next)
    res <- pred target' prev' next'
    return (fromBool res)
  (\(TreeView arg1) arg2 arg3 arg4 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_set_column_drag_function argPtr1 arg2 arg3 arg4)
{-# LINE 578 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
    fPtr
    (castFunPtrToPtr fPtr) destroyFunPtr

type TreeViewColumnDropFunc = FunPtr (((Ptr TreeView) -> ((Ptr TreeViewColumn) -> ((Ptr TreeViewColumn) -> ((Ptr TreeViewColumn) -> ((Ptr ()) -> (IO CInt)))))))
{-# LINE 583 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}

foreign import ccall "wrapper" mkTreeViewColumnDropFunc ::
  (Ptr TreeView -> Ptr TreeViewColumn -> Ptr TreeViewColumn -> Ptr TreeViewColumn ->
  Ptr () -> IO (CInt)) -> IO TreeViewColumnDropFunc

-- | Scroll to a coordinate.
--
-- * Scrolls the tree view such that the top-left corner of the
-- visible area is @treeX@, @treeY@, where @treeX@
-- and @treeY@ are specified in tree window coordinates.
-- The 'TreeView' must be realized before this function is
-- called. If it isn't, you probably want to use
-- 'treeViewScrollToCell'.
--
treeViewScrollToPoint :: TreeViewClass self => self
 -> Int
 -> Int
 -> IO ()
treeViewScrollToPoint self treeX treeY =
  (\(TreeView arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_scroll_to_point argPtr1 arg2 arg3)
{-# LINE 603 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
    (fromIntegral treeX)
    (fromIntegral treeY)

-- | Scroll to a cell.
--
-- Moves the alignments of tree_view to the position specified by mbColumn and mbPath.
-- If mbColumn is Nothing, then no horizontal scrolling occurs. Likewise, if mbPath
-- is Nothing no vertical scrolling occurs. At a minimum, one of mbColumn or mbPath
-- need to be provided. @rowAlign@ determines where the row is placed, and
-- @colAlign@ determines where column is placed. Both are expected to be between
-- 0.0 and 1.0. 0.0 means left/top alignment, 1.0 means right/bottom alignment,
-- 0.5 means center.
--
-- If Nothing is passed instead of @rowAlign@ and @colAlign@, then the tree does
-- the minimum amount of work to scroll the cell onto the screen. This means
-- that the cell will be scrolled to the edge closest to its current position.
-- If the cell is currently visible on the screen, nothing is done.
--
-- This function only works if the model is set, and path is a valid row on
-- the model. If the model changes before the tree_view is realized, the
-- centered path will be modified to reflect this change.
--
treeViewScrollToCell :: TreeViewClass self => self
 -> Maybe TreePath
 -> Maybe TreeViewColumn
 -> Maybe (Float, Float)
 -> IO ()
treeViewScrollToCell self mbPath mbColumn (Just (rowAlign, colAlign)) =
  maybeWithTreePath mbPath $ \path ->
  (\(TreeView arg1) (NativeTreePath arg2) (TreeViewColumn arg3) arg4 arg5 arg6 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg3 $ \argPtr3 ->gtk_tree_view_scroll_to_cell argPtr1 arg2 argPtr3 arg4 arg5 arg6)
{-# LINE 634 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
    path
    (maybe (TreeViewColumn nullForeignPtr) toTreeViewColumn mbColumn)
    1
    (realToFrac rowAlign)
    (realToFrac colAlign)
treeViewScrollToCell self mbPath mbColumn Nothing =
  maybeWithTreePath mbPath $ \path ->
  (\(TreeView arg1) (NativeTreePath arg2) (TreeViewColumn arg3) arg4 arg5 arg6 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg3 $ \argPtr3 ->gtk_tree_view_scroll_to_cell argPtr1 arg2 argPtr3 arg4 arg5 arg6)
{-# LINE 643 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
    path
    (maybe (TreeViewColumn nullForeignPtr) toTreeViewColumn mbColumn)
    0
    0.0
    0.0

-- | Selects a specific row.
--
-- * Sets the current keyboard focus to be at @path@, and
-- selects it. This is useful when you want to focus the user\'s
-- attention on a particular row. If @focusColumn@ is given,
-- then the input focus is given to the column specified by
-- it. Additionally, if @focusColumn@ is specified, and
-- @startEditing@ is @True@,
-- then editing will be started in the
-- specified cell. This function is often followed by a
-- 'widgetGrabFocus' to the 'TreeView' in order
-- to give keyboard focus to the widget.
--
treeViewSetCursor :: TreeViewClass self => self
 -> TreePath
 -> (Maybe (TreeViewColumn, Bool))
 -> IO ()
treeViewSetCursor self path Nothing =
  withTreePath path $ \path ->
  (\(TreeView arg1) (NativeTreePath arg2) (TreeViewColumn arg3) arg4 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg3 $ \argPtr3 ->gtk_tree_view_set_cursor argPtr1 arg2 argPtr3 arg4)
{-# LINE 670 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
    path
    (TreeViewColumn nullForeignPtr)
    (fromBool False)
treeViewSetCursor self path (Just (focusColumn, startEditing)) =
  withTreePath path $ \path ->
  (\(TreeView arg1) (NativeTreePath arg2) (TreeViewColumn arg3) arg4 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg3 $ \argPtr3 ->gtk_tree_view_set_cursor argPtr1 arg2 argPtr3 arg4)
{-# LINE 677 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
    path
    focusColumn
    (fromBool startEditing)


-- | Selects a cell in a specific row.
--
-- * Similar to 'treeViewSetCursor' but allows a column to
-- containt several 'CellRenderer's.
--
-- * Only available in Gtk 2.2 and higher.
--
treeViewSetCursorOnCell :: (TreeViewClass self, CellRendererClass focusCell) => self
 -> TreePath
 -> TreeViewColumn
 -> focusCell
 -> Bool
 -> IO ()
treeViewSetCursorOnCell self path focusColumn focusCell startEditing =
  withTreePath path $ \path ->
  (\(TreeView arg1) (NativeTreePath arg2) (TreeViewColumn arg3) (CellRenderer arg4) arg5 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg3 $ \argPtr3 ->withForeignPtr arg4 $ \argPtr4 ->gtk_tree_view_set_cursor_on_cell argPtr1 arg2 argPtr3 argPtr4 arg5)
{-# LINE 699 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
    path
    focusColumn
    (toCellRenderer focusCell)
    (fromBool startEditing)


-- | Retrieves the position of the focus.
--
-- * Returns a pair @(path, column)@.If the cursor is not currently
-- set, @path@ will be @[]@. If no column is currently
-- selected, @column@ will be @Nothing@.
--
treeViewGetCursor :: TreeViewClass self => self
 -> IO (TreePath, Maybe TreeViewColumn)
treeViewGetCursor self =
  alloca $ \tpPtrPtr -> alloca $ \tvcPtrPtr -> do
  (\(TreeView arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_get_cursor argPtr1 arg2 arg3)
{-# LINE 717 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
    (castPtr tpPtrPtr)
    (castPtr tvcPtrPtr)
  tpPtr <- peek tpPtrPtr
  tvcPtr <- peek tvcPtrPtr
  tp <- fromTreePath tpPtr
  tvc <- if tvcPtr==nullPtr then return Nothing else liftM Just $
    makeNewObject mkTreeViewColumn (return tvcPtr)
  return (tp,tvc)

-- | Emit the activated signal on a cell.
--
treeViewRowActivated :: TreeViewClass self => self
 -> TreePath
 -> TreeViewColumn
 -> IO ()
treeViewRowActivated self path column =
  withTreePath path $ \path ->
  (\(TreeView arg1) (NativeTreePath arg2) (TreeViewColumn arg3) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg3 $ \argPtr3 ->gtk_tree_view_row_activated argPtr1 arg2 argPtr3)
{-# LINE 736 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
    path
    column

-- | Recursively expands all nodes in the tree view.
--
treeViewExpandAll :: TreeViewClass self => self -> IO ()
treeViewExpandAll self =
  (\(TreeView arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_expand_all argPtr1)
{-# LINE 745 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)

-- | Recursively collapses all visible, expanded nodes in the tree view.
--
treeViewCollapseAll :: TreeViewClass self => self -> IO ()
treeViewCollapseAll self =
  (\(TreeView arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_collapse_all argPtr1)
{-# LINE 752 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)


-- | Make a certain path visible.
--
-- * This will expand all parent rows of @tp@ as necessary.
--
-- * Only available in Gtk 2.2 and higher.
--
treeViewExpandToPath :: TreeViewClass self => self -> TreePath -> IO ()
treeViewExpandToPath self path =
  withTreePath path $ \path ->
  (\(TreeView arg1) (NativeTreePath arg2) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_expand_to_path argPtr1 arg2)
{-# LINE 765 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
    path


-- | Opens the row so its children are visible.
--
treeViewExpandRow :: TreeViewClass self => self
 -> TreePath -- ^ @path@ - path to a row
 -> Bool -- ^ @openAll@ - whether to recursively expand, or just expand
             -- immediate children
 -> IO Bool -- ^ returns @True@ if the row existed and had children
treeViewExpandRow self path openAll =
  liftM toBool $
  withTreePath path $ \path ->
  (\(TreeView arg1) (NativeTreePath arg2) arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_expand_row argPtr1 arg2 arg3)
{-# LINE 780 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
    path
    (fromBool openAll)

-- | Collapses a row (hides its child rows, if they exist).
--
treeViewCollapseRow :: TreeViewClass self => self
 -> TreePath -- ^ @path@ - path to a row in the tree view
 -> IO Bool -- ^ returns @True@ if the row was collapsed.
treeViewCollapseRow self path =
  liftM toBool $
  withTreePath path $ \path ->
  (\(TreeView arg1) (NativeTreePath arg2) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_collapse_row argPtr1 arg2)
{-# LINE 793 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
    path

-- | Call function for every expaned row.
--
treeViewMapExpandedRows :: TreeViewClass self => self
 -> (TreePath -> IO ())
 -> IO ()
treeViewMapExpandedRows self func = do
  fPtr <- mkTreeViewMappingFunc $ \_ tpPtr _ -> fromTreePath tpPtr >>= func
  (\(TreeView arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_map_expanded_rows argPtr1 arg2 arg3)
{-# LINE 804 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
    fPtr
    nullPtr
  freeHaskellFunPtr fPtr

type TreeViewMappingFunc = FunPtr (((Ptr TreeView) -> ((Ptr NativeTreePath) -> ((Ptr ()) -> (IO ())))))
{-# LINE 810 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}

foreign import ccall "wrapper" mkTreeViewMappingFunc ::
  (Ptr TreeView -> Ptr NativeTreePath -> Ptr () -> IO ()) ->
  IO TreeViewMappingFunc

-- | Check if row is expanded.
--
treeViewRowExpanded :: TreeViewClass self => self
 -> TreePath -- ^ @path@ - A 'TreePath' to test expansion state.
 -> IO Bool -- ^ returns @True@ if @path@ is expanded.
treeViewRowExpanded self path =
  liftM toBool $
  withTreePath path $ \path ->
  (\(TreeView arg1) (NativeTreePath arg2) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_row_expanded argPtr1 arg2)
{-# LINE 824 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
    path

-- | Query if rows can be moved around.
--
-- * See 'treeViewSetReorderable'.
--
treeViewGetReorderable :: TreeViewClass self => self -> IO Bool
treeViewGetReorderable self =
  liftM toBool $
  (\(TreeView arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_get_reorderable argPtr1)
{-# LINE 835 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)

-- | Check if rows can be moved around.
--
-- * Set whether the user can use drag and drop (DND) to reorder the rows in
-- the store. This works on both 'TreeStore' and 'ListStore' models. If @ro@
-- is @True@, then the user can reorder the model by dragging and dropping
-- rows. The developer can listen to these changes by connecting to the
-- model's signals. If you need to control which rows may be dragged or
-- where rows may be dropped, you can override the
-- 'Graphics.UI.Gtk.ModelView.CustomStore.treeDragSourceRowDraggable'
-- function in the default DND implementation of the model.
--
treeViewSetReorderable :: TreeViewClass self => self
 -> Bool
 -> IO ()
treeViewSetReorderable self reorderable =
  (\(TreeView arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_set_reorderable argPtr1 arg2)
{-# LINE 853 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
    (fromBool reorderable)

-- | Map a pixel to the specific cell.
--
-- * Finds the path at the 'Point' @(x, y)@. The
-- coordinates @x@ and @y@ are relative to the top left
-- corner of the 'TreeView' drawing window. As such, coordinates
-- in a mouse click event can be used directly to determine the cell
-- which the user clicked on. This function is useful to realize
-- popup menus.
--
-- * The returned point is the input point relative to the cell's upper
-- left corner. The whole 'TreeView' is divided between all cells.
-- The returned point is relative to the rectangle this cell occupies
-- within the 'TreeView'.
--
treeViewGetPathAtPos :: TreeViewClass self => self
 -> Point
 -> IO (Maybe (TreePath, TreeViewColumn, Point))
treeViewGetPathAtPos self (x,y) =
  alloca $ \tpPtrPtr ->
  alloca $ \tvcPtrPtr ->
  alloca $ \xPtr ->
  alloca $ \yPtr -> do
    res <- liftM toBool $
      (\(TreeView arg1) arg2 arg3 arg4 arg5 arg6 arg7 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_get_path_at_pos argPtr1 arg2 arg3 arg4 arg5 arg6 arg7)
{-# LINE 880 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
      (toTreeView self)
      (fromIntegral x)
      (fromIntegral y)
      (castPtr tpPtrPtr)
      (castPtr tvcPtrPtr)
      xPtr
      yPtr
    tpPtr <- peek tpPtrPtr
    tvcPtr <- peek tvcPtrPtr
    xCell <- peek xPtr
    yCell <- peek yPtr
    if not res then return Nothing else do
      tp <- fromTreePath tpPtr
      tvc <- makeNewObject mkTreeViewColumn (return tvcPtr)
      return (Just (tp,tvc,(fromIntegral xCell, fromIntegral yCell)))

-- | Retrieve the smallest bounding box of a cell.
--
-- * Fills the bounding rectangle in tree window coordinates for the
-- cell at the row specified by @tp@ and the column specified by
-- @tvc@.
-- If @path@ is @Nothing@ or points to a path not
-- currently displayed, the @y@ and @height@ fields of
-- the 'Rectangle' will be filled with @0@. The sum of
-- all cell rectangles does not cover the entire tree; there are extra
-- pixels in between rows, for example.
--
treeViewGetCellArea :: TreeViewClass self => self
 -> Maybe TreePath
 -> TreeViewColumn
 -> IO Rectangle
treeViewGetCellArea self Nothing tvc =
  alloca $ \rPtr ->
  (\(TreeView arg1) (NativeTreePath arg2) (TreeViewColumn arg3) arg4 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg3 $ \argPtr3 ->gtk_tree_view_get_cell_area argPtr1 arg2 argPtr3 arg4)
{-# LINE 914 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
    (NativeTreePath nullPtr)
    tvc
    (castPtr (rPtr :: Ptr Rectangle))
    >> peek rPtr
treeViewGetCellArea self (Just tp) tvc =
  withTreePath tp $ \tp ->
  alloca $ \rPtr -> do
  (\(TreeView arg1) (NativeTreePath arg2) (TreeViewColumn arg3) arg4 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg3 $ \argPtr3 ->gtk_tree_view_get_cell_area argPtr1 arg2 argPtr3 arg4)
{-# LINE 923 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
    tp
    tvc
    (castPtr (rPtr :: Ptr Rectangle))
    >> peek rPtr

-- | Retrieve the largest bounding box of a cell.
--
-- * Fills the bounding rectangle in tree window coordinates for the
-- cell at the row specified by @tp@ and the column specified by
-- @tvc@.
-- If @path@ is @Nothing@ or points to a path not
-- currently displayed, the @y@ and @height@ fields of
-- the 'Rectangle' will be filled with @0@. The background
-- areas tile the widget's area to cover the entire tree window
-- (except for the area used for header buttons). Contrast this with
-- 'treeViewGetCellArea'.
--
treeViewGetBackgroundArea :: TreeViewClass self => self
 -> Maybe TreePath
 -> TreeViewColumn
 -> IO Rectangle
treeViewGetBackgroundArea self Nothing tvc =
  alloca $ \rPtr ->
  (\(TreeView arg1) (NativeTreePath arg2) (TreeViewColumn arg3) arg4 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg3 $ \argPtr3 ->gtk_tree_view_get_background_area argPtr1 arg2 argPtr3 arg4)
{-# LINE 948 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
    (NativeTreePath nullPtr)
    tvc
    (castPtr (rPtr :: Ptr Rectangle))
  >> peek rPtr
treeViewGetBackgroundArea self (Just tp) tvc =
  withTreePath tp $ \tp -> alloca $ \rPtr ->
  (\(TreeView arg1) (NativeTreePath arg2) (TreeViewColumn arg3) arg4 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg3 $ \argPtr3 ->gtk_tree_view_get_background_area argPtr1 arg2 argPtr3 arg4)
{-# LINE 956 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
    tp
    tvc
    (castPtr (rPtr :: Ptr Rectangle))
  >> peek rPtr

-- | Retrieve the currently visible area.
--
-- * The returned rectangle gives the visible part of the tree in tree
-- coordinates.
--
treeViewGetVisibleRect :: TreeViewClass self => self -> IO Rectangle
treeViewGetVisibleRect self =
  alloca $ \rPtr -> do
  (\(TreeView arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_get_visible_rect argPtr1 arg2)
{-# LINE 971 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
    (castPtr (rPtr :: Ptr Rectangle))
  peek rPtr



-- | 'treeViewTreeToWidgetCoords' has been deprecated since version 2.12 and should not be used in
-- newly-written code. Due to historial reasons the name of this function is incorrect. For converting
-- bin window coordinates to coordinates relative to bin window, please see
-- 'treeViewConvertBinWindowToWidgetCoords'.
--
-- Converts tree coordinates (coordinates in full scrollable area of the tree) to bin window
-- coordinates.
--
-- Removed in Gtk3.
treeViewTreeToWidgetCoords :: TreeViewClass self => self
 -> Point -- ^ @(tx, ty)@ - tree X and Y coordinates
 -> IO Point -- ^ @(wx, wy)@ returns widget X and Y coordinates
treeViewTreeToWidgetCoords self (tx, ty) =
  alloca $ \wxPtr ->
  alloca $ \wyPtr -> do
  (\(TreeView arg1) arg2 arg3 arg4 arg5 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_tree_to_widget_coords argPtr1 arg2 arg3 arg4 arg5)
{-# LINE 993 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
    (fromIntegral tx)
    (fromIntegral ty)
    wxPtr
    wyPtr
  wx <- peek wxPtr
  wy <- peek wyPtr
  return (fromIntegral wx, fromIntegral wy)

-- | 'treeViewWidgetToTreeCoords' has been deprecated since version 2.12 and should not be used in
-- newly-written code. Due to historial reasons the name of this function is incorrect. For converting
-- coordinates relative to the widget to bin window coordinates, please see
-- 'treeViewConvertWidgetToBinWindowCoords'.
--
-- Converts bin window coordinates to coordinates for the tree (the full scrollable area of the tree).
--
-- Removed in Gtk3.
treeViewWidgetToTreeCoords :: TreeViewClass self => self
 -> Point -- ^ @(wx, wy)@ - widget X and Y coordinates
 -> IO Point -- ^ @(tx, ty)@ returns tree X and Y coordinates
treeViewWidgetToTreeCoords self (wx, wy) =
  alloca $ \txPtr ->
  alloca $ \tyPtr -> do
  (\(TreeView arg1) arg2 arg3 arg4 arg5 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_widget_to_tree_coords argPtr1 arg2 arg3 arg4 arg5)
{-# LINE 1017 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
    (fromIntegral wx)
    (fromIntegral wy)
    txPtr
    tyPtr
  tx <- peek txPtr
  ty <- peek tyPtr
  return (fromIntegral tx, fromIntegral ty)




-- | Converts bin window coordinates to coordinates for the tree (the full scrollable area of the tree).
treeViewConvertBinWindowToTreeCoords :: TreeViewClass self => self
 -> Point -- ^ @(bx, by)@ - bin window X and Y coordinates
 -> IO Point -- ^ @(tx, ty)@ returns tree X and Y coordinates
treeViewConvertBinWindowToTreeCoords self (bx, by) =
  alloca $ \txPtr ->
  alloca $ \tyPtr -> do
  (\(TreeView arg1) arg2 arg3 arg4 arg5 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_convert_bin_window_to_tree_coords argPtr1 arg2 arg3 arg4 arg5)
{-# LINE 1037 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
    (fromIntegral bx)
    (fromIntegral by)
    txPtr
    tyPtr
  tx <- peek txPtr
  ty <- peek tyPtr
  return (fromIntegral tx, fromIntegral ty)

-- | Converts bin window coordinates (see 'treeViewGetBinWindow' to widget relative coordinates.
treeViewConvertBinWindowToWidgetCoords :: TreeViewClass self => self
 -> Point -- ^ @(bx, by)@ - bin window X and Y coordinates
 -> IO Point -- ^ @(wx, wy)@ returns widget X and Y coordinates
treeViewConvertBinWindowToWidgetCoords self (bx, by) =
  alloca $ \wxPtr ->
  alloca $ \wyPtr -> do
  (\(TreeView arg1) arg2 arg3 arg4 arg5 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_convert_bin_window_to_widget_coords argPtr1 arg2 arg3 arg4 arg5)
{-# LINE 1054 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
    (fromIntegral bx)
    (fromIntegral by)
    wxPtr
    wyPtr
  wx <- peek wxPtr
  wy <- peek wyPtr
  return (fromIntegral wx, fromIntegral wy)

-- | Converts tree coordinates (coordinates in full scrollable area of the tree) to bin window
-- coordinates.
treeViewConvertTreeToBinWindowCoords :: TreeViewClass self => self
 -> Point -- ^ @(tx, ty)@ - tree X and Y coordinates
 -> IO Point -- ^ @(bx, by)@ returns bin window X and Y coordinates
treeViewConvertTreeToBinWindowCoords self (tx, ty) =
  alloca $ \bxPtr ->
  alloca $ \byPtr -> do
  (\(TreeView arg1) arg2 arg3 arg4 arg5 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_convert_tree_to_bin_window_coords argPtr1 arg2 arg3 arg4 arg5)
{-# LINE 1072 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
    (fromIntegral tx)
    (fromIntegral ty)
    bxPtr
    byPtr
  bx <- peek bxPtr
  by <- peek byPtr
  return (fromIntegral bx, fromIntegral by)

-- | Converts tree coordinates (coordinates in full scrollable area of the tree) to widget coordinates.
treeViewConvertTreeToWidgetCoords :: TreeViewClass self => self
 -> Point -- ^ @(tx, ty)@ - tree X and Y coordinates
 -> IO Point -- ^ @(wx, wy)@ returns widget X and Y coordinates
treeViewConvertTreeToWidgetCoords self (wx, wy) =
  alloca $ \bxPtr ->
  alloca $ \byPtr -> do
  (\(TreeView arg1) arg2 arg3 arg4 arg5 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_convert_tree_to_widget_coords argPtr1 arg2 arg3 arg4 arg5)
{-# LINE 1089 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
    (fromIntegral wx)
    (fromIntegral wy)
    bxPtr
    byPtr
  bx <- peek bxPtr
  by <- peek byPtr
  return (fromIntegral bx, fromIntegral by)

-- | Converts widget coordinates to coordinates for the window (see 'treeViewGetBinWindow' ).
treeViewConvertWidgetToBinWindowCoords :: TreeViewClass self => self
 -> Point -- ^ @(wx, wy)@ - widget X and Y coordinates
 -> IO Point -- ^ @(bx, by)@ returns bin window X and Y coordinates
treeViewConvertWidgetToBinWindowCoords self (wx, wy) =
  alloca $ \bxPtr ->
  alloca $ \byPtr -> do
  (\(TreeView arg1) arg2 arg3 arg4 arg5 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_convert_widget_to_bin_window_coords argPtr1 arg2 arg3 arg4 arg5)
{-# LINE 1106 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
    (fromIntegral wx)
    (fromIntegral wy)
    bxPtr
    byPtr
  bx <- peek bxPtr
  by <- peek byPtr
  return (fromIntegral bx, fromIntegral by)

-- | Converts widget coordinates to coordinates for the tree (the full scrollable area of the tree).
treeViewConvertWidgetToTreeCoords :: TreeViewClass self => self
 -> Point -- ^ @(wx, wy)@ - bin window X and Y coordinates
 -> IO Point -- ^ @(tx, ty)@ returns tree X and Y coordinates
treeViewConvertWidgetToTreeCoords self (wx, wy) =
  alloca $ \txPtr ->
  alloca $ \tyPtr -> do
  (\(TreeView arg1) arg2 arg3 arg4 arg5 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_convert_widget_to_tree_coords argPtr1 arg2 arg3 arg4 arg5)
{-# LINE 1123 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
    (fromIntegral wx)
    (fromIntegral wy)
    txPtr
    tyPtr
  tx <- peek txPtr
  ty <- peek tyPtr
  return (fromIntegral tx, fromIntegral ty)



-- | Creates a 'Pixmap' representation of the row at the given path. This image
-- can be used for a drag icon.
--
-- Removed in Gtk3.
treeViewCreateRowDragIcon :: TreeViewClass self => self
 -> TreePath
 -> IO Pixmap
treeViewCreateRowDragIcon self path =
  wrapNewGObject mkPixmap $
  withTreePath path $ \path ->
  (\(TreeView arg1) (NativeTreePath arg2) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_create_row_drag_icon argPtr1 arg2)
{-# LINE 1145 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
    path


-- | Returns whether or not the tree allows to start interactive searching by
-- typing in text.
--
-- * If enabled, the user can type in text which will set the cursor to
-- the first matching entry.
--
treeViewGetEnableSearch :: TreeViewClass self => self -> IO Bool
treeViewGetEnableSearch self =
  liftM toBool $
  (\(TreeView arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_get_enable_search argPtr1)
{-# LINE 1159 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)

-- | If this is set, then the user can type in text to search
-- through the tree interactively (this is sometimes called \"typeahead
-- find\").
--
-- Note that even if this is @False@, the user can still initiate a search
-- using the \"start-interactive-search\" key binding. In any case,
-- a predicate that compares a row of the model with the text the user
-- has typed must be set using 'treeViewSetSearchEqualFunc'.
--
treeViewSetEnableSearch :: TreeViewClass self => self -> Bool -> IO ()
treeViewSetEnableSearch self enableSearch =
  (\(TreeView arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_set_enable_search argPtr1 arg2)
{-# LINE 1173 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
    (fromBool enableSearch)

-- %hash c:ecc5 d:bed6
-- | Gets the column searched on by the interactive search code.
--
treeViewGetSearchColumn :: (TreeViewClass self, GlibString string) => self
 -> IO (ColumnId row string) -- ^ returns the column the interactive search code searches in.
treeViewGetSearchColumn self =
  liftM (makeColumnIdString . fromIntegral) $
  (\(TreeView arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_get_search_column argPtr1)
{-# LINE 1184 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)

-- %hash c:d0d0
-- | Sets @column@ as the column where the interactive search code should
-- search in.
--
-- If the sort column is set, users can use the \"start-interactive-search\"
-- key binding to bring up search popup. The enable-search property controls
-- whether simply typing text will also start an interactive search.
--
-- Note that @column@ refers to a column of the model. Furthermore, the
-- search column is not used if a comparison function is set, see
-- 'treeViewSetSearchEqualFunc'.
--
treeViewSetSearchColumn :: (TreeViewClass self, GlibString string) => self
 -> (ColumnId row string) -- ^ @column@ - the column of the model to search in, or -1 to disable
        -- searching
 -> IO ()
treeViewSetSearchColumn self column =
  (\(TreeView arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_set_search_column argPtr1 arg2)
{-# LINE 1204 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
    (fromIntegral (columnIdToNumber column))


-- | Set the predicate to test for equality.
--
-- * The predicate must returns @True@ if the text entered by the user
-- and the row of the model match. Calling this function will overwrite
-- the 'treeViewSearchColumn' (which isn't used anyway when a comparison
-- function is installed).
--
treeViewSetSearchEqualFunc :: (TreeViewClass self, GlibString string) => self
 -> Maybe (string -> TreeIter -> IO Bool)
 -> IO ()
treeViewSetSearchEqualFunc self (Just pred) = do
  fPtr <- mkTreeViewSearchEqualFunc (\_ _ keyPtr iterPtr _ -> do
    key <- peekUTFString keyPtr
    iter <- peek iterPtr
    liftM (fromBool . not) $ pred key iter)
  (\(TreeView arg1) arg2 arg3 arg4 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_set_search_equal_func argPtr1 arg2 arg3 arg4) (toTreeView self) fPtr
    (castFunPtrToPtr fPtr) destroyFunPtr
  (\(TreeView arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_set_search_column argPtr1 arg2) (toTreeView self) 0
treeViewSetSearchEqualFunc self Nothing = do
  (\(TreeView arg1) arg2 arg3 arg4 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_set_search_equal_func argPtr1 arg2 arg3 arg4) (toTreeView self)
    nullFunPtr nullPtr nullFunPtr
  (\(TreeView arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_set_search_column argPtr1 arg2) (toTreeView self) (-1)

type TreeViewSearchEqualFunc = FunPtr (((Ptr TreeModel) -> (CInt -> ((Ptr CChar) -> ((Ptr TreeIter) -> ((Ptr ()) -> (IO CInt)))))))
{-# LINE 1232 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}

foreign import ccall "wrapper" mkTreeViewSearchEqualFunc ::
  (Ptr TreeModel -> (CInt) -> CString -> Ptr TreeIter -> Ptr () ->
   IO (CInt)) -> IO TreeViewSearchEqualFunc

-- helper to marshal native tree paths to TreePaths
readNTP :: Ptr TreePath -> IO TreePath
readNTP ptr = peekTreePath (castPtr ptr)


-- | Returns whether fixed height mode is turned on for the tree view.
--
-- * Available since Gtk+ version 2.6
--
treeViewGetFixedHeightMode :: TreeViewClass self => self
 -> IO Bool -- ^ returns @True@ if the tree view is in fixed height mode
treeViewGetFixedHeightMode self =
  liftM toBool $
  (\(TreeView arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_get_fixed_height_mode argPtr1)
{-# LINE 1251 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)

-- | Enables or disables the fixed height mode of the tree view. Fixed height
-- mode speeds up 'TreeView' by assuming that all rows have the same height.
-- Only enable this option if all rows are the same height and all columns are
-- of type 'TreeViewColumnFixed'.
--
-- * Available since Gtk+ version 2.6
--
treeViewSetFixedHeightMode :: TreeViewClass self => self
 -> Bool -- ^ @enable@ - @True@ to enable fixed height mode
 -> IO ()
treeViewSetFixedHeightMode self enable =
  (\(TreeView arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_set_fixed_height_mode argPtr1 arg2)
{-# LINE 1265 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
    (fromBool enable)

-- | Returns whether hover selection mode is turned on for @treeView@.
--
-- * Available since Gtk+ version 2.6
--
treeViewGetHoverSelection :: TreeViewClass self => self
 -> IO Bool -- ^ returns @True@ if the tree view is in hover selection mode
treeViewGetHoverSelection self =
  liftM toBool $
  (\(TreeView arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_get_hover_selection argPtr1)
{-# LINE 1277 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)

-- | Enables of disables the hover selection mode of the tree view. Hover
-- selection makes the selected row follow the pointer. Currently, this works
-- only for the selection modes 'SelectionSingle' and 'SelectionBrowse'.
--
-- * Available since Gtk+ version 2.6
--
treeViewSetHoverSelection :: TreeViewClass self => self
 -> Bool -- ^ @hover@ - @True@ to enable hover selection mode
 -> IO ()
treeViewSetHoverSelection self hover =
  (\(TreeView arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_set_hover_selection argPtr1 arg2)
{-# LINE 1290 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
    (fromBool hover)

-- | Returns whether hover expansion mode is turned on for the tree view.
--
-- * Available since Gtk+ version 2.6
--
treeViewGetHoverExpand :: TreeViewClass self => self
 -> IO Bool -- ^ returns @True@ if the tree view is in hover expansion mode
treeViewGetHoverExpand self =
  liftM toBool $
  (\(TreeView arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_get_hover_expand argPtr1)
{-# LINE 1302 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)

-- | Enables of disables the hover expansion mode of the tree view. Hover
-- expansion makes rows expand or collaps if the pointer moves over them.
--
-- * Available since Gtk+ version 2.6
--
treeViewSetHoverExpand :: TreeViewClass self => self
 -> Bool -- ^ @expand@ - @True@ to enable hover selection mode
 -> IO ()
treeViewSetHoverExpand self expand =
  (\(TreeView arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_set_hover_expand argPtr1 arg2)
{-# LINE 1314 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
    (fromBool expand)




-- %hash c:88cb d:65c9
-- | Returns whether all header columns are clickable.
--
-- * Available since Gtk+ version 2.10
--
treeViewGetHeadersClickable :: TreeViewClass self => self
 -> IO Bool -- ^ returns @True@ if all header columns are clickable, otherwise
            -- @False@
treeViewGetHeadersClickable self =
  liftM toBool $
  (\(TreeView arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_get_headers_clickable argPtr1)
{-# LINE 1331 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)



-- %hash c:1d81 d:3587
-- | Return the first and last visible path.
-- Note that there may be invisible paths in between.
--
-- * Available since Gtk+ version 2.8
--
treeViewGetVisibleRange :: TreeViewClass self => self
 -> IO (TreePath, TreePath) -- ^ the first and the last node that is visible
treeViewGetVisibleRange self = alloca $ \startPtr -> alloca $ \endPtr -> do
  valid <- liftM toBool $
    (\(TreeView arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_get_visible_range argPtr1 arg2 arg3)
{-# LINE 1346 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self) (castPtr startPtr) (castPtr endPtr)
  if not valid then return ([],[]) else do
    startTPPtr <- peek startPtr
    endTPPtr <- peek endPtr
    startPath <- fromTreePath startTPPtr
    endPath <- fromTreePath endTPPtr
    return (startPath, endPath)




-- %hash c:61e1 d:3a0a
-- | Turns @treeView@ into a drop destination for automatic DND.
--
treeViewEnableModelDragDest :: TreeViewClass self => self
  -> TargetList -- ^ @targets@ - the list of targets that the
                               -- the view will support
  -> [DragAction] -- ^ @actions@ - flags denoting the possible actions
                               -- for a drop into this widget
  -> IO ()
treeViewEnableModelDragDest self targets actions =
  alloca $ \nTargetsPtr -> do
  tlPtr <- (\(TargetList arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_target_table_new_from_list argPtr1 arg2) targets nTargetsPtr
  nTargets <- peek nTargetsPtr
  (\(TreeView arg1) arg2 arg3 arg4 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_enable_model_drag_dest argPtr1 arg2 arg3 arg4)
{-# LINE 1371 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
    tlPtr
    nTargets
    ((fromIntegral . fromFlags) actions)
  gtk_target_table_free tlPtr nTargets

-- %hash c:1df9 d:622
-- | Turns @treeView@ into a drag source for automatic DND.
--
treeViewEnableModelDragSource :: TreeViewClass self => self
 -> [Modifier] -- ^ @startButtonMask@ - Mask of allowed buttons
                              -- to start drag
 -> TargetList -- ^ @targets@ - the list of targets that the
                              -- the view will support
 -> [DragAction] -- ^ @actions@ - flags denoting the possible actions
                              -- for a drag from this widget
 -> IO ()
treeViewEnableModelDragSource self startButtonMask targets actions =
  alloca $ \nTargetsPtr -> do
  tlPtr <- (\(TargetList arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_target_table_new_from_list argPtr1 arg2) targets nTargetsPtr
  nTargets <- peek nTargetsPtr
  (\(TreeView arg1) arg2 arg3 arg4 arg5 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_enable_model_drag_source argPtr1 arg2 arg3 arg4 arg5)
{-# LINE 1393 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
    ((fromIntegral . fromFlags) startButtonMask)
    tlPtr
    nTargets
    ((fromIntegral . fromFlags) actions)
  gtk_target_table_free tlPtr nTargets

-- %hash c:5201 d:f3be
-- | Undoes the effect of 'treeViewEnableModelDragSource'.
--
treeViewUnsetRowsDragSource :: TreeViewClass self => self -> IO ()
treeViewUnsetRowsDragSource self =
  (\(TreeView arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_unset_rows_drag_source argPtr1)
{-# LINE 1406 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)

-- %hash c:e31e d:323d
-- | Undoes the effect of 'treeViewEnableModelDragDest'.
--
treeViewUnsetRowsDragDest :: TreeViewClass self => self -> IO ()
treeViewUnsetRowsDragDest self =
  (\(TreeView arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_unset_rows_drag_dest argPtr1)
{-# LINE 1414 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)

-- %hash c:3355 d:3bbe
-- | Returns the 'Entry' which is currently in use as interactive search entry
-- for @treeView@. In case the built-in entry is being used, @Nothing@ will be
-- returned.
--
-- * Available since Gtk+ version 2.10
--
treeViewGetSearchEntry :: TreeViewClass self => self
 -> IO (Maybe Entry) -- ^ returns the entry currently in use as search entry.
treeViewGetSearchEntry self = do
  ePtr <- (\(TreeView arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_get_search_entry argPtr1)
{-# LINE 1427 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
  if ePtr==nullPtr then return Nothing else liftM Just $
    makeNewObject mkEntry (return ePtr)

-- %hash c:5e11 d:8ec5
-- | Sets the entry which the interactive search code will use for this
-- @treeView@. This is useful when you want to provide a search entry in our
-- interface at all time at a fixed position. Passing @Nothing@ for @entry@
-- will make the interactive search code use the built-in popup entry again.
--
-- * Available since Gtk+ version 2.10
--
treeViewSetSearchEntry :: (TreeViewClass self, EntryClass entry) => self
 -> (Maybe entry)
          -- ^ @entry@ - the entry the interactive search code of @treeView@
          -- should use or @Nothing@
 -> IO ()
treeViewSetSearchEntry self (Just entry) =
  (\(TreeView arg1) (Entry arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_tree_view_set_search_entry argPtr1 argPtr2)
{-# LINE 1446 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
    (toEntry entry)
treeViewSetSearchEntry self Nothing =
  (\(TreeView arg1) (Entry arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_tree_view_set_search_entry argPtr1 argPtr2)
{-# LINE 1450 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
    (Entry nullForeignPtr)



-- %hash c:6326 d:a050
-- | Sets the row separator function, which is used to determine whether a row
-- should be drawn as a separator. If the row separator function is @Nothing@,
-- no separators are drawn. This is the default value.
--
-- * Available since Gtk+ version 2.6
--
treeViewSetRowSeparatorFunc :: TreeViewClass self => self
 -> Maybe (TreeIter -> IO Bool) -- ^ @func@ - a callback function that
                                    -- returns @True@ if the given row of
                                    -- the model should be drawn as separator
 -> IO ()
treeViewSetRowSeparatorFunc self Nothing =
  (\(TreeView arg1) arg2 arg3 arg4 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_set_row_separator_func argPtr1 arg2 arg3 arg4)
{-# LINE 1469 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self) nullFunPtr nullPtr nullFunPtr
treeViewSetRowSeparatorFunc self (Just func) = do
  funcPtr <- mkTreeViewRowSeparatorFunc $ \_ tiPtr _ -> do
    ti <- peekTreeIter tiPtr
    liftM fromBool $ func ti
  (\(TreeView arg1) arg2 arg3 arg4 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_set_row_separator_func argPtr1 arg2 arg3 arg4)
{-# LINE 1475 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self) funcPtr (castFunPtrToPtr funcPtr) destroyFunPtr

type TreeViewRowSeparatorFunc = FunPtr (((Ptr TreeModel) -> ((Ptr TreeIter) -> ((Ptr ()) -> (IO CInt)))))
{-# LINE 1478 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}

foreign import ccall "wrapper" mkTreeViewRowSeparatorFunc ::
  (Ptr TreeModel -> Ptr TreeIter -> Ptr () -> IO (CInt)) ->
  IO TreeViewRowSeparatorFunc


-- %hash c:778a d:eacd
-- | Returns whether rubber banding is turned on for @treeView@. If the
-- selection mode is 'SelectionMultiple', rubber banding will allow the user to
-- select multiple rows by dragging the mouse.
--
-- * Available since Gtk+ version 2.10
--
treeViewGetRubberBanding :: TreeViewClass self => self
 -> IO Bool -- ^ returns @True@ if rubber banding in @treeView@ is enabled.
treeViewGetRubberBanding self =
  liftM toBool $
  (\(TreeView arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_get_rubber_banding argPtr1)
{-# LINE 1496 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)

-- %hash c:4a69 d:93aa
-- | Enables or disables rubber banding in @treeView@. If the selection mode
-- is 'SelectionMultiple', rubber banding will allow the user to select
-- multiple rows by dragging the mouse.
--
-- * Available since Gtk+ version 2.10
--
treeViewSetRubberBanding :: TreeViewClass self => self
 -> Bool -- ^ @enable@ - @True@ to enable rubber banding
 -> IO ()
treeViewSetRubberBanding self enable =
  (\(TreeView arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_set_rubber_banding argPtr1 arg2)
{-# LINE 1510 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
    (fromBool enable)

-- %hash c:c8f8 d:c47
-- | Returns whether or not tree lines are drawn in @treeView@.
--
-- * Available since Gtk+ version 2.10
--
treeViewGetEnableTreeLines :: TreeViewClass self => self
 -> IO Bool -- ^ returns @True@ if tree lines are drawn in @treeView@, @False@
            -- otherwise.
treeViewGetEnableTreeLines self =
  liftM toBool $
  (\(TreeView arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_get_enable_tree_lines argPtr1)
{-# LINE 1524 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)

-- %hash c:205d d:1df9
-- | Sets whether to draw lines interconnecting the expanders in @treeView@.
-- This does not have any visible effects for lists.
--
-- * Available since Gtk+ version 2.10
--
treeViewSetEnableTreeLines :: TreeViewClass self => self
 -> Bool -- ^ @enabled@ - @True@ to enable tree line drawing, @False@
         -- otherwise.
 -> IO ()
treeViewSetEnableTreeLines self enabled =
  (\(TreeView arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_set_enable_tree_lines argPtr1 arg2)
{-# LINE 1538 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
    (fromBool enabled)

-- | Grid lines.
data TreeViewGridLines = TreeViewGridLinesNone
                       | TreeViewGridLinesHorizontal
                       | TreeViewGridLinesVertical
                       | TreeViewGridLinesBoth
                       deriving (Enum)

{-# LINE 1543 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}

-- %hash c:cd40 d:fe96
-- | Returns which grid lines are enabled in @treeView@.
--
-- * Available since Gtk+ version 2.10
--
treeViewGetGridLines :: TreeViewClass self => self
 -> IO TreeViewGridLines -- ^ returns a 'TreeViewGridLines' value indicating
                         -- which grid lines are enabled.
treeViewGetGridLines self =
  liftM (toEnum . fromIntegral) $
  (\(TreeView arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_get_grid_lines argPtr1)
{-# LINE 1555 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)

-- %hash c:74b0 d:79f0
-- | Sets which grid lines to draw in @treeView@.
--
-- * Available since Gtk+ version 2.10
--
treeViewSetGridLines :: TreeViewClass self => self
 -> TreeViewGridLines -- ^ @gridLines@ - a 'TreeViewGridLines' value
                      -- indicating which grid lines to enable.
 -> IO ()
treeViewSetGridLines self gridLines =
  (\(TreeView arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_set_grid_lines argPtr1 arg2)
{-# LINE 1568 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
    ((fromIntegral . fromEnum) gridLines)




-- | Sets the tip area of @tooltip@ to be the area covered by @path@. See also
-- 'treeViewTooltipColumn' for a simpler alternative. See also
-- 'tooltipSetTipArea'.
treeViewSetTooltipRow :: TreeViewClass self => self
  -> Tooltip -- ^ the @tooltip@
  -> TreePath -- ^ @path@ - the position of the @tooltip@
  -> IO ()
treeViewSetTooltipRow self tip path =
  withTreePath path $ \path ->
  (\(TreeView arg1) (Tooltip arg2) (NativeTreePath arg3) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_tree_view_set_tooltip_row argPtr1 argPtr2 arg3) (toTreeView self) tip path

-- | Sets the tip area of tooltip to the area path, column and cell have in
-- common. For example if @path@ is @Nothing@ and @column@ is set, the tip area will be
-- set to the full area covered by column. See also
-- 'tooltipSetTipArea'. Note that if @path@ is not specified and @cell@ is
-- set and part of a column containing the expander, the tooltip might not
-- show and hide at the correct position. In such cases @path@ must be set to
-- the current node under the mouse cursor for this function to operate
-- correctly. See also 'treeViewTooltipColumn' for a simpler alternative.
--
treeViewSetTooltipCell :: (TreeViewClass self, TreeViewColumnClass col,
                           CellRendererClass renderer) => self
  -> Tooltip -- ^ the @tooltip@
  -> Maybe TreePath -- ^ @path@ at which the tip should be shown
  -> Maybe col -- ^ @column@ at which the tip should be shown
  -> Maybe renderer -- ^ the @renderer@ for which to show the tip
  -> IO ()
treeViewSetTooltipCell self tip mPath mColumn mRenderer =
  (case mPath of Just path -> withTreePath path
                 Nothing -> \f -> f (NativeTreePath nullPtr)) $ \path -> do
  (\(TreeView arg1) (Tooltip arg2) (NativeTreePath arg3) (TreeViewColumn arg4) (CellRenderer arg5) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->withForeignPtr arg4 $ \argPtr4 ->withForeignPtr arg5 $ \argPtr5 ->gtk_tree_view_set_tooltip_cell argPtr1 argPtr2 arg3 argPtr4 argPtr5) (toTreeView self) tip path
    (maybe (TreeViewColumn nullForeignPtr) toTreeViewColumn mColumn)
    (maybe (CellRenderer nullForeignPtr) toCellRenderer mRenderer)

-- | This function is supposed to be used in a 'widgetQueryTooltip' signal handler
-- for this 'TreeView'. The @point@ value which is received in the
-- signal handler should be passed to this function without modification. A
-- return value of @Just iter@ indicates that there is a tree view row at the given
-- coordinates (if @Just (x,y)@ is passed in, denoting a mouse position), resp.
-- the cursor row (if @Nothing@ is passed in, denoting a keyboard request).
--
treeViewGetTooltipContext :: TreeViewClass self => self
  -> Maybe Point -- ^ @point@ - the coordinates of the mouse or @Nothing@
                 -- if a keyboard tooltip is to be generated
  -> IO (Maybe TreeIter) -- ^ @Just iter@ if a tooltip should be shown for that row
treeViewGetTooltipContext self (Just (x,y)) =
  alloca $ \xPtr -> alloca $ \yPtr -> receiveTreeIter $
    (\(TreeView arg1) arg2 arg3 arg4 arg5 arg6 arg7 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_get_tooltip_context argPtr1 arg2 arg3 arg4 arg5 arg6 arg7) (toTreeView self)
    xPtr yPtr 0 nullPtr nullPtr
treeViewGetTooltipContext self Nothing =
  receiveTreeIter $
    (\(TreeView arg1) arg2 arg3 arg4 arg5 arg6 arg7 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_get_tooltip_context argPtr1 arg2 arg3 arg4 arg5 arg6 arg7) (toTreeView self)
    nullPtr nullPtr 1 nullPtr nullPtr


--------------------
-- Attributes

-- | The model for the tree view.
--
treeViewModel :: TreeViewClass self => Attr self (Maybe TreeModel)
treeViewModel = newAttr
  treeViewGetModel
  treeViewSetModel

-- | Horizontal Adjustment for the widget.
--
treeViewHAdjustment :: TreeViewClass self => Attr self (Maybe Adjustment)
treeViewHAdjustment = newAttr
  treeViewGetHAdjustment
  treeViewSetHAdjustment

-- | Vertical Adjustment for the widget.
--
treeViewVAdjustment :: TreeViewClass self => Attr self (Maybe Adjustment)
treeViewVAdjustment = newAttr
  treeViewGetVAdjustment
  treeViewSetVAdjustment

-- | Show the column header buttons.
--
-- Default value: @True@
--
treeViewHeadersVisible :: TreeViewClass self => Attr self Bool
treeViewHeadersVisible = newAttr
  treeViewGetHeadersVisible
  treeViewSetHeadersVisible

-- | Column headers respond to click events.
--
-- Default value: @False@
--
treeViewHeadersClickable :: TreeViewClass self => Attr self Bool
treeViewHeadersClickable = newAttrFromBoolProperty "headers-clickable"

-- | Set the column for the expander column.
--
treeViewExpanderColumn :: TreeViewClass self => ReadWriteAttr self TreeViewColumn (Maybe TreeViewColumn)
treeViewExpanderColumn = newAttr
  treeViewGetExpanderColumn
  treeViewSetExpanderColumn

-- | View is reorderable.
--
-- Default value: @False@
--
treeViewReorderable :: TreeViewClass self => Attr self Bool
treeViewReorderable = newAttr
  treeViewGetReorderable
  treeViewSetReorderable

-- | Set a hint to the theme engine to draw rows in alternating colors.
--
-- Default value: @False@
--
treeViewRulesHint :: TreeViewClass self => Attr self Bool
treeViewRulesHint = newAttr
  treeViewGetRulesHint
  treeViewSetRulesHint

-- | View allows user to search through columns interactively.
--
-- Default value: @True@
--
treeViewEnableSearch :: TreeViewClass self => Attr self Bool
treeViewEnableSearch = newAttr
  treeViewGetEnableSearch
  treeViewSetEnableSearch

-- %hash c:e732
-- | Model column to search through when searching through code.
--
-- Default value: 'invalidColumnId'
--
treeViewSearchColumn :: (TreeViewClass self, GlibString string) => Attr self (ColumnId row string)
treeViewSearchColumn = newAttr
  treeViewGetSearchColumn
  treeViewSetSearchColumn


-- %hash c:c7ff d:24d1
-- | Setting the 'treeViewFixedHeightMode' property to @True@ speeds up 'TreeView'
-- by assuming that all rows have the same height. Only enable this option if
-- all rows are the same height. Please see 'treeViewSetFixedHeightMode' for
-- more information on this option.
--
-- Default value: @False@
--
-- * Available since Gtk+ version 2.4
--
treeViewFixedHeightMode :: TreeViewClass self => Attr self Bool
treeViewFixedHeightMode = newAttrFromBoolProperty "fixed-height-mode"


-- %hash c:2026 d:839a
-- | Enables of disables the hover selection mode of @treeView@. Hover
-- selection makes the selected row follow the pointer. Currently, this works
-- only for the selection modes 'SelectionSingle' and 'SelectionBrowse'.
--
-- This mode is primarily intended for 'TreeView's in popups, e.g. in
-- 'ComboBox' or 'EntryCompletion'.
--
-- Default value: @False@
--
-- * Available since Gtk+ version 2.6
--
treeViewHoverSelection :: TreeViewClass self => Attr self Bool
treeViewHoverSelection = newAttrFromBoolProperty "hover-selection"

-- %hash c:c694 d:3f15
-- | Enables of disables the hover expansion mode of @treeView@. Hover
-- expansion makes rows expand or collaps if the pointer moves over them.
--
-- This mode is primarily intended for 'TreeView's in popups, e.g. in
-- 'ComboBox' or 'EntryCompletion'.
--
-- Default value: @False@
--
-- * Available since Gtk+ version 2.6
--
treeViewHoverExpand :: TreeViewClass self => Attr self Bool
treeViewHoverExpand = newAttrFromBoolProperty "hover-expand"



-- %hash c:b409 d:2ed2
-- | View has expanders.
--
-- Default value: @True@
--
treeViewShowExpanders :: TreeViewClass self => Attr self Bool
treeViewShowExpanders = newAttrFromBoolProperty "show-expanders"

-- %hash c:f0e5 d:9017
-- | Extra indentation for each level.
--
-- Allowed values: >= 0
--
-- Default value: 0
--
treeViewLevelIndentation :: TreeViewClass self => Attr self Int
treeViewLevelIndentation = newAttrFromIntProperty "level-indentation"

-- %hash c:a647 d:9e53
-- | Whether to enable selection of multiple items by dragging the mouse
-- pointer.
--
-- Default value: @False@
--
treeViewRubberBanding :: TreeViewClass self => Attr self Bool
treeViewRubberBanding = newAttrFromBoolProperty "rubber-banding"


-- %hash c:e926 d:86a8
-- | Whether grid lines should be drawn in the tree view.
--
-- Default value: 'TreeViewGridLinesNone'
--
treeViewEnableGridLines :: TreeViewClass self => Attr self TreeViewGridLines
treeViewEnableGridLines = newAttrFromEnumProperty "enable-grid-lines"
                            gtk_tree_view_grid_lines_get_type
{-# LINE 1795 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}


-- %hash c:a7eb d:4c53
-- | Whether tree lines should be drawn in the tree view.
--
-- Default value: @False@
--
treeViewEnableTreeLines :: TreeViewClass self => Attr self Bool
treeViewEnableTreeLines = newAttrFromBoolProperty "enable-tree-lines"


-- %hash c:688c d:cbcd
-- | \'gridLines\' property. See 'treeViewGetGridLines' and
-- 'treeViewSetGridLines'
--
treeViewGridLines :: TreeViewClass self => Attr self TreeViewGridLines
treeViewGridLines = newAttr
  treeViewGetGridLines
  treeViewSetGridLines

-- %hash c:9cbe d:2962
-- | \'searchEntry\' property. See 'treeViewGetSearchEntry' and
-- 'treeViewSetSearchEntry'
--
treeViewSearchEntry :: (TreeViewClass self, EntryClass entry) => ReadWriteAttr self (Maybe Entry) (Maybe entry)
treeViewSearchEntry = newAttr
  treeViewGetSearchEntry
  treeViewSetSearchEntry



-- | The column for which to show tooltips.
--
-- If you only plan to have simple (text-only) tooltips on full rows, you can
-- use this function to have 'TreeView' handle these automatically for you.
-- @column@ should be set to a column in model containing the tooltip texts,
-- or @-1@ to disable this feature. When enabled, 'widgetHasTooltip' will be
-- set to @True@ and this view will connect to the 'widgetQueryTooltip' signal
-- handler.
--
-- Note that the signal handler sets the text as 'Markup',
-- so \&, \<, etc have to be escaped in the text.
--
-- Default value: 'invalidColumnId'
--
treeViewTooltipColumn :: (TreeViewClass self, GlibString string) => Attr self (ColumnId row string)
treeViewTooltipColumn = newAttr
  (\self -> liftM (makeColumnIdString . fromIntegral) $
  (\(TreeView arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_get_tooltip_column argPtr1)
{-# LINE 1844 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
  )
  (\self column ->
  (\(TreeView arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_set_tooltip_column argPtr1 arg2)
{-# LINE 1848 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
    (toTreeView self)
    (fromIntegral (columnIdToNumber column))
  )


--------------------
-- Signals

-- %hash c:9fc5 d:3e66
-- | The given row is about to be expanded (show its children nodes). Use this
-- signal if you need to control the expandability of individual rows.
--
testExpandRow :: TreeViewClass self => Signal self (TreeIter -> TreePath -> IO Bool)
testExpandRow = Signal (connect_BOXED_BOXED__BOOL "test-expand-row" peek readNTP)

-- %hash c:20de d:96a3
-- | The given row is about to be collapsed (hide its children nodes). Use
-- this signal if you need to control the collapsibility of individual rows.
--
testCollapseRow :: TreeViewClass self => Signal self (TreeIter -> TreePath -> IO Bool)
testCollapseRow = Signal (connect_BOXED_BOXED__BOOL "test-collapse-row" peek readNTP)

-- %hash c:16dc d:b113
-- | The given row has been expanded (child nodes are shown).
--
rowExpanded :: TreeViewClass self => Signal self (TreeIter -> TreePath -> IO ())
rowExpanded = Signal (connect_BOXED_BOXED__NONE "row-expanded" peek readNTP)

-- | A row was activated.
--
-- * Activation usually means the user has pressed return on a row.
--
rowActivated :: TreeViewClass self => Signal self (TreePath -> TreeViewColumn -> IO ())
rowActivated = Signal (connect_BOXED_OBJECT__NONE "row-activated" readNTP)

-- %hash c:9ee6 d:325e
-- | The given row has been collapsed (child nodes are hidden).
--
rowCollapsed :: TreeViewClass self => Signal self (TreeIter -> TreePath -> IO ())
rowCollapsed = Signal (connect_BOXED_BOXED__NONE "row-collapsed" peek readNTP)

-- %hash c:4350 d:4f94
-- | The number of columns of the treeview has changed.
--
columnsChanged :: TreeViewClass self => Signal self (IO ())
columnsChanged = Signal (connect_NONE__NONE "columns-changed")

-- %hash c:6487 d:5b57
-- | The position of the cursor (focused cell) has changed.
--
cursorChanged :: TreeViewClass self => Signal self (IO ())
cursorChanged = Signal (connect_NONE__NONE "cursor-changed")

--------------------
-- Deprecated Signals



-- | The user has dragged a column to another position.
--
onColumnsChanged, afterColumnsChanged :: TreeViewClass self => self
 -> IO ()
 -> IO (ConnectId self)
onColumnsChanged = connect_NONE__NONE "columns_changed" False
afterColumnsChanged = connect_NONE__NONE "columns_changed" True

-- | The cursor in the tree has moved.
--
onCursorChanged, afterCursorChanged :: TreeViewClass self => self
 -> IO ()
 -> IO (ConnectId self)
onCursorChanged = connect_NONE__NONE "cursor_changed" False
afterCursorChanged = connect_NONE__NONE "cursor_changed" True

-- | A row was activated.
--
-- * Activation usually means the user has pressed return on a row.
--
onRowActivated, afterRowActivated :: TreeViewClass self => self
 -> (TreePath -> TreeViewColumn -> IO ())
 -> IO (ConnectId self)
onRowActivated = connect_BOXED_OBJECT__NONE "row_activated"
                   readNTP False
afterRowActivated = connect_BOXED_OBJECT__NONE "row_activated"
                      readNTP True

-- | Children of this node were hidden.
--
onRowCollapsed, afterRowCollapsed :: TreeViewClass self => self
 -> (TreeIter -> TreePath -> IO ())
 -> IO (ConnectId self)
onRowCollapsed = connect_BOXED_BOXED__NONE "row_collapsed"
  peek readNTP False
afterRowCollapsed = connect_BOXED_BOXED__NONE "row_collapsed"
  peek readNTP True

-- | Children of this node are made visible.
--
onRowExpanded, afterRowExpanded :: TreeViewClass self => self
 -> (TreeIter -> TreePath -> IO ())
 -> IO (ConnectId self)
onRowExpanded = connect_BOXED_BOXED__NONE "row_expanded"
  peek readNTP False
afterRowExpanded = connect_BOXED_BOXED__NONE "row_expanded"
  peek readNTP True

-- | The user wants to search interactively.
--
-- * Connect to this signal if you want to provide you own search facility.
-- Note that you must handle all keyboard input yourself.
--
onStartInteractiveSearch, afterStartInteractiveSearch ::
  TreeViewClass self => self -> IO () -> IO (ConnectId self)



onStartInteractiveSearch self fun =
  connect_NONE__BOOL "start_interactive_search" False self (fun >> return True)
afterStartInteractiveSearch self fun =
  connect_NONE__BOOL "start_interactive_search" True self (fun >> return True)
{-# LINE 1978 "./Graphics/UI/Gtk/ModelView/TreeView.chs" #-}
-- | Determine if this row should be collapsed.
--
-- * If the application connects to this function and returns @False@,
-- the specifc row will not be altered.
--
onTestCollapseRow, afterTestCollapseRow :: TreeViewClass self => self
 -> (TreeIter -> TreePath -> IO Bool)
 -> IO (ConnectId self)
onTestCollapseRow = connect_BOXED_BOXED__BOOL "test_collapse_row"
  peek readNTP False
afterTestCollapseRow = connect_BOXED_BOXED__BOOL "test_collapse_row"
  peek readNTP True

-- | Determine if this row should be expanded.
--
-- * If the application connects to this function and returns @False@,
-- the specifc row will not be altered.
--
onTestExpandRow, afterTestExpandRow :: TreeViewClass self => self
 -> (TreeIter -> TreePath -> IO Bool)
 -> IO (ConnectId self)
onTestExpandRow = connect_BOXED_BOXED__BOOL "test_expand_row"
  peek readNTP False
afterTestExpandRow = connect_BOXED_BOXED__BOOL "test_expand_row"
  peek readNTP True

foreign import ccall safe "gtk_tree_view_new"
  gtk_tree_view_new :: (IO (Ptr Widget))

foreign import ccall safe "gtk_tree_view_new_with_model"
  gtk_tree_view_new_with_model :: ((Ptr TreeModel) -> (IO (Ptr Widget)))

foreign import ccall unsafe "gtk_tree_view_get_model"
  gtk_tree_view_get_model :: ((Ptr TreeView) -> (IO (Ptr TreeModel)))

foreign import ccall safe "gtk_tree_view_set_model"
  gtk_tree_view_set_model :: ((Ptr TreeView) -> ((Ptr TreeModel) -> (IO ())))

foreign import ccall unsafe "gtk_tree_view_get_selection"
  gtk_tree_view_get_selection :: ((Ptr TreeView) -> (IO (Ptr TreeSelection)))

foreign import ccall unsafe "gtk_tree_view_get_hadjustment"
  gtk_tree_view_get_hadjustment :: ((Ptr TreeView) -> (IO (Ptr Adjustment)))

foreign import ccall safe "gtk_tree_view_set_hadjustment"
  gtk_tree_view_set_hadjustment :: ((Ptr TreeView) -> ((Ptr Adjustment) -> (IO ())))

foreign import ccall unsafe "gtk_tree_view_get_vadjustment"
  gtk_tree_view_get_vadjustment :: ((Ptr TreeView) -> (IO (Ptr Adjustment)))

foreign import ccall safe "gtk_tree_view_set_vadjustment"
  gtk_tree_view_set_vadjustment :: ((Ptr TreeView) -> ((Ptr Adjustment) -> (IO ())))

foreign import ccall unsafe "gtk_tree_view_get_headers_visible"
  gtk_tree_view_get_headers_visible :: ((Ptr TreeView) -> (IO CInt))

foreign import ccall safe "gtk_tree_view_set_headers_visible"
  gtk_tree_view_set_headers_visible :: ((Ptr TreeView) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_tree_view_columns_autosize"
  gtk_tree_view_columns_autosize :: ((Ptr TreeView) -> (IO ()))

foreign import ccall safe "gtk_tree_view_set_headers_clickable"
  gtk_tree_view_set_headers_clickable :: ((Ptr TreeView) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_tree_view_get_rules_hint"
  gtk_tree_view_get_rules_hint :: ((Ptr TreeView) -> (IO CInt))

foreign import ccall safe "gtk_tree_view_set_rules_hint"
  gtk_tree_view_set_rules_hint :: ((Ptr TreeView) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_tree_view_append_column"
  gtk_tree_view_append_column :: ((Ptr TreeView) -> ((Ptr TreeViewColumn) -> (IO CInt)))

foreign import ccall safe "gtk_tree_view_remove_column"
  gtk_tree_view_remove_column :: ((Ptr TreeView) -> ((Ptr TreeViewColumn) -> (IO CInt)))

foreign import ccall safe "gtk_tree_view_insert_column"
  gtk_tree_view_insert_column :: ((Ptr TreeView) -> ((Ptr TreeViewColumn) -> (CInt -> (IO CInt))))

foreign import ccall unsafe "gtk_tree_view_get_column"
  gtk_tree_view_get_column :: ((Ptr TreeView) -> (CInt -> (IO (Ptr TreeViewColumn))))

foreign import ccall unsafe "gtk_tree_view_get_columns"
  gtk_tree_view_get_columns :: ((Ptr TreeView) -> (IO (Ptr ())))

foreign import ccall safe "gtk_tree_view_move_column_after"
  gtk_tree_view_move_column_after :: ((Ptr TreeView) -> ((Ptr TreeViewColumn) -> ((Ptr TreeViewColumn) -> (IO ()))))

foreign import ccall unsafe "gtk_tree_view_set_expander_column"
  gtk_tree_view_set_expander_column :: ((Ptr TreeView) -> ((Ptr TreeViewColumn) -> (IO ())))

foreign import ccall unsafe "gtk_tree_view_get_expander_column"
  gtk_tree_view_get_expander_column :: ((Ptr TreeView) -> (IO (Ptr TreeViewColumn)))

foreign import ccall safe "gtk_tree_view_set_column_drag_function"
  gtk_tree_view_set_column_drag_function :: ((Ptr TreeView) -> ((FunPtr ((Ptr TreeView) -> ((Ptr TreeViewColumn) -> ((Ptr TreeViewColumn) -> ((Ptr TreeViewColumn) -> ((Ptr ()) -> (IO CInt))))))) -> ((Ptr ()) -> ((FunPtr ((Ptr ()) -> (IO ()))) -> (IO ())))))

foreign import ccall safe "gtk_tree_view_scroll_to_point"
  gtk_tree_view_scroll_to_point :: ((Ptr TreeView) -> (CInt -> (CInt -> (IO ()))))

foreign import ccall safe "gtk_tree_view_scroll_to_cell"
  gtk_tree_view_scroll_to_cell :: ((Ptr TreeView) -> ((Ptr NativeTreePath) -> ((Ptr TreeViewColumn) -> (CInt -> (CFloat -> (CFloat -> (IO ())))))))

foreign import ccall safe "gtk_tree_view_set_cursor"
  gtk_tree_view_set_cursor :: ((Ptr TreeView) -> ((Ptr NativeTreePath) -> ((Ptr TreeViewColumn) -> (CInt -> (IO ())))))

foreign import ccall safe "gtk_tree_view_set_cursor_on_cell"
  gtk_tree_view_set_cursor_on_cell :: ((Ptr TreeView) -> ((Ptr NativeTreePath) -> ((Ptr TreeViewColumn) -> ((Ptr CellRenderer) -> (CInt -> (IO ()))))))

foreign import ccall unsafe "gtk_tree_view_get_cursor"
  gtk_tree_view_get_cursor :: ((Ptr TreeView) -> ((Ptr NativeTreePath) -> ((Ptr TreeViewColumn) -> (IO ()))))

foreign import ccall safe "gtk_tree_view_row_activated"
  gtk_tree_view_row_activated :: ((Ptr TreeView) -> ((Ptr NativeTreePath) -> ((Ptr TreeViewColumn) -> (IO ()))))

foreign import ccall safe "gtk_tree_view_expand_all"
  gtk_tree_view_expand_all :: ((Ptr TreeView) -> (IO ()))

foreign import ccall safe "gtk_tree_view_collapse_all"
  gtk_tree_view_collapse_all :: ((Ptr TreeView) -> (IO ()))

foreign import ccall safe "gtk_tree_view_expand_to_path"
  gtk_tree_view_expand_to_path :: ((Ptr TreeView) -> ((Ptr NativeTreePath) -> (IO ())))

foreign import ccall safe "gtk_tree_view_expand_row"
  gtk_tree_view_expand_row :: ((Ptr TreeView) -> ((Ptr NativeTreePath) -> (CInt -> (IO CInt))))

foreign import ccall safe "gtk_tree_view_collapse_row"
  gtk_tree_view_collapse_row :: ((Ptr TreeView) -> ((Ptr NativeTreePath) -> (IO CInt)))

foreign import ccall safe "gtk_tree_view_map_expanded_rows"
  gtk_tree_view_map_expanded_rows :: ((Ptr TreeView) -> ((FunPtr ((Ptr TreeView) -> ((Ptr NativeTreePath) -> ((Ptr ()) -> (IO ()))))) -> ((Ptr ()) -> (IO ()))))

foreign import ccall unsafe "gtk_tree_view_row_expanded"
  gtk_tree_view_row_expanded :: ((Ptr TreeView) -> ((Ptr NativeTreePath) -> (IO CInt)))

foreign import ccall unsafe "gtk_tree_view_get_reorderable"
  gtk_tree_view_get_reorderable :: ((Ptr TreeView) -> (IO CInt))

foreign import ccall safe "gtk_tree_view_set_reorderable"
  gtk_tree_view_set_reorderable :: ((Ptr TreeView) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_tree_view_get_path_at_pos"
  gtk_tree_view_get_path_at_pos :: ((Ptr TreeView) -> (CInt -> (CInt -> ((Ptr NativeTreePath) -> ((Ptr TreeViewColumn) -> ((Ptr CInt) -> ((Ptr CInt) -> (IO CInt))))))))

foreign import ccall unsafe "gtk_tree_view_get_cell_area"
  gtk_tree_view_get_cell_area :: ((Ptr TreeView) -> ((Ptr NativeTreePath) -> ((Ptr TreeViewColumn) -> ((Ptr ()) -> (IO ())))))

foreign import ccall unsafe "gtk_tree_view_get_background_area"
  gtk_tree_view_get_background_area :: ((Ptr TreeView) -> ((Ptr NativeTreePath) -> ((Ptr TreeViewColumn) -> ((Ptr ()) -> (IO ())))))

foreign import ccall unsafe "gtk_tree_view_get_visible_rect"
  gtk_tree_view_get_visible_rect :: ((Ptr TreeView) -> ((Ptr ()) -> (IO ())))

foreign import ccall unsafe "gtk_tree_view_tree_to_widget_coords"
  gtk_tree_view_tree_to_widget_coords :: ((Ptr TreeView) -> (CInt -> (CInt -> ((Ptr CInt) -> ((Ptr CInt) -> (IO ()))))))

foreign import ccall unsafe "gtk_tree_view_widget_to_tree_coords"
  gtk_tree_view_widget_to_tree_coords :: ((Ptr TreeView) -> (CInt -> (CInt -> ((Ptr CInt) -> ((Ptr CInt) -> (IO ()))))))

foreign import ccall unsafe "gtk_tree_view_convert_bin_window_to_tree_coords"
  gtk_tree_view_convert_bin_window_to_tree_coords :: ((Ptr TreeView) -> (CInt -> (CInt -> ((Ptr CInt) -> ((Ptr CInt) -> (IO ()))))))

foreign import ccall unsafe "gtk_tree_view_convert_bin_window_to_widget_coords"
  gtk_tree_view_convert_bin_window_to_widget_coords :: ((Ptr TreeView) -> (CInt -> (CInt -> ((Ptr CInt) -> ((Ptr CInt) -> (IO ()))))))

foreign import ccall unsafe "gtk_tree_view_convert_tree_to_bin_window_coords"
  gtk_tree_view_convert_tree_to_bin_window_coords :: ((Ptr TreeView) -> (CInt -> (CInt -> ((Ptr CInt) -> ((Ptr CInt) -> (IO ()))))))

foreign import ccall unsafe "gtk_tree_view_convert_tree_to_widget_coords"
  gtk_tree_view_convert_tree_to_widget_coords :: ((Ptr TreeView) -> (CInt -> (CInt -> ((Ptr CInt) -> ((Ptr CInt) -> (IO ()))))))

foreign import ccall unsafe "gtk_tree_view_convert_widget_to_bin_window_coords"
  gtk_tree_view_convert_widget_to_bin_window_coords :: ((Ptr TreeView) -> (CInt -> (CInt -> ((Ptr CInt) -> ((Ptr CInt) -> (IO ()))))))

foreign import ccall unsafe "gtk_tree_view_convert_widget_to_tree_coords"
  gtk_tree_view_convert_widget_to_tree_coords :: ((Ptr TreeView) -> (CInt -> (CInt -> ((Ptr CInt) -> ((Ptr CInt) -> (IO ()))))))

foreign import ccall unsafe "gtk_tree_view_create_row_drag_icon"
  gtk_tree_view_create_row_drag_icon :: ((Ptr TreeView) -> ((Ptr NativeTreePath) -> (IO (Ptr Pixmap))))

foreign import ccall unsafe "gtk_tree_view_get_enable_search"
  gtk_tree_view_get_enable_search :: ((Ptr TreeView) -> (IO CInt))

foreign import ccall safe "gtk_tree_view_set_enable_search"
  gtk_tree_view_set_enable_search :: ((Ptr TreeView) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_tree_view_get_search_column"
  gtk_tree_view_get_search_column :: ((Ptr TreeView) -> (IO CInt))

foreign import ccall safe "gtk_tree_view_set_search_column"
  gtk_tree_view_set_search_column :: ((Ptr TreeView) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_tree_view_set_search_equal_func"
  gtk_tree_view_set_search_equal_func :: ((Ptr TreeView) -> ((FunPtr ((Ptr TreeModel) -> (CInt -> ((Ptr CChar) -> ((Ptr TreeIter) -> ((Ptr ()) -> (IO CInt))))))) -> ((Ptr ()) -> ((FunPtr ((Ptr ()) -> (IO ()))) -> (IO ())))))

foreign import ccall safe "gtk_tree_view_get_fixed_height_mode"
  gtk_tree_view_get_fixed_height_mode :: ((Ptr TreeView) -> (IO CInt))

foreign import ccall safe "gtk_tree_view_set_fixed_height_mode"
  gtk_tree_view_set_fixed_height_mode :: ((Ptr TreeView) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_tree_view_get_hover_selection"
  gtk_tree_view_get_hover_selection :: ((Ptr TreeView) -> (IO CInt))

foreign import ccall safe "gtk_tree_view_set_hover_selection"
  gtk_tree_view_set_hover_selection :: ((Ptr TreeView) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_tree_view_get_hover_expand"
  gtk_tree_view_get_hover_expand :: ((Ptr TreeView) -> (IO CInt))

foreign import ccall safe "gtk_tree_view_set_hover_expand"
  gtk_tree_view_set_hover_expand :: ((Ptr TreeView) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_tree_view_get_headers_clickable"
  gtk_tree_view_get_headers_clickable :: ((Ptr TreeView) -> (IO CInt))

foreign import ccall safe "gtk_tree_view_get_visible_range"
  gtk_tree_view_get_visible_range :: ((Ptr TreeView) -> ((Ptr NativeTreePath) -> ((Ptr NativeTreePath) -> (IO CInt))))

foreign import ccall unsafe "gtk_target_table_new_from_list"
  gtk_target_table_new_from_list :: ((Ptr TargetList) -> ((Ptr CInt) -> (IO (Ptr ()))))

foreign import ccall safe "gtk_tree_view_enable_model_drag_dest"
  gtk_tree_view_enable_model_drag_dest :: ((Ptr TreeView) -> ((Ptr ()) -> (CInt -> (CInt -> (IO ())))))

foreign import ccall unsafe "gtk_target_table_free"
  gtk_target_table_free :: ((Ptr ()) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_tree_view_enable_model_drag_source"
  gtk_tree_view_enable_model_drag_source :: ((Ptr TreeView) -> (CInt -> ((Ptr ()) -> (CInt -> (CInt -> (IO ()))))))

foreign import ccall safe "gtk_tree_view_unset_rows_drag_source"
  gtk_tree_view_unset_rows_drag_source :: ((Ptr TreeView) -> (IO ()))

foreign import ccall safe "gtk_tree_view_unset_rows_drag_dest"
  gtk_tree_view_unset_rows_drag_dest :: ((Ptr TreeView) -> (IO ()))

foreign import ccall safe "gtk_tree_view_get_search_entry"
  gtk_tree_view_get_search_entry :: ((Ptr TreeView) -> (IO (Ptr Entry)))

foreign import ccall safe "gtk_tree_view_set_search_entry"
  gtk_tree_view_set_search_entry :: ((Ptr TreeView) -> ((Ptr Entry) -> (IO ())))

foreign import ccall safe "gtk_tree_view_set_row_separator_func"
  gtk_tree_view_set_row_separator_func :: ((Ptr TreeView) -> ((FunPtr ((Ptr TreeModel) -> ((Ptr TreeIter) -> ((Ptr ()) -> (IO CInt))))) -> ((Ptr ()) -> ((FunPtr ((Ptr ()) -> (IO ()))) -> (IO ())))))

foreign import ccall safe "gtk_tree_view_get_rubber_banding"
  gtk_tree_view_get_rubber_banding :: ((Ptr TreeView) -> (IO CInt))

foreign import ccall safe "gtk_tree_view_set_rubber_banding"
  gtk_tree_view_set_rubber_banding :: ((Ptr TreeView) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_tree_view_get_enable_tree_lines"
  gtk_tree_view_get_enable_tree_lines :: ((Ptr TreeView) -> (IO CInt))

foreign import ccall safe "gtk_tree_view_set_enable_tree_lines"
  gtk_tree_view_set_enable_tree_lines :: ((Ptr TreeView) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_tree_view_get_grid_lines"
  gtk_tree_view_get_grid_lines :: ((Ptr TreeView) -> (IO CInt))

foreign import ccall safe "gtk_tree_view_set_grid_lines"
  gtk_tree_view_set_grid_lines :: ((Ptr TreeView) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_tree_view_set_tooltip_row"
  gtk_tree_view_set_tooltip_row :: ((Ptr TreeView) -> ((Ptr Tooltip) -> ((Ptr NativeTreePath) -> (IO ()))))

foreign import ccall safe "gtk_tree_view_set_tooltip_cell"
  gtk_tree_view_set_tooltip_cell :: ((Ptr TreeView) -> ((Ptr Tooltip) -> ((Ptr NativeTreePath) -> ((Ptr TreeViewColumn) -> ((Ptr CellRenderer) -> (IO ()))))))

foreign import ccall safe "gtk_tree_view_get_tooltip_context"
  gtk_tree_view_get_tooltip_context :: ((Ptr TreeView) -> ((Ptr CInt) -> ((Ptr CInt) -> (CInt -> ((Ptr TreeModel) -> ((Ptr NativeTreePath) -> ((Ptr TreeIter) -> (IO CInt))))))))

foreign import ccall unsafe "gtk_tree_view_grid_lines_get_type"
  gtk_tree_view_grid_lines_get_type :: CULong

foreign import ccall unsafe "gtk_tree_view_get_tooltip_column"
  gtk_tree_view_get_tooltip_column :: ((Ptr TreeView) -> (IO CInt))

foreign import ccall safe "gtk_tree_view_set_tooltip_column"
  gtk_tree_view_set_tooltip_column :: ((Ptr TreeView) -> (CInt -> (IO ())))
