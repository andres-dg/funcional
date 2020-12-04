
{-# LINE 2 "./Graphics/UI/Gtk/ModelView/TreeViewColumn.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) TreeViewColumn TreeView
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
-- |
-- Maintainer : gtk2hs-users@lists.sourceforge.net
-- Stability : provisional
-- Portability : portable (depends on GHC)
--
-- A visible column in a 'TreeView' widget
--
module Graphics.UI.Gtk.ModelView.TreeViewColumn (
-- * Detail
--
-- | The 'TreeViewColumn' object represents a visible column in a 'TreeView'
-- widget. It allows to set properties of the column header, and functions as a
-- holding pen for the cell renderers which determine how the data in the
-- column is displayed.
--

-- * Class Hierarchy
-- |
-- @
-- | 'GObject'
-- | +----'Object'
-- | +----TreeViewColumn
-- @

-- * Types
  TreeViewColumn,
  TreeViewColumnClass,
  castToTreeViewColumn, gTypeTreeViewColumn,
  toTreeViewColumn,

-- * Constructors
  treeViewColumnNew,

-- * Methods
  treeViewColumnPackStart,
  treeViewColumnPackEnd,
  treeViewColumnClear,

  treeViewColumnGetCellRenderers,

  treeViewColumnSetSpacing,
  treeViewColumnGetSpacing,
  treeViewColumnSetVisible,
  treeViewColumnGetVisible,
  treeViewColumnSetResizable,
  treeViewColumnGetResizable,
  TreeViewColumnSizing(..),
  treeViewColumnSetSizing,
  treeViewColumnGetSizing,
  treeViewColumnGetWidth,
  treeViewColumnSetFixedWidth,
  treeViewColumnGetFixedWidth,
  treeViewColumnSetMinWidth,
  treeViewColumnGetMinWidth,
  treeViewColumnSetMaxWidth,
  treeViewColumnGetMaxWidth,
  treeViewColumnClicked,
  treeViewColumnSetTitle,
  treeViewColumnGetTitle,
  treeViewColumnSetClickable,
  treeViewColumnGetClickable,
  treeViewColumnSetWidget,
  treeViewColumnGetWidget,
  treeViewColumnSetAlignment,
  treeViewColumnGetAlignment,
  treeViewColumnSetReorderable,
  treeViewColumnGetReorderable,
  treeViewColumnSetSortColumnId,
  treeViewColumnGetSortColumnId,
  treeViewColumnSetSortIndicator,
  treeViewColumnGetSortIndicator,
  treeViewColumnSetSortOrder,
  treeViewColumnGetSortOrder,
  SortType(..),

  treeViewColumnSetExpand,
  treeViewColumnGetExpand,

  treeViewColumnCellIsVisible,

  treeViewColumnFocusCell,

  treeViewColumnQueueResize,



-- * Attributes
  treeViewColumnVisible,
  treeViewColumnResizable,
  treeViewColumnWidth,
  treeViewColumnSpacing,
  treeViewColumnSizing,
  treeViewColumnFixedWidth,
  treeViewColumnMinWidth,
  treeViewColumnMaxWidth,
  treeViewColumnTitle,
  treeViewColumnExpand,
  treeViewColumnClickable,
  treeViewColumnWidget,
  treeViewColumnAlignment,
  treeViewColumnReorderable,
  treeViewColumnSortIndicator,
  treeViewColumnSortOrder,
  treeViewColumnSortColumnId,

-- * Signals
  onColClicked,
  afterColClicked
  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.UTFString

import System.Glib.GList (fromGList)

import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.UI.Gtk.Types
{-# LINE 142 "./Graphics/UI/Gtk/ModelView/TreeViewColumn.chs" #-}
import Graphics.UI.Gtk.Signals
{-# LINE 143 "./Graphics/UI/Gtk/ModelView/TreeViewColumn.chs" #-}
import Graphics.UI.Gtk.General.Enums (TreeViewColumnSizing(..),
                                                 SortType(..))
import Graphics.UI.Gtk.General.Structs (SortColumnId)
import Graphics.UI.Gtk.ModelView.TreeModel ()


{-# LINE 149 "./Graphics/UI/Gtk/ModelView/TreeViewColumn.chs" #-}

--------------------
-- Constructors

-- | Generate a new TreeViewColumn widget.
--
treeViewColumnNew :: IO TreeViewColumn
treeViewColumnNew = makeNewObject mkTreeViewColumn
  gtk_tree_view_column_new
{-# LINE 158 "./Graphics/UI/Gtk/ModelView/TreeViewColumn.chs" #-}


--------------------
-- Methods

-- | Add a cell renderer at the beginning of a column.
--
-- * Excess space is divided equally among all renderers which have
-- @expand@ set to True.
--
treeViewColumnPackStart :: CellRendererClass cell => TreeViewColumn
 -> cell
 -> Bool
 -> IO ()
treeViewColumnPackStart self cell expand =
  (\(TreeViewColumn arg1) (CellRenderer arg2) arg3 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_tree_view_column_pack_start argPtr1 argPtr2 arg3)
{-# LINE 174 "./Graphics/UI/Gtk/ModelView/TreeViewColumn.chs" #-}
    self
    (toCellRenderer cell)
    (fromBool expand)

-- | Add a cell renderer at the end of a column.
--
-- * Excess space is divided equally among all renderers which have
-- @expand@ set to True.
--
treeViewColumnPackEnd :: CellRendererClass cell => TreeViewColumn
 -> cell
 -> Bool
 -> IO ()
treeViewColumnPackEnd self cell expand =
  (\(TreeViewColumn arg1) (CellRenderer arg2) arg3 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_tree_view_column_pack_end argPtr1 argPtr2 arg3)
{-# LINE 189 "./Graphics/UI/Gtk/ModelView/TreeViewColumn.chs" #-}
    self
    (toCellRenderer cell)
    (fromBool expand)

-- | Remove the associations of attributes to a store for all 'CellRenderer's.
--
treeViewColumnClear :: TreeViewColumn -> IO ()
treeViewColumnClear self =
  (\(TreeViewColumn arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_column_clear argPtr1)
{-# LINE 198 "./Graphics/UI/Gtk/ModelView/TreeViewColumn.chs" #-}
    self


-- | Retrieve all 'CellRenderer's that are contained in this column.
--
-- Removed in Gtk3.
treeViewColumnGetCellRenderers :: TreeViewColumn -> IO [CellRenderer]
treeViewColumnGetCellRenderers self =
  (\(TreeViewColumn arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_column_get_cell_renderers argPtr1)
{-# LINE 207 "./Graphics/UI/Gtk/ModelView/TreeViewColumn.chs" #-}
    self
  >>= fromGList
  >>= mapM (makeNewObject mkCellRenderer . return)


-- | Set the number of pixels between two cell renderers.
--
treeViewColumnSetSpacing :: TreeViewColumn -> Int -> IO ()
treeViewColumnSetSpacing self spacing =
  (\(TreeViewColumn arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_column_set_spacing argPtr1 arg2)
{-# LINE 217 "./Graphics/UI/Gtk/ModelView/TreeViewColumn.chs" #-}
    self
    (fromIntegral spacing)


-- | Get the number of pixels between two cell renderers.
--
treeViewColumnGetSpacing :: TreeViewColumn -> IO Int
treeViewColumnGetSpacing self =
  liftM fromIntegral $
  (\(TreeViewColumn arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_column_get_spacing argPtr1)
{-# LINE 227 "./Graphics/UI/Gtk/ModelView/TreeViewColumn.chs" #-}
    self

-- | Set the visibility of a given column.
--
treeViewColumnSetVisible :: TreeViewColumn -> Bool -> IO ()
treeViewColumnSetVisible self visible =
  (\(TreeViewColumn arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_column_set_visible argPtr1 arg2)
{-# LINE 234 "./Graphics/UI/Gtk/ModelView/TreeViewColumn.chs" #-}
    self
    (fromBool visible)

-- | Get the visibility of a given column.
--
treeViewColumnGetVisible :: TreeViewColumn -> IO Bool
treeViewColumnGetVisible self =
  liftM toBool $
  (\(TreeViewColumn arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_column_get_visible argPtr1)
{-# LINE 243 "./Graphics/UI/Gtk/ModelView/TreeViewColumn.chs" #-}
    self

-- | Set if a given column is resizable by the user.
--
treeViewColumnSetResizable :: TreeViewColumn -> Bool -> IO ()
treeViewColumnSetResizable self resizable =
  (\(TreeViewColumn arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_column_set_resizable argPtr1 arg2)
{-# LINE 250 "./Graphics/UI/Gtk/ModelView/TreeViewColumn.chs" #-}
    self
    (fromBool resizable)

-- | Get if a given column is resizable by the user.
--
treeViewColumnGetResizable :: TreeViewColumn -> IO Bool
treeViewColumnGetResizable self =
  liftM toBool $
  (\(TreeViewColumn arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_column_get_resizable argPtr1)
{-# LINE 259 "./Graphics/UI/Gtk/ModelView/TreeViewColumn.chs" #-}
    self

-- | Set wether the column can be resized.
--
treeViewColumnSetSizing :: TreeViewColumn
 -> TreeViewColumnSizing
 -> IO ()
treeViewColumnSetSizing self type_ =
  (\(TreeViewColumn arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_column_set_sizing argPtr1 arg2)
{-# LINE 268 "./Graphics/UI/Gtk/ModelView/TreeViewColumn.chs" #-}
    self
    ((fromIntegral . fromEnum) type_)

-- | Return the resizing type of the column.
--
treeViewColumnGetSizing :: TreeViewColumn
 -> IO TreeViewColumnSizing
treeViewColumnGetSizing self =
  liftM (toEnum . fromIntegral) $
  (\(TreeViewColumn arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_column_get_sizing argPtr1)
{-# LINE 278 "./Graphics/UI/Gtk/ModelView/TreeViewColumn.chs" #-}
    self

-- | Query the current width of the column.
--
treeViewColumnGetWidth :: TreeViewColumn -> IO Int
treeViewColumnGetWidth self =
  liftM fromIntegral $
  (\(TreeViewColumn arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_column_get_width argPtr1)
{-# LINE 286 "./Graphics/UI/Gtk/ModelView/TreeViewColumn.chs" #-}
    self

-- | Set the width of the column.
--
-- * This is meaningful only if the sizing type is 'TreeViewColumnFixed'.
--
treeViewColumnSetFixedWidth :: TreeViewColumn -> Int -> IO ()
treeViewColumnSetFixedWidth self fixedWidth =
  (\(TreeViewColumn arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_column_set_fixed_width argPtr1 arg2)
{-# LINE 295 "./Graphics/UI/Gtk/ModelView/TreeViewColumn.chs" #-}
    self
    (fromIntegral fixedWidth)

-- | Gets the fixed width of the column.
--
-- * This is meaningful only if the sizing type is 'TreeViewColumnFixed'.
--
-- * This value is only meaning may not be the actual width of the column on the
-- screen, just what is requested.
--
treeViewColumnGetFixedWidth :: TreeViewColumn -> IO Int
treeViewColumnGetFixedWidth self =
  liftM fromIntegral $
  (\(TreeViewColumn arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_column_get_fixed_width argPtr1)
{-# LINE 309 "./Graphics/UI/Gtk/ModelView/TreeViewColumn.chs" #-}
    self

-- | Set minimum width of the column.
--
treeViewColumnSetMinWidth :: TreeViewColumn -> Int -> IO ()
treeViewColumnSetMinWidth self minWidth =
  (\(TreeViewColumn arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_column_set_min_width argPtr1 arg2)
{-# LINE 316 "./Graphics/UI/Gtk/ModelView/TreeViewColumn.chs" #-}
    self
    (fromIntegral minWidth)

-- | Get the minimum width of a column. Returns -1 if this width was not set.
--
treeViewColumnGetMinWidth :: TreeViewColumn -> IO Int
treeViewColumnGetMinWidth self =
  liftM fromIntegral $
  (\(TreeViewColumn arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_column_get_min_width argPtr1)
{-# LINE 325 "./Graphics/UI/Gtk/ModelView/TreeViewColumn.chs" #-}
    self

-- | Set maximum width of the column.
--
treeViewColumnSetMaxWidth :: TreeViewColumn -> Int -> IO ()
treeViewColumnSetMaxWidth self maxWidth =
  (\(TreeViewColumn arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_column_set_max_width argPtr1 arg2)
{-# LINE 332 "./Graphics/UI/Gtk/ModelView/TreeViewColumn.chs" #-}
    self
    (fromIntegral maxWidth)

-- | Get the maximum width of a column. Returns -1 if this width was not set.
--
treeViewColumnGetMaxWidth :: TreeViewColumn -> IO Int
treeViewColumnGetMaxWidth self =
  liftM fromIntegral $
  (\(TreeViewColumn arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_column_get_max_width argPtr1)
{-# LINE 341 "./Graphics/UI/Gtk/ModelView/TreeViewColumn.chs" #-}
    self

-- | Emit the @clicked@ signal on the column.
--
treeViewColumnClicked :: TreeViewColumn -> IO ()
treeViewColumnClicked self =
  (\(TreeViewColumn arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_column_clicked argPtr1)
{-# LINE 348 "./Graphics/UI/Gtk/ModelView/TreeViewColumn.chs" #-}
    self

-- | Set the widget's title if a custom widget has not been set.
--
treeViewColumnSetTitle :: GlibString string => TreeViewColumn -> string -> IO ()
treeViewColumnSetTitle self title =
  withUTFString title $ \titlePtr ->
  (\(TreeViewColumn arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_column_set_title argPtr1 arg2)
{-# LINE 356 "./Graphics/UI/Gtk/ModelView/TreeViewColumn.chs" #-}
    self
    titlePtr

-- | Get the widget's title.
--
treeViewColumnGetTitle :: GlibString string => TreeViewColumn -> IO (Maybe string)
treeViewColumnGetTitle self =
  (\(TreeViewColumn arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_column_get_title argPtr1)
{-# LINE 364 "./Graphics/UI/Gtk/ModelView/TreeViewColumn.chs" #-}
    self
  >>= maybePeek peekUTFString

-- | Set if the column should be sensitive to mouse clicks.
--
treeViewColumnSetClickable :: TreeViewColumn -> Bool -> IO ()
treeViewColumnSetClickable self clickable =
  (\(TreeViewColumn arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_column_set_clickable argPtr1 arg2)
{-# LINE 372 "./Graphics/UI/Gtk/ModelView/TreeViewColumn.chs" #-}
    self
    (fromBool clickable)

-- | Returns True if the user can click on the header for the column.
--
treeViewColumnGetClickable :: TreeViewColumn -> IO Bool
treeViewColumnGetClickable self =
  liftM toBool $
  (\(TreeViewColumn arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_column_get_clickable argPtr1)
{-# LINE 381 "./Graphics/UI/Gtk/ModelView/TreeViewColumn.chs" #-}
    self

-- | Set the column's title to this widget.
--
treeViewColumnSetWidget :: WidgetClass widget => TreeViewColumn
 -> Maybe widget
 -> IO ()
treeViewColumnSetWidget self widget =
  (\(TreeViewColumn arg1) (Widget arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_tree_view_column_set_widget argPtr1 argPtr2)
{-# LINE 390 "./Graphics/UI/Gtk/ModelView/TreeViewColumn.chs" #-}
    self
    (maybe (Widget nullForeignPtr) toWidget widget)

-- | Retrieve the widget responsible for
-- showing the column title. In case only a text title was set this will be a
-- 'Alignment' widget with a 'Label' inside.
--
treeViewColumnGetWidget :: TreeViewColumn
 -> IO (Maybe Widget) -- ^ returns the 'Widget' in the column header, or 'Nothing'
treeViewColumnGetWidget self = do
  widgetPtr <- (\(TreeViewColumn arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_column_get_widget argPtr1) self
  if widgetPtr == nullPtr
     then return Nothing
     else liftM Just $ makeNewObject mkWidget (return widgetPtr)

-- | Sets the alignment of the title or custom widget inside the column
-- header. The alignment determines its location inside the button -- 0.0 for
-- left, 0.5 for center, 1.0 for right.
--
treeViewColumnSetAlignment :: TreeViewColumn
 -> Float -- ^ @xalign@ - The alignment, which is between [0.0 and
                   -- 1.0] inclusive.
 -> IO ()
treeViewColumnSetAlignment self xalign =
  (\(TreeViewColumn arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_column_set_alignment argPtr1 arg2)
{-# LINE 415 "./Graphics/UI/Gtk/ModelView/TreeViewColumn.chs" #-}
    self
    (realToFrac xalign)

-- | Returns the current x alignment of the tree column. This value can range
-- between 0.0 and 1.0.
--
treeViewColumnGetAlignment :: TreeViewColumn -> IO Float
treeViewColumnGetAlignment self =
  liftM realToFrac $
  (\(TreeViewColumn arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_column_get_alignment argPtr1)
{-# LINE 425 "./Graphics/UI/Gtk/ModelView/TreeViewColumn.chs" #-}
    self

-- | Set if the column can be reordered by the end user dragging the header.
--
treeViewColumnSetReorderable :: TreeViewColumn -> Bool -> IO ()
treeViewColumnSetReorderable self reorderable =
  (\(TreeViewColumn arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_column_set_reorderable argPtr1 arg2)
{-# LINE 432 "./Graphics/UI/Gtk/ModelView/TreeViewColumn.chs" #-}
    self
    (fromBool reorderable)

-- | Returns whether the column can be reordered by the user.
--
treeViewColumnGetReorderable :: TreeViewColumn -> IO Bool
treeViewColumnGetReorderable self =
  liftM toBool $
  (\(TreeViewColumn arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_column_get_reorderable argPtr1)
{-# LINE 441 "./Graphics/UI/Gtk/ModelView/TreeViewColumn.chs" #-}
    self

-- | Set the column by which to sort.
--
-- * Sets the logical @columnId@ that this column sorts on when
-- this column is selected for sorting. The selected column's header
-- will be clickable after this call. Logical refers to the
-- 'Graphics.UI.Gtk.ModelView.TreeSortable.SortColumnId' for which
-- a comparison function was set.
--
treeViewColumnSetSortColumnId :: TreeViewColumn -> SortColumnId -> IO ()
treeViewColumnSetSortColumnId self sortColumnId =
  (\(TreeViewColumn arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_column_set_sort_column_id argPtr1 arg2)
{-# LINE 454 "./Graphics/UI/Gtk/ModelView/TreeViewColumn.chs" #-}
    self
    (fromIntegral sortColumnId)

-- | Get the column by which to sort.
--
-- * Retrieves the logical @columnId@ that the model sorts on when this column
-- is selected for sorting.
--
-- * Returns
-- 'Graphics.UI.Gtk.ModelView.TreeSortable.treeSortableDefaultSortColumnId'
-- if this tree view column has no
-- 'Graphics.UI.Gtk.ModelView.TreeSortable.SortColumnId' associated with it.
--
treeViewColumnGetSortColumnId :: TreeViewColumn -> IO SortColumnId
treeViewColumnGetSortColumnId self =
  liftM fromIntegral $
  (\(TreeViewColumn arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_column_get_sort_column_id argPtr1)
{-# LINE 471 "./Graphics/UI/Gtk/ModelView/TreeViewColumn.chs" #-}
    self

-- | Set if a given column has sorting arrows in its heading.
--
treeViewColumnSetSortIndicator :: TreeViewColumn
 -> Bool -> IO ()
treeViewColumnSetSortIndicator self setting =
  (\(TreeViewColumn arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_column_set_sort_indicator argPtr1 arg2)
{-# LINE 479 "./Graphics/UI/Gtk/ModelView/TreeViewColumn.chs" #-}
    self
    (fromBool setting)

-- | Query if a given column has sorting arrows in its heading.
--
treeViewColumnGetSortIndicator :: TreeViewColumn -> IO Bool
treeViewColumnGetSortIndicator self =
  liftM toBool $
  (\(TreeViewColumn arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_column_get_sort_indicator argPtr1)
{-# LINE 488 "./Graphics/UI/Gtk/ModelView/TreeViewColumn.chs" #-}
    self

-- | Set if a given column is sorted in ascending or descending order.
--
-- * In order for sorting to work, it is necessary to either use automatic
-- sorting via 'treeViewColumnSetSortColumnId' or to use a
-- user defined sorting on the elements in a 'TreeModel'.
--
treeViewColumnSetSortOrder :: TreeViewColumn
 -> SortType -> IO ()
treeViewColumnSetSortOrder self order =
  (\(TreeViewColumn arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_column_set_sort_order argPtr1 arg2)
{-# LINE 500 "./Graphics/UI/Gtk/ModelView/TreeViewColumn.chs" #-}
    self
    ((fromIntegral . fromEnum) order)

-- | Query if a given column is sorted in ascending or descending order.
--
treeViewColumnGetSortOrder :: TreeViewColumn -> IO SortType
treeViewColumnGetSortOrder self =
  liftM (toEnum . fromIntegral) $
  (\(TreeViewColumn arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_column_get_sort_order argPtr1)
{-# LINE 509 "./Graphics/UI/Gtk/ModelView/TreeViewColumn.chs" #-}
    self


-- %hash c:7808 d:942b
-- | Sets the column to take available extra space. This space is shared
-- equally amongst all columns that have the expand set to @True@. If no column
-- has this option set, then the last column gets all extra space. By default,
-- every column is created with this @False@.
--
-- * Available since Gtk+ version 2.4
--
treeViewColumnSetExpand :: TreeViewColumn
 -> Bool -- ^ @expand@ - @True@ if the column should take available extra
         -- space, @False@ if not
 -> IO ()
treeViewColumnSetExpand self expand =
  (\(TreeViewColumn arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_column_set_expand argPtr1 arg2)
{-# LINE 526 "./Graphics/UI/Gtk/ModelView/TreeViewColumn.chs" #-}
    self
    (fromBool expand)

-- %hash c:ee41 d:f16b
-- | Return @True@ if the column expands to take any available space.
--
-- * Available since Gtk+ version 2.4
--
treeViewColumnGetExpand :: TreeViewColumn
 -> IO Bool -- ^ returns @True@, if the column expands
treeViewColumnGetExpand self =
  liftM toBool $
  (\(TreeViewColumn arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_column_get_expand argPtr1)
{-# LINE 539 "./Graphics/UI/Gtk/ModelView/TreeViewColumn.chs" #-}
    self


-- %hash c:77e0 d:e1c7
-- | Returns @True@ if any of the cells packed into the @treeColumn@ are
-- visible. For this to be meaningful, you must first initialize the cells with
-- 'treeViewColumnCellSetCellData'
--
treeViewColumnCellIsVisible :: TreeViewColumn
 -> IO Bool -- ^ returns @True@, if any of the cells packed into the
            -- @treeColumn@ are currently visible
treeViewColumnCellIsVisible self =
  liftM toBool $
  (\(TreeViewColumn arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_column_cell_is_visible argPtr1)
{-# LINE 553 "./Graphics/UI/Gtk/ModelView/TreeViewColumn.chs" #-}
    self


-- %hash c:a202 d:1401
-- | Sets the current keyboard focus to be at @cell@, if the column contains 2
-- or more editable and activatable cells.
--
-- * Available since Gtk+ version 2.2
--
treeViewColumnFocusCell :: CellRendererClass cell => TreeViewColumn
 -> cell -- ^ @cell@ - A 'CellRenderer'
 -> IO ()
treeViewColumnFocusCell self cell =
  (\(TreeViewColumn arg1) (CellRenderer arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_tree_view_column_focus_cell argPtr1 argPtr2)
{-# LINE 567 "./Graphics/UI/Gtk/ModelView/TreeViewColumn.chs" #-}
    self
    (toCellRenderer cell)


-- %hash c:4420 d:bfde
-- | Flags the column, and the cell renderers added to this column, to have
-- their sizes renegotiated.
--
-- * Available since Gtk+ version 2.8
--
treeViewColumnQueueResize :: TreeViewColumn -> IO ()
treeViewColumnQueueResize self =
  (\(TreeViewColumn arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_tree_view_column_queue_resize argPtr1)
{-# LINE 580 "./Graphics/UI/Gtk/ModelView/TreeViewColumn.chs" #-}
    self



--------------------
-- Attributes

-- | Whether to display the column.
--
-- Default value: @True@
--
treeViewColumnVisible :: Attr TreeViewColumn Bool
treeViewColumnVisible = newAttr
  treeViewColumnGetVisible
  treeViewColumnSetVisible

-- | Column is user-resizable.
--
-- Default value: @False@
--
treeViewColumnResizable :: Attr TreeViewColumn Bool
treeViewColumnResizable = newAttr
  treeViewColumnGetResizable
  treeViewColumnSetResizable

-- | Current width of the column.
--
-- Allowed values: >= 0
--
-- Default value: 0
--
treeViewColumnWidth :: ReadAttr TreeViewColumn Int
treeViewColumnWidth = readAttrFromIntProperty "width"

-- | Space which is inserted between cells.
--
-- Allowed values: >= 0
--
-- Default value: 0
--
treeViewColumnSpacing :: Attr TreeViewColumn Int
treeViewColumnSpacing = newAttr
  treeViewColumnGetSpacing
  treeViewColumnSetSpacing

-- | Resize mode of the column.
--
-- Default value: 'TreeViewColumnGrowOnly'
--
treeViewColumnSizing :: Attr TreeViewColumn TreeViewColumnSizing
treeViewColumnSizing = newAttr
  treeViewColumnGetSizing
  treeViewColumnSetSizing

-- | Current fixed width of the column.
--
-- Allowed values: >= 1
--
-- Default value: 1
--
treeViewColumnFixedWidth :: Attr TreeViewColumn Int
treeViewColumnFixedWidth = newAttr
  treeViewColumnGetFixedWidth
  treeViewColumnSetFixedWidth

-- | Minimum allowed width of the column.
--
-- Allowed values: >= -1
--
-- Default value: -1
--
treeViewColumnMinWidth :: Attr TreeViewColumn Int
treeViewColumnMinWidth = newAttr
  treeViewColumnGetMinWidth
  treeViewColumnSetMinWidth

-- | Maximum allowed width of the column.
--
-- Allowed values: >= -1
--
-- Default value: -1
--
treeViewColumnMaxWidth :: Attr TreeViewColumn Int
treeViewColumnMaxWidth = newAttr
  treeViewColumnGetMaxWidth
  treeViewColumnSetMaxWidth

-- | Title to appear in column header.
--
-- Default value: \"\"
--
treeViewColumnTitle :: GlibString string => ReadWriteAttr TreeViewColumn (Maybe string) string
treeViewColumnTitle = newAttr
  treeViewColumnGetTitle
  treeViewColumnSetTitle

-- %hash c:800 d:eb1a
-- | Column gets share of extra width allocated to the widget.
--
-- Default value: @False@
--
treeViewColumnExpand :: Attr TreeViewColumn Bool
treeViewColumnExpand = newAttrFromBoolProperty "expand"

-- | Whether the header can be clicked.
--
-- Default value: @False@
--
treeViewColumnClickable :: Attr TreeViewColumn Bool
treeViewColumnClickable = newAttr
  treeViewColumnGetClickable
  treeViewColumnSetClickable

-- | Widget to put in column header button instead of column title.
--
treeViewColumnWidget :: WidgetClass widget => ReadWriteAttr TreeViewColumn (Maybe Widget) (Maybe widget)
treeViewColumnWidget = newAttr
  treeViewColumnGetWidget
  treeViewColumnSetWidget

-- | X Alignment of the column header text or widget.
--
-- Allowed values: [0,1]
--
-- Default value: 0
--
treeViewColumnAlignment :: Attr TreeViewColumn Float
treeViewColumnAlignment = newAttr
  treeViewColumnGetAlignment
  treeViewColumnSetAlignment

-- | Whether the column can be reordered around the headers.
--
-- Default value: @False@
--
treeViewColumnReorderable :: Attr TreeViewColumn Bool
treeViewColumnReorderable = newAttr
  treeViewColumnGetReorderable
  treeViewColumnSetReorderable

-- | Whether to show a sort indicator.
--
-- Default value: @False@
--
treeViewColumnSortIndicator :: Attr TreeViewColumn Bool
treeViewColumnSortIndicator = newAttr
  treeViewColumnGetSortIndicator
  treeViewColumnSetSortIndicator

-- | Sort direction the sort indicator should indicate.
--
-- Default value: 'SortAscending'
--
treeViewColumnSortOrder :: Attr TreeViewColumn SortType
treeViewColumnSortOrder = newAttr
  treeViewColumnGetSortOrder
  treeViewColumnSetSortOrder

-- | \'sortColumnId\' property. See 'treeViewColumnGetSortColumnId' and
-- 'treeViewColumnSetSortColumnId'
--
treeViewColumnSortColumnId :: Attr TreeViewColumn SortColumnId
treeViewColumnSortColumnId = newAttr
  treeViewColumnGetSortColumnId
  treeViewColumnSetSortColumnId

--------------------
-- Signals

-- | Emitted when the header of this column has been clicked on.
--
onColClicked, afterColClicked :: TreeViewColumnClass self => self
 -> IO ()
 -> IO (ConnectId self)
onColClicked = connect_NONE__NONE "clicked" False
afterColClicked = connect_NONE__NONE "clicked" True

foreign import ccall safe "gtk_tree_view_column_new"
  gtk_tree_view_column_new :: (IO (Ptr TreeViewColumn))

foreign import ccall unsafe "gtk_tree_view_column_pack_start"
  gtk_tree_view_column_pack_start :: ((Ptr TreeViewColumn) -> ((Ptr CellRenderer) -> (CInt -> (IO ()))))

foreign import ccall unsafe "gtk_tree_view_column_pack_end"
  gtk_tree_view_column_pack_end :: ((Ptr TreeViewColumn) -> ((Ptr CellRenderer) -> (CInt -> (IO ()))))

foreign import ccall safe "gtk_tree_view_column_clear"
  gtk_tree_view_column_clear :: ((Ptr TreeViewColumn) -> (IO ()))

foreign import ccall unsafe "gtk_tree_view_column_get_cell_renderers"
  gtk_tree_view_column_get_cell_renderers :: ((Ptr TreeViewColumn) -> (IO (Ptr ())))

foreign import ccall safe "gtk_tree_view_column_set_spacing"
  gtk_tree_view_column_set_spacing :: ((Ptr TreeViewColumn) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_tree_view_column_get_spacing"
  gtk_tree_view_column_get_spacing :: ((Ptr TreeViewColumn) -> (IO CInt))

foreign import ccall safe "gtk_tree_view_column_set_visible"
  gtk_tree_view_column_set_visible :: ((Ptr TreeViewColumn) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_tree_view_column_get_visible"
  gtk_tree_view_column_get_visible :: ((Ptr TreeViewColumn) -> (IO CInt))

foreign import ccall safe "gtk_tree_view_column_set_resizable"
  gtk_tree_view_column_set_resizable :: ((Ptr TreeViewColumn) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_tree_view_column_get_resizable"
  gtk_tree_view_column_get_resizable :: ((Ptr TreeViewColumn) -> (IO CInt))

foreign import ccall safe "gtk_tree_view_column_set_sizing"
  gtk_tree_view_column_set_sizing :: ((Ptr TreeViewColumn) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_tree_view_column_get_sizing"
  gtk_tree_view_column_get_sizing :: ((Ptr TreeViewColumn) -> (IO CInt))

foreign import ccall unsafe "gtk_tree_view_column_get_width"
  gtk_tree_view_column_get_width :: ((Ptr TreeViewColumn) -> (IO CInt))

foreign import ccall safe "gtk_tree_view_column_set_fixed_width"
  gtk_tree_view_column_set_fixed_width :: ((Ptr TreeViewColumn) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_tree_view_column_get_fixed_width"
  gtk_tree_view_column_get_fixed_width :: ((Ptr TreeViewColumn) -> (IO CInt))

foreign import ccall safe "gtk_tree_view_column_set_min_width"
  gtk_tree_view_column_set_min_width :: ((Ptr TreeViewColumn) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_tree_view_column_get_min_width"
  gtk_tree_view_column_get_min_width :: ((Ptr TreeViewColumn) -> (IO CInt))

foreign import ccall safe "gtk_tree_view_column_set_max_width"
  gtk_tree_view_column_set_max_width :: ((Ptr TreeViewColumn) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_tree_view_column_get_max_width"
  gtk_tree_view_column_get_max_width :: ((Ptr TreeViewColumn) -> (IO CInt))

foreign import ccall safe "gtk_tree_view_column_clicked"
  gtk_tree_view_column_clicked :: ((Ptr TreeViewColumn) -> (IO ()))

foreign import ccall safe "gtk_tree_view_column_set_title"
  gtk_tree_view_column_set_title :: ((Ptr TreeViewColumn) -> ((Ptr CChar) -> (IO ())))

foreign import ccall unsafe "gtk_tree_view_column_get_title"
  gtk_tree_view_column_get_title :: ((Ptr TreeViewColumn) -> (IO (Ptr CChar)))

foreign import ccall safe "gtk_tree_view_column_set_clickable"
  gtk_tree_view_column_set_clickable :: ((Ptr TreeViewColumn) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_tree_view_column_get_clickable"
  gtk_tree_view_column_get_clickable :: ((Ptr TreeViewColumn) -> (IO CInt))

foreign import ccall safe "gtk_tree_view_column_set_widget"
  gtk_tree_view_column_set_widget :: ((Ptr TreeViewColumn) -> ((Ptr Widget) -> (IO ())))

foreign import ccall unsafe "gtk_tree_view_column_get_widget"
  gtk_tree_view_column_get_widget :: ((Ptr TreeViewColumn) -> (IO (Ptr Widget)))

foreign import ccall safe "gtk_tree_view_column_set_alignment"
  gtk_tree_view_column_set_alignment :: ((Ptr TreeViewColumn) -> (CFloat -> (IO ())))

foreign import ccall unsafe "gtk_tree_view_column_get_alignment"
  gtk_tree_view_column_get_alignment :: ((Ptr TreeViewColumn) -> (IO CFloat))

foreign import ccall safe "gtk_tree_view_column_set_reorderable"
  gtk_tree_view_column_set_reorderable :: ((Ptr TreeViewColumn) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_tree_view_column_get_reorderable"
  gtk_tree_view_column_get_reorderable :: ((Ptr TreeViewColumn) -> (IO CInt))

foreign import ccall safe "gtk_tree_view_column_set_sort_column_id"
  gtk_tree_view_column_set_sort_column_id :: ((Ptr TreeViewColumn) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_tree_view_column_get_sort_column_id"
  gtk_tree_view_column_get_sort_column_id :: ((Ptr TreeViewColumn) -> (IO CInt))

foreign import ccall safe "gtk_tree_view_column_set_sort_indicator"
  gtk_tree_view_column_set_sort_indicator :: ((Ptr TreeViewColumn) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_tree_view_column_get_sort_indicator"
  gtk_tree_view_column_get_sort_indicator :: ((Ptr TreeViewColumn) -> (IO CInt))

foreign import ccall safe "gtk_tree_view_column_set_sort_order"
  gtk_tree_view_column_set_sort_order :: ((Ptr TreeViewColumn) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_tree_view_column_get_sort_order"
  gtk_tree_view_column_get_sort_order :: ((Ptr TreeViewColumn) -> (IO CInt))

foreign import ccall safe "gtk_tree_view_column_set_expand"
  gtk_tree_view_column_set_expand :: ((Ptr TreeViewColumn) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_tree_view_column_get_expand"
  gtk_tree_view_column_get_expand :: ((Ptr TreeViewColumn) -> (IO CInt))

foreign import ccall safe "gtk_tree_view_column_cell_is_visible"
  gtk_tree_view_column_cell_is_visible :: ((Ptr TreeViewColumn) -> (IO CInt))

foreign import ccall safe "gtk_tree_view_column_focus_cell"
  gtk_tree_view_column_focus_cell :: ((Ptr TreeViewColumn) -> ((Ptr CellRenderer) -> (IO ())))

foreign import ccall safe "gtk_tree_view_column_queue_resize"
  gtk_tree_view_column_queue_resize :: ((Ptr TreeViewColumn) -> (IO ()))
