
{-# LINE 2 "./Graphics/UI/Gtk/Layout/Table.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget Table
--
-- Author : Axel Simon
--
-- Created: 15 May 2001
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
-- The table widget is a container in which widgets can be aligned in cells.
--
module Graphics.UI.Gtk.Layout.Table (
-- * Detail
--
-- | The 'Table' functions allow the programmer to arrange widgets in rows and
-- columns, making it easy to align many widgets next to each other,
-- horizontally and vertically.
--
-- Tables are created with a call to 'tableNew', the size of which can later
-- be changed with 'tableResize'.
--
-- Widgets can be added to a table using 'tableAttach' or the more
-- convenient (but slightly less flexible) 'tableAttachDefaults'.
--
-- To alter the space next to a specific row, use 'tableSetRowSpacing', and
-- for a column, 'tableSetColSpacing'.
--
-- The gaps between /all/ rows or columns can be changed by calling
-- 'tableSetRowSpacings' or 'tableSetColSpacings' respectively.
--
-- 'tableSetHomogeneous', can be used to set whether all cells in the table
-- will resize themselves to the size of the largest widget in the table.

-- * Class Hierarchy
-- |
-- @
-- | 'GObject'
-- | +----'Object'
-- | +----'Widget'
-- | +----'Container'
-- | +----Table
-- @

-- * Types
  Table,
  TableClass,
  castToTable, gTypeTable,
  toTable,

-- * Constructors
  tableNew,

-- * Methods
  tableResize,
  AttachOptions(..),
  tableAttach,
  tableAttachDefaults,
  tableSetRowSpacing,
  tableGetRowSpacing,
  tableSetColSpacing,
  tableGetColSpacing,
  tableSetRowSpacings,
  tableGetDefaultRowSpacing,
  tableSetColSpacings,
  tableGetDefaultColSpacing,
  tableSetHomogeneous,
  tableGetHomogeneous,

  tableGetSize,


-- * Attributes
  tableNRows,
  tableNColumns,
  tableRowSpacing,
  tableColumnSpacing,
  tableHomogeneous,

-- * Child Attributes
  tableChildLeftAttach,
  tableChildRightAttach,
  tableChildTopAttach,
  tableChildBottomAttach,
  tableChildXOptions,
  tableChildYOptions,
  tableChildXPadding,
  tableChildYPadding,
  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.Flags (fromFlags)
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.UI.Gtk.Types
{-# LINE 114 "./Graphics/UI/Gtk/Layout/Table.chs" #-}
import Graphics.UI.Gtk.General.Enums (AttachOptions(..))
import Graphics.UI.Gtk.Abstract.ContainerChildProperties


{-# LINE 118 "./Graphics/UI/Gtk/Layout/Table.chs" #-}

--------------------
-- Constructors

-- | Used to create a new table widget. An initial size must be given by
-- specifying how many rows and columns the table should have, although this
-- can be changed later with 'tableResize'. @rows@ and @columns@ must both be
-- in the range 0 .. 65535.
--
tableNew ::
    Int -- ^ @rows@ - The number of rows the new table should have.
 -> Int -- ^ @columns@ - The number of columns the new table should have.
 -> Bool -- ^ @homogeneous@ - If set to @True@, all table cells are
             -- resized to the size of the cell containing the largest widget.
 -> IO Table
tableNew rows columns homogeneous =
  makeNewObject mkTable $
  liftM (castPtr :: Ptr Widget -> Ptr Table) $
  gtk_table_new
{-# LINE 137 "./Graphics/UI/Gtk/Layout/Table.chs" #-}
    (fromIntegral rows)
    (fromIntegral columns)
    (fromBool homogeneous)

--------------------
-- Methods

-- | Change the dimensions of an already existing table.
--
tableResize :: TableClass self => self
 -> Int -- ^ @rows@ - The new number of rows.
 -> Int -- ^ @columns@ - The new number of columns.
 -> IO ()
tableResize self rows columns =
  (\(Table arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_table_resize argPtr1 arg2 arg3)
{-# LINE 152 "./Graphics/UI/Gtk/Layout/Table.chs" #-}
    (toTable self)
    (fromIntegral rows)
    (fromIntegral columns)

-- | Adds a widget to a table. The number of \'cells\' that a widget will
-- occupy is specified by @leftAttach@, @rightAttach@, @topAttach@ and
-- @bottomAttach@. These each represent the leftmost, rightmost, uppermost and
-- lowest column and row numbers of the table. (Columns and rows are indexed
-- from zero).
--
tableAttach :: (TableClass self, WidgetClass child) => self
 -> child -- ^ @child@ - The widget to add.
 -> Int -- ^ @leftAttach@ - the column number to attach the left
                    -- side of a child widget to.
 -> Int -- ^ @rightAttach@ - the column number to attach the right
                    -- side of a child widget to.
 -> Int -- ^ @topAttach@ - the row number to attach the top of a
                    -- child widget to.
 -> Int -- ^ @bottomAttach@ - the row number to attach the bottom
                    -- of a child widget to.
 -> [AttachOptions] -- ^ @xoptions@ - Used to specify the properties of the
                    -- child widget when the table is resized.
 -> [AttachOptions] -- ^ @yoptions@ - The same as xoptions, except this field
                    -- determines behaviour of vertical resizing.
 -> Int -- ^ @xpadding@ - An integer value specifying the padding
                    -- on the left and right of the widget being added to the
                    -- table.
 -> Int -- ^ @ypadding@ - The amount of padding above and below
                    -- the child widget.
 -> IO ()
tableAttach self child leftAttach rightAttach topAttach bottomAttach xoptions
            yoptions xpadding ypadding =
  (\(Table arg1) (Widget arg2) arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_table_attach argPtr1 argPtr2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10)
{-# LINE 185 "./Graphics/UI/Gtk/Layout/Table.chs" #-}
    (toTable self)
    (toWidget child)
    (fromIntegral leftAttach)
    (fromIntegral rightAttach)
    (fromIntegral topAttach)
    (fromIntegral bottomAttach)
    ((fromIntegral . fromFlags) xoptions)
    ((fromIntegral . fromFlags) yoptions)
    (fromIntegral xpadding)
    (fromIntegral ypadding)

-- | As there are many options associated with 'tableAttach', this convenience
-- function provides the programmer with a means to add children to a table
-- with identical padding and expansion options. The values used for the
-- 'AttachOptions' are @['Expand', 'Fill']@, and the padding is set to 0.
--
tableAttachDefaults :: (TableClass self, WidgetClass widget) => self
 -> widget -- ^ @widget@ - The child widget to add.
 -> Int -- ^ @leftAttach@ - The column number to attach the left side of
           -- the child widget to.
 -> Int -- ^ @rightAttach@ - The column number to attach the right side of
           -- the child widget to.
 -> Int -- ^ @topAttach@ - The row number to attach the top of the child
           -- widget to.
 -> Int -- ^ @bottomAttach@ - The row number to attach the bottom of the
           -- child widget to.
 -> IO ()
tableAttachDefaults self widget leftAttach rightAttach topAttach bottomAttach =
  (\(Table arg1) (Widget arg2) arg3 arg4 arg5 arg6 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_table_attach_defaults argPtr1 argPtr2 arg3 arg4 arg5 arg6)
{-# LINE 214 "./Graphics/UI/Gtk/Layout/Table.chs" #-}
    (toTable self)
    (toWidget widget)
    (fromIntegral leftAttach)
    (fromIntegral rightAttach)
    (fromIntegral topAttach)
    (fromIntegral bottomAttach)

-- | Changes the space between a given table row and its surrounding rows.
--
tableSetRowSpacing :: TableClass self => self
 -> Int -- ^ @row@ - row number whose spacing will be changed.
 -> Int -- ^ @spacing@ - number of pixels that the spacing should take up.
 -> IO ()
tableSetRowSpacing self row spacing =
  (\(Table arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_table_set_row_spacing argPtr1 arg2 arg3)
{-# LINE 229 "./Graphics/UI/Gtk/Layout/Table.chs" #-}
    (toTable self)
    (fromIntegral row)
    (fromIntegral spacing)

-- | Gets the amount of space between row @row@, and row @row@ + 1. See
-- 'tableSetRowSpacing'.
--
tableGetRowSpacing :: TableClass self => self
 -> Int -- ^ @row@ - a row in the table, 0 indicates the first row
 -> IO Int -- ^ returns the row spacing
tableGetRowSpacing self row =
  liftM fromIntegral $
  (\(Table arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_table_get_row_spacing argPtr1 arg2)
{-# LINE 242 "./Graphics/UI/Gtk/Layout/Table.chs" #-}
    (toTable self)
    (fromIntegral row)

-- | Alters the amount of space between a given table column and the adjacent
-- columns.
--
tableSetColSpacing :: TableClass self => self
 -> Int -- ^ @column@ - the column whose spacing should be changed.
 -> Int -- ^ @spacing@ - number of pixels that the spacing should take up.
 -> IO ()
tableSetColSpacing self column spacing =
  (\(Table arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_table_set_col_spacing argPtr1 arg2 arg3)
{-# LINE 254 "./Graphics/UI/Gtk/Layout/Table.chs" #-}
    (toTable self)
    (fromIntegral column)
    (fromIntegral spacing)

-- | Gets the amount of space between column @col@, and column @col@ + 1. See
-- 'tableSetColSpacing'.
--
tableGetColSpacing :: TableClass self => self
 -> Int -- ^ @column@ - a column in the table, 0 indicates the first column
 -> IO Int -- ^ returns the column spacing
tableGetColSpacing self column =
  liftM fromIntegral $
  (\(Table arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_table_get_col_spacing argPtr1 arg2)
{-# LINE 267 "./Graphics/UI/Gtk/Layout/Table.chs" #-}
    (toTable self)
    (fromIntegral column)

-- | Sets the space between every row in @table@ equal to @spacing@.
--
tableSetRowSpacings :: TableClass self => self
 -> Int -- ^ @spacing@ - the number of pixels of space to place between
          -- every row in the table.
 -> IO ()
tableSetRowSpacings self spacing =
  (\(Table arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_table_set_row_spacings argPtr1 arg2)
{-# LINE 278 "./Graphics/UI/Gtk/Layout/Table.chs" #-}
    (toTable self)
    (fromIntegral spacing)

-- | Gets the default row spacing for the table. This is the spacing that will
-- be used for newly added rows. (See 'tableSetRowSpacings')
--
tableGetDefaultRowSpacing :: TableClass self => self
 -> IO Int -- ^ returns the default row spacing
tableGetDefaultRowSpacing self =
  liftM fromIntegral $
  (\(Table arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_table_get_default_row_spacing argPtr1)
{-# LINE 289 "./Graphics/UI/Gtk/Layout/Table.chs" #-}
    (toTable self)

-- | Sets the space between every column in @table@ equal to @spacing@.
--
tableSetColSpacings :: TableClass self => self
 -> Int -- ^ @spacing@ - the number of pixels of space to place between
          -- every column in the table.
 -> IO ()
tableSetColSpacings self spacing =
  (\(Table arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_table_set_col_spacings argPtr1 arg2)
{-# LINE 299 "./Graphics/UI/Gtk/Layout/Table.chs" #-}
    (toTable self)
    (fromIntegral spacing)

-- | Gets the default column spacing for the table. This is the spacing that
-- will be used for newly added columns. (See 'tableSetColSpacings')
--
tableGetDefaultColSpacing :: TableClass self => self
 -> IO Int -- ^ returns the default column spacing
tableGetDefaultColSpacing self =
  liftM fromIntegral $
  (\(Table arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_table_get_default_col_spacing argPtr1)
{-# LINE 310 "./Graphics/UI/Gtk/Layout/Table.chs" #-}
    (toTable self)

-- | Changes the homogenous property of table cells, ie. whether all cells are
-- an equal size or not.
--
tableSetHomogeneous :: TableClass self => self
 -> Bool -- ^ @homogeneous@ - Set to @True@ to ensure all table cells are the
          -- same size. Set to @False@ if this is not your desired behaviour.
 -> IO ()
tableSetHomogeneous self homogeneous =
  (\(Table arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_table_set_homogeneous argPtr1 arg2)
{-# LINE 321 "./Graphics/UI/Gtk/Layout/Table.chs" #-}
    (toTable self)
    (fromBool homogeneous)

-- | Returns whether the table cells are all constrained to the same width and
-- height. (See 'tableSetHomogeneous')
--
tableGetHomogeneous :: TableClass self => self
 -> IO Bool -- ^ returns @True@ if the cells are all constrained to the same
            -- size
tableGetHomogeneous self =
  liftM toBool $
  (\(Table arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_table_get_homogeneous argPtr1)
{-# LINE 333 "./Graphics/UI/Gtk/Layout/Table.chs" #-}
    (toTable self)


-- | Returns the size of 'Table'.
--
-- * Available since Gtk+ version 2.22
--
tableGetSize :: TableClass self => self
             -> IO (Int, Int) -- ^ returns (rows, columns) of table
tableGetSize self =
  alloca $ \ rowsPtr ->
  alloca $ \ columnsPtr -> do
  (\(Table arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_table_get_size argPtr1 arg2 arg3)
{-# LINE 346 "./Graphics/UI/Gtk/Layout/Table.chs" #-}
    (toTable self)
    rowsPtr
    columnsPtr
  rows <- peek rowsPtr
  columns <- peek columnsPtr
  return (fromIntegral rows, fromIntegral columns)



--------------------
-- Attributes

-- | The number of rows in the table.
--
-- Default value: 0
--
tableNRows :: TableClass self => Attr self Int
tableNRows = newAttrFromUIntProperty "n-rows"

-- | The number of columns in the table.
--
-- Default value: 0
--
tableNColumns :: TableClass self => Attr self Int
tableNColumns = newAttrFromUIntProperty "n-columns"

-- | The amount of space between two consecutive rows.
--
-- Default value: 0
--
tableRowSpacing :: TableClass self => Attr self Int
tableRowSpacing = newAttrFromUIntProperty "row-spacing"

-- | The amount of space between two consecutive columns.
--
-- Default value: 0
--
tableColumnSpacing :: TableClass self => Attr self Int
tableColumnSpacing = newAttrFromUIntProperty "column-spacing"

-- | If @True@ this means the table cells are all the same width\/height.
--
-- Default value: @False@
--
tableHomogeneous :: TableClass self => Attr self Bool
tableHomogeneous = newAttr
  tableGetHomogeneous
  tableSetHomogeneous

--------------------
-- Child Attributes

-- | The column number to attach the left side of the child to.
--
-- Allowed values: \<= 65535
--
-- Default value: 0
--
tableChildLeftAttach :: (TableClass self, WidgetClass child) => child -> Attr self Int
tableChildLeftAttach = newAttrFromContainerChildUIntProperty "left-attach"

-- | The column number to attach the right side of a child widget to.
--
-- Allowed values: [1,65535]
--
-- Default value: 1
--
tableChildRightAttach :: (TableClass self, WidgetClass child) => child -> Attr self Int
tableChildRightAttach = newAttrFromContainerChildUIntProperty "right-attach"

-- | The row number to attach the top of a child widget to.
--
-- Allowed values: \<= 65535
--
-- Default value: 0
--
tableChildTopAttach :: (TableClass self, WidgetClass child) => child -> Attr self Int
tableChildTopAttach = newAttrFromContainerChildUIntProperty "top-attach"

-- | The row number to attach the bottom of the child to.
--
-- Allowed values: [1,65535]
--
-- Default value: 1
--
tableChildBottomAttach :: (TableClass self, WidgetClass child) => child -> Attr self Int
tableChildBottomAttach = newAttrFromContainerChildUIntProperty "bottom-attach"

-- | Options specifying the horizontal behaviour of the child.
--
-- Default value: @['Expand', 'Fill']@
--
tableChildXOptions :: (TableClass self, WidgetClass child) => child -> Attr self [AttachOptions]
tableChildXOptions = newAttrFromContainerChildFlagsProperty "x-options"
                       gtk_attach_options_get_type
{-# LINE 441 "./Graphics/UI/Gtk/Layout/Table.chs" #-}

-- | Options specifying the vertical behaviour of the child.
--
-- Default value: @['Expand', 'Fill']@
--
tableChildYOptions :: (TableClass self, WidgetClass child) => child -> Attr self [AttachOptions]
tableChildYOptions = newAttrFromContainerChildFlagsProperty "y-options"
                       gtk_attach_options_get_type
{-# LINE 449 "./Graphics/UI/Gtk/Layout/Table.chs" #-}

-- | Extra space to put between the child and its left and right neighbors, in
-- pixels.
--
-- Allowed values: \<= 65535
--
-- Default value: 0
--
tableChildXPadding :: (TableClass self, WidgetClass child) => child -> Attr self Int
tableChildXPadding = newAttrFromContainerChildUIntProperty "x-padding"

-- | Extra space to put between the child and its upper and lower neighbors,
-- in pixels.
--
-- Allowed values: \<= 65535
--
-- Default value: 0
--
tableChildYPadding :: (TableClass self, WidgetClass child) => child -> Attr self Int
tableChildYPadding = newAttrFromContainerChildUIntProperty "y-padding"

foreign import ccall unsafe "gtk_table_new"
  gtk_table_new :: (CUInt -> (CUInt -> (CInt -> (IO (Ptr Widget)))))

foreign import ccall safe "gtk_table_resize"
  gtk_table_resize :: ((Ptr Table) -> (CUInt -> (CUInt -> (IO ()))))

foreign import ccall safe "gtk_table_attach"
  gtk_table_attach :: ((Ptr Table) -> ((Ptr Widget) -> (CUInt -> (CUInt -> (CUInt -> (CUInt -> (CInt -> (CInt -> (CUInt -> (CUInt -> (IO ())))))))))))

foreign import ccall safe "gtk_table_attach_defaults"
  gtk_table_attach_defaults :: ((Ptr Table) -> ((Ptr Widget) -> (CUInt -> (CUInt -> (CUInt -> (CUInt -> (IO ())))))))

foreign import ccall safe "gtk_table_set_row_spacing"
  gtk_table_set_row_spacing :: ((Ptr Table) -> (CUInt -> (CUInt -> (IO ()))))

foreign import ccall unsafe "gtk_table_get_row_spacing"
  gtk_table_get_row_spacing :: ((Ptr Table) -> (CUInt -> (IO CUInt)))

foreign import ccall safe "gtk_table_set_col_spacing"
  gtk_table_set_col_spacing :: ((Ptr Table) -> (CUInt -> (CUInt -> (IO ()))))

foreign import ccall unsafe "gtk_table_get_col_spacing"
  gtk_table_get_col_spacing :: ((Ptr Table) -> (CUInt -> (IO CUInt)))

foreign import ccall safe "gtk_table_set_row_spacings"
  gtk_table_set_row_spacings :: ((Ptr Table) -> (CUInt -> (IO ())))

foreign import ccall unsafe "gtk_table_get_default_row_spacing"
  gtk_table_get_default_row_spacing :: ((Ptr Table) -> (IO CUInt))

foreign import ccall safe "gtk_table_set_col_spacings"
  gtk_table_set_col_spacings :: ((Ptr Table) -> (CUInt -> (IO ())))

foreign import ccall unsafe "gtk_table_get_default_col_spacing"
  gtk_table_get_default_col_spacing :: ((Ptr Table) -> (IO CUInt))

foreign import ccall safe "gtk_table_set_homogeneous"
  gtk_table_set_homogeneous :: ((Ptr Table) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_table_get_homogeneous"
  gtk_table_get_homogeneous :: ((Ptr Table) -> (IO CInt))

foreign import ccall unsafe "gtk_table_get_size"
  gtk_table_get_size :: ((Ptr Table) -> ((Ptr CUInt) -> ((Ptr CUInt) -> (IO ()))))

foreign import ccall unsafe "gtk_attach_options_get_type"
  gtk_attach_options_get_type :: CULong
