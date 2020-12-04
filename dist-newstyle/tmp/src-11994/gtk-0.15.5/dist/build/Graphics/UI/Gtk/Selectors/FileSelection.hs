
{-# LINE 2 "./Graphics/UI/Gtk/Selectors/FileSelection.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget FileSelection
--
-- Author : Manuel M T Chakravarty
--
-- Created: 20 January 1999
--
-- Copyright (C) 1999-2005 Manuel M T Chakravarty, Jens Petersen
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
-- Prompt the user for a file or directory name
--
-- * As of Gtk+ 2.4 this module has been deprecated in favour of 'FileChooser'
--
-- This module is empty in Gtk3.
module Graphics.UI.Gtk.Selectors.FileSelection (
-- * Detail
--
-- | 'FileSelection' should be used to retrieve file or directory names from
-- the user. It will create a new dialog window containing a directory list,
-- and a file list corresponding to the current working directory. The
-- filesystem can be navigated using the directory list or the drop-down
-- history menu. Alternatively, the TAB key can be used to navigate using
-- filename completion - common in text based editors such as emacs and jed.
--
-- File selection dialogs are created with a call to 'fileSelectionNew'.
--
-- The default filename can be set using 'fileSelectionSetFilename' and the
-- selected filename retrieved using 'fileSelectionGetFilename'.
--
-- Use 'fileSelectionComplete' to display files and directories that match a
-- given pattern. This can be used for example, to show only *.txt files, or
-- only files beginning with gtk*.
--
-- Simple file operations; create directory, delete file, and rename file,
-- are available from buttons at the top of the dialog. These can be hidden
-- using 'fileSelectionHideFileopButtons' and shown again using
-- 'fileSelectionShowFileopButtons'.
--

-- * Class Hierarchy
-- |
-- @
-- | 'GObject'
-- | +----'Object'
-- | +----'Widget'
-- | +----'Container'
-- | +----'Bin'
-- | +----'Window'
-- | +----'Dialog'
-- | +----FileSelection
-- @

-- * Types
  FileSelection,
  FileSelectionClass,
  castToFileSelection, gTypeFileSelection,
  toFileSelection,

-- * Constructors
  fileSelectionNew,

-- * Methods
  fileSelectionSetFilename,
  fileSelectionGetFilename,
  fileSelectionShowFileopButtons,
  fileSelectionHideFileopButtons,
  fileSelectionGetButtons,
  fileSelectionComplete,
  fileSelectionGetSelections,
  fileSelectionSetSelectMultiple,
  fileSelectionGetSelectMultiple,

-- * Attributes
  fileSelectionFilename,
  fileSelectionShowFileops,
  fileSelectionSelectMultiple,

  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Types
{-# LINE 103 "./Graphics/UI/Gtk/Selectors/FileSelection.chs" #-}
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.UI.Gtk.General.Structs (fileSelectionGetButtons)


{-# LINE 107 "./Graphics/UI/Gtk/Selectors/FileSelection.chs" #-}

--------------------
-- Constructors

-- | Creates a new file selection dialog box. By default it will contain a
-- 'TreeView' of the application's current working directory, and a file
-- listing. Operation buttons that allow the user to create a directory, delete
-- files and rename files, are also present.
--
fileSelectionNew :: GlibString string
 => string -- ^ @title@ - a message that will be placed in the file
                     -- requestor's titlebar.
 -> IO FileSelection
fileSelectionNew title =
  makeNewObject mkFileSelection $
  liftM (castPtr :: Ptr Widget -> Ptr FileSelection) $
  withUTFString title $ \titlePtr ->
  gtk_file_selection_new
{-# LINE 125 "./Graphics/UI/Gtk/Selectors/FileSelection.chs" #-}
    titlePtr

--------------------
-- Methods

-- | Sets a default path for the file requestor. If @filename@ includes a
-- directory path, then the requestor will open with that path as its current
-- working directory.
--
-- This has the consequence that in order to open the requestor with a
-- working directory and an empty filename, @filename@ must have a trailing
-- directory separator.
--
fileSelectionSetFilename :: (FileSelectionClass self, GlibString string) => self
 -> string -- ^ @filename@ - a string to set as the default file name.
 -> IO ()
fileSelectionSetFilename self filename =
  withUTFString filename $ \filenamePtr ->



  (\(FileSelection arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_file_selection_set_filename argPtr1 arg2)
{-# LINE 147 "./Graphics/UI/Gtk/Selectors/FileSelection.chs" #-}

    (toFileSelection self)
    filenamePtr

-- | This function returns the selected filename.
--
-- If no file is selected then the selected directory path is returned.
--
fileSelectionGetFilename :: (FileSelectionClass self, GlibString string) => self
 -> IO string -- ^ returns currently-selected filename
fileSelectionGetFilename self =



  (\(FileSelection arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_file_selection_get_filename argPtr1)
{-# LINE 162 "./Graphics/UI/Gtk/Selectors/FileSelection.chs" #-}

    (toFileSelection self)
  >>= peekUTFString

-- | Shows the file operation buttons, if they have previously been hidden.
-- The rest of the widgets in the dialog will be resized accordingly.
--
fileSelectionShowFileopButtons :: FileSelectionClass self => self -> IO ()
fileSelectionShowFileopButtons self =
  (\(FileSelection arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_file_selection_show_fileop_buttons argPtr1)
{-# LINE 172 "./Graphics/UI/Gtk/Selectors/FileSelection.chs" #-}
    (toFileSelection self)

-- | Hides the file operation buttons that normally appear at the top of the
-- dialog. Useful if you wish to create a custom file selector, based on
-- 'FileSelection'.
--
fileSelectionHideFileopButtons :: FileSelectionClass self => self -> IO ()
fileSelectionHideFileopButtons self =
  (\(FileSelection arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_file_selection_hide_fileop_buttons argPtr1)
{-# LINE 181 "./Graphics/UI/Gtk/Selectors/FileSelection.chs" #-}
    (toFileSelection self)

-- | Will attempt to match @pattern@ to a valid filenames or subdirectories in
-- the current directory. If a match can be made, the matched filename will
-- appear in the text entry field of the file selection dialog. If a partial
-- match can be made, the \"Files\" list will contain those file names which
-- have been partially matched, and the \"Folders\" list those directories
-- which have been partially matched.
--
fileSelectionComplete :: (FileSelectionClass self, GlibString string) => self
 -> string -- ^ @pattern@ - a string of characters which may or may not match
           -- any filenames in the current directory.
 -> IO ()
fileSelectionComplete self pattern =
  withUTFString pattern $ \patternPtr ->
  (\(FileSelection arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_file_selection_complete argPtr1 arg2)
{-# LINE 197 "./Graphics/UI/Gtk/Selectors/FileSelection.chs" #-}
    (toFileSelection self)
    patternPtr

-- | Retrieves the list of file selections the user has made in the dialog
-- box. This function is intended for use when the user can select multiple
-- files in the file list.
--
fileSelectionGetSelections :: (FileSelectionClass self, GlibString string) => self -> IO [string]
fileSelectionGetSelections self = do
  cStrArr <-



    (\(FileSelection arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_file_selection_get_selections argPtr1)
{-# LINE 211 "./Graphics/UI/Gtk/Selectors/FileSelection.chs" #-}

    (toFileSelection self)
  cStrs <- peekArray0 nullPtr cStrArr
  result <- mapM peekUTFString cStrs
  g_strfreev cStrArr
  return result

-- | Sets whether the user is allowed to select multiple files in the file
-- list. Use 'fileSelectionGetSelections' to get the list of selected files.
--
fileSelectionSetSelectMultiple :: FileSelectionClass self => self
 -> Bool -- ^ @selectMultiple@ - whether or not the user is allowed to select
          -- multiple files in the file list.
 -> IO ()
fileSelectionSetSelectMultiple self selectMultiple =
  (\(FileSelection arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_file_selection_set_select_multiple argPtr1 arg2)
{-# LINE 227 "./Graphics/UI/Gtk/Selectors/FileSelection.chs" #-}
    (toFileSelection self)
    (fromBool selectMultiple)

-- | Determines whether or not the user is allowed to select multiple files in
-- the file list. See 'fileSelectionSetSelectMultiple'.
--
fileSelectionGetSelectMultiple :: FileSelectionClass self => self
 -> IO Bool -- ^ returns @True@ if the user is allowed to select multiple
            -- files in the file list
fileSelectionGetSelectMultiple self =
  liftM toBool $
  (\(FileSelection arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_file_selection_get_select_multiple argPtr1)
{-# LINE 239 "./Graphics/UI/Gtk/Selectors/FileSelection.chs" #-}
    (toFileSelection self)

--------------------
-- Attributes

-- | The currently selected filename.
--
--
fileSelectionFilename :: (FileSelectionClass self, GlibString string) => Attr self string
fileSelectionFilename = newAttr
  fileSelectionGetFilename
  fileSelectionSetFilename

-- | Whether buttons for creating\/manipulating files should be displayed.
--
-- Default value: @False@
--
fileSelectionShowFileops :: FileSelectionClass self => Attr self Bool
fileSelectionShowFileops = newAttrFromBoolProperty "show-fileops"

-- | Whether to allow multiple files to be selected.
--
-- Default value: @False@
--
fileSelectionSelectMultiple :: FileSelectionClass self => Attr self Bool
fileSelectionSelectMultiple = newAttr
  fileSelectionGetSelectMultiple
  fileSelectionSetSelectMultiple

foreign import ccall unsafe "gtk_file_selection_new"
  gtk_file_selection_new :: ((Ptr CChar) -> (IO (Ptr Widget)))

foreign import ccall unsafe "gtk_file_selection_set_filename"
  gtk_file_selection_set_filename :: ((Ptr FileSelection) -> ((Ptr CChar) -> (IO ())))

foreign import ccall unsafe "gtk_file_selection_get_filename"
  gtk_file_selection_get_filename :: ((Ptr FileSelection) -> (IO (Ptr CChar)))

foreign import ccall safe "gtk_file_selection_show_fileop_buttons"
  gtk_file_selection_show_fileop_buttons :: ((Ptr FileSelection) -> (IO ()))

foreign import ccall safe "gtk_file_selection_hide_fileop_buttons"
  gtk_file_selection_hide_fileop_buttons :: ((Ptr FileSelection) -> (IO ()))

foreign import ccall safe "gtk_file_selection_complete"
  gtk_file_selection_complete :: ((Ptr FileSelection) -> ((Ptr CChar) -> (IO ())))

foreign import ccall safe "gtk_file_selection_get_selections"
  gtk_file_selection_get_selections :: ((Ptr FileSelection) -> (IO (Ptr (Ptr CChar))))

foreign import ccall unsafe "g_strfreev"
  g_strfreev :: ((Ptr (Ptr CChar)) -> (IO ()))

foreign import ccall safe "gtk_file_selection_set_select_multiple"
  gtk_file_selection_set_select_multiple :: ((Ptr FileSelection) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_file_selection_get_select_multiple"
  gtk_file_selection_get_select_multiple :: ((Ptr FileSelection) -> (IO CInt))