
{-# LINE 2 "./Graphics/UI/Gtk/Selectors/FileFilter.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget FileFilter
--
-- Author : Duncan Coutts
--
-- Created: 26 February 2005
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
-- A filter for selecting a file subset
--
-- * Module available since Gtk+ version 2.4
--
module Graphics.UI.Gtk.Selectors.FileFilter (
-- * Detail
--
-- | A 'FileFilter' can be used to restrict the files being shown in a
-- 'FileChooser'. Files can be filtered based on their name (with
-- 'fileFilterAddPattern'), on their mime type (with 'fileFilterAddMimeType'),
-- or by a custom filter function (with 'fileFilterAddCustom').
--
-- Filtering by mime types handles aliasing and subclassing of mime types;
-- e.g. a filter for \"text\/plain\" also matches a file with mime type
-- \"application\/rtf\", since \"application\/rtf\" is a subclass of
-- \"text\/plain\". Note that 'FileFilter' allows wildcards for the subtype of
-- a mime type, so you can e.g. filter for \"image\/\*\".
--
-- Normally, filters are used by adding them to a 'FileChooser', see
-- 'Graphics.UI.Gtk.Selectors.FileChooser.fileChooserAddFilter'.

-- * Class Hierarchy
-- |
-- @
-- | 'GObject'
-- | +----'Object'
-- | +----FileFilter
-- @


-- * Types
  FileFilter,
  FileFilterClass,
  castToFileFilter, gTypeFileFilter,
  toFileFilter,
  FileFilterFlags(..),

-- * Constructors
  fileFilterNew,

-- * Methods
  fileFilterSetName,
  fileFilterGetName,
  fileFilterAddMimeType,
  fileFilterAddPattern,
  fileFilterAddCustom,

  fileFilterAddPixbufFormats,


-- * Attributes
  fileFilterName,

  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.Flags (Flags, fromFlags)
import System.Glib.UTFString
import System.Glib.Attributes
import Graphics.UI.Gtk.Types
{-# LINE 88 "./Graphics/UI/Gtk/Selectors/FileFilter.chs" #-}
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)


{-# LINE 91 "./Graphics/UI/Gtk/Selectors/FileFilter.chs" #-}



data FileFilterFlags = FileFilterFilename
                     | FileFilterUri
                     | FileFilterDisplayName
                     | FileFilterMimeType
                     deriving (Bounded,Show,Eq)
instance Enum FileFilterFlags where
  fromEnum FileFilterFilename = 1
  fromEnum FileFilterUri = 2
  fromEnum FileFilterDisplayName = 4
  fromEnum FileFilterMimeType = 8

  toEnum 1 = FileFilterFilename
  toEnum 2 = FileFilterUri
  toEnum 4 = FileFilterDisplayName
  toEnum 8 = FileFilterMimeType
  toEnum unmatched = error ("FileFilterFlags.toEnum: Cannot match " ++ show unmatched)

  succ FileFilterFilename = FileFilterUri
  succ FileFilterUri = FileFilterDisplayName
  succ FileFilterDisplayName = FileFilterMimeType
  succ _ = undefined

  pred FileFilterUri = FileFilterFilename
  pred FileFilterDisplayName = FileFilterUri
  pred FileFilterMimeType = FileFilterDisplayName
  pred _ = undefined

  enumFromTo x y | fromEnum x == fromEnum y = [ y ]
                 | otherwise = x : enumFromTo (succ x) y
  enumFrom x = enumFromTo x FileFilterMimeType
  enumFromThen _ _ =     error "Enum FileFilterFlags: enumFromThen not implemented"
  enumFromThenTo _ _ _ =     error "Enum FileFilterFlags: enumFromThenTo not implemented"

{-# LINE 95 "./Graphics/UI/Gtk/Selectors/FileFilter.chs" #-}
instance Flags FileFilterFlags

--------------------
-- Constructors

-- | Creates a new 'FileFilter' with no rules added to it. Such a filter
-- doesn't accept any files, so is not particularly useful until you add rules
-- with 'fileFilterAddMimeType', 'fileFilterAddPattern', or
-- 'fileFilterAddCustom'.
--
fileFilterNew :: IO FileFilter
fileFilterNew =
  makeNewObject mkFileFilter $
  gtk_file_filter_new
{-# LINE 109 "./Graphics/UI/Gtk/Selectors/FileFilter.chs" #-}

--------------------
-- Methods

-- | Sets the human-readable name of the filter; this is the string that will
-- be displayed in the file selector user interface if there is a selectable
-- list of filters.
--
fileFilterSetName :: GlibString string
 => FileFilter
 -> string -- ^ @name@ - the human-readable-name for the filter
 -> IO ()
fileFilterSetName self name =
  withUTFString name $ \namePtr ->
  (\(FileFilter arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_file_filter_set_name argPtr1 arg2)
{-# LINE 124 "./Graphics/UI/Gtk/Selectors/FileFilter.chs" #-}
    self
    namePtr

-- | Gets the human-readable name for the filter. See 'fileFilterSetName'.
--
fileFilterGetName :: GlibString string
 => FileFilter
 -> IO string -- ^ returns The human-readable name of the filter
fileFilterGetName self =
  (\(FileFilter arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_file_filter_get_name argPtr1)
{-# LINE 134 "./Graphics/UI/Gtk/Selectors/FileFilter.chs" #-}
    self
  >>= peekUTFString

-- | Adds a rule allowing a given mime type to @filter@.
--
fileFilterAddMimeType :: GlibString string
 => FileFilter
 -> string -- ^ @mimeType@ - name of a MIME type
 -> IO ()
fileFilterAddMimeType self mimeType =
  withUTFString mimeType $ \mimeTypePtr ->
  (\(FileFilter arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_file_filter_add_mime_type argPtr1 arg2)
{-# LINE 146 "./Graphics/UI/Gtk/Selectors/FileFilter.chs" #-}
    self
    mimeTypePtr

-- | Adds a rule allowing a shell style glob to a filter.
--
fileFilterAddPattern :: GlibString string
 => FileFilter
 -> string -- ^ @pattern@ - a shell style glob
 -> IO ()
fileFilterAddPattern self pattern =
  withUTFString pattern $ \patternPtr ->
  (\(FileFilter arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_file_filter_add_pattern argPtr1 arg2)
{-# LINE 158 "./Graphics/UI/Gtk/Selectors/FileFilter.chs" #-}
    self
    patternPtr

-- | Adds rule to a filter that allows files based on a custom callback
-- function. The list of flags @needed@ which is passed in provides information
-- about what sorts of information that the filter function needs; this allows
-- Gtk+ to avoid retrieving expensive information when it isn't needed by the
-- filter.
--
fileFilterAddCustom :: GlibString string => FileFilter
 -> [FileFilterFlags] -- ^ @needed@ - list of flags indicating the
                          -- information that the custom filter function needs.
 -> ( Maybe string -- filename
     -> Maybe string -- uri
     -> Maybe string -- display name
     -> Maybe string -- mime type
     -> IO Bool) -- ^ @(\filename uri displayName mimeType -> ...)@ -
                          -- filter function; if the function
                          -- returns @True@, then the file will be displayed.
 -> IO ()
fileFilterAddCustom self needed func = do
  hPtr <- mkHandler_GtkFileFilterFunc
    (\filterInfoPtr _ -> do
      filenamePtr <- (\ptr -> do {peekByteOff ptr 8 ::IO (Ptr CChar)}) filterInfoPtr
      uriPtr <- (\ptr -> do {peekByteOff ptr 16 ::IO (Ptr CChar)}) filterInfoPtr
      displayNamePtr <- (\ptr -> do {peekByteOff ptr 24 ::IO (Ptr CChar)}) filterInfoPtr
      mimeTypePtr <- (\ptr -> do {peekByteOff ptr 32 ::IO (Ptr CChar)}) filterInfoPtr
      filename <- maybePeek peekUTFString filenamePtr
      uri <- maybePeek peekUTFString uriPtr
      displayName <- maybePeek peekUTFString displayNamePtr
      mimeType <- maybePeek peekUTFString mimeTypePtr
      liftM fromBool $ func filename uri displayName mimeType)
  (\(FileFilter arg1) arg2 arg3 arg4 arg5 -> withForeignPtr arg1 $ \argPtr1 ->gtk_file_filter_add_custom argPtr1 arg2 arg3 arg4 arg5)
{-# LINE 191 "./Graphics/UI/Gtk/Selectors/FileFilter.chs" #-}
    self
    ((fromIntegral . fromFlags) needed)
    hPtr
    (castFunPtrToPtr hPtr)
    destroyFunPtr

type GtkFileFilterInfoPtr = Ptr (())
{-# LINE 198 "./Graphics/UI/Gtk/Selectors/FileFilter.chs" #-}

type GtkFileFilterFunc =
  GtkFileFilterInfoPtr -> --GtkFileFilterInfo *filter_info
  Ptr () -> --gpointer user_data
  IO CInt

foreign import ccall "wrapper" mkHandler_GtkFileFilterFunc ::
  GtkFileFilterFunc ->
  IO (FunPtr GtkFileFilterFunc)


-- | Adds a rule allowing image files in the formats supported by 'Pixbuf'.
--
-- * Available since Gtk+ version 2.6
--
fileFilterAddPixbufFormats :: FileFilter -> IO ()
fileFilterAddPixbufFormats self =
  (\(FileFilter arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_file_filter_add_pixbuf_formats argPtr1)
{-# LINE 216 "./Graphics/UI/Gtk/Selectors/FileFilter.chs" #-}
    self


--------------------
-- Attributes

-- | \'name\' property. See 'fileFilterGetName' and 'fileFilterSetName'
--
fileFilterName :: GlibString string => Attr FileFilter string
fileFilterName = newAttr
  fileFilterGetName
  fileFilterSetName

foreign import ccall safe "gtk_file_filter_new"
  gtk_file_filter_new :: (IO (Ptr FileFilter))

foreign import ccall safe "gtk_file_filter_set_name"
  gtk_file_filter_set_name :: ((Ptr FileFilter) -> ((Ptr CChar) -> (IO ())))

foreign import ccall safe "gtk_file_filter_get_name"
  gtk_file_filter_get_name :: ((Ptr FileFilter) -> (IO (Ptr CChar)))

foreign import ccall safe "gtk_file_filter_add_mime_type"
  gtk_file_filter_add_mime_type :: ((Ptr FileFilter) -> ((Ptr CChar) -> (IO ())))

foreign import ccall safe "gtk_file_filter_add_pattern"
  gtk_file_filter_add_pattern :: ((Ptr FileFilter) -> ((Ptr CChar) -> (IO ())))

foreign import ccall safe "gtk_file_filter_add_custom"
  gtk_file_filter_add_custom :: ((Ptr FileFilter) -> (CInt -> ((FunPtr ((Ptr ()) -> ((Ptr ()) -> (IO CInt)))) -> ((Ptr ()) -> ((FunPtr ((Ptr ()) -> (IO ()))) -> (IO ()))))))

foreign import ccall safe "gtk_file_filter_add_pixbuf_formats"
  gtk_file_filter_add_pixbuf_formats :: ((Ptr FileFilter) -> (IO ()))
