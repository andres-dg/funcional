
{-# LINE 2 "./Graphics/UI/Gtk/Recent/RecentFilter.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget RecentFilter
--
-- Author : Andy Stewart
--
-- Created: 27 Mar 2010
--
-- Copyright (C) 2010 Andy Stewart
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
-- A filter for selecting a subset of recently used files
--
-- * Module available since Gtk+ version 2.10
--
module Graphics.UI.Gtk.Recent.RecentFilter (

-- * Detail
--
-- | A 'RecentFilter' can be used to restrict the files being shown in a
-- 'RecentChooser'. Files can be filtered based on their name (with
-- 'recentFilterAddPattern'), on their mime type (with
-- 'fileFilterAddMimeType'), on the application that has registered them (with
-- 'recentFilterAddApplication'), or by a custom filter function (with
-- 'recentFilterAddCustom').
--
-- Filtering by mime type handles aliasing and subclassing of mime types;
-- e.g. a filter for text\/plain also matches a file with mime type
-- application\/rtf, since application\/rtf is a subclass of text\/plain. Note
-- that 'RecentFilter' allows wildcards for the subtype of a mime type, so you
-- can e.g. filter for image\/.
--
-- Normally, filters are used by adding them to a 'RecentChooser', see
-- 'recentChooserAddFilter', but it is also possible to manually use a filter
-- on a file with 'recentFilterFilter'.
--
-- Recently used files are supported since Gtk+ 2.10.

-- * Class Hierarchy
--
-- |
-- @
-- | 'GObject'
-- | +----'Object'
-- | +----RecentFilter
-- @


-- * Types
  RecentFilter,
  RecentFilterClass,
  castToRecentFilter,
  toRecentFilter,

-- * Enums
  RecentFilterFlags(..),

-- * Constructors
  recentFilterNew,

-- * Methods
  recentFilterGetName,
  recentFilterSetName,
  recentFilterAddMimeType,
  recentFilterAddPattern,
  recentFilterAddPixbufFormats,
  recentFilterAddApplication,
  recentFilterAddGroup,
  recentFilterAddAge,

  ) where



import System.Glib.FFI
import System.Glib.UTFString
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.UI.Gtk.Types
{-# LINE 93 "./Graphics/UI/Gtk/Recent/RecentFilter.chs" #-}


{-# LINE 95 "./Graphics/UI/Gtk/Recent/RecentFilter.chs" #-}

---------------------
-- Enums
-- | These flags indicate what parts of a 'RecentFilterInfo' struct are filled or need to be filled.
data RecentFilterFlags = RecentFilterUri
                       | RecentFilterDisplayName
                       | RecentFilterMimeType
                       | RecentFilterApplication
                       | RecentFilterGroup
                       | RecentFilterAge
                       deriving (Bounded,Eq,Show)
instance Enum RecentFilterFlags where
  fromEnum RecentFilterUri = 1
  fromEnum RecentFilterDisplayName = 2
  fromEnum RecentFilterMimeType = 4
  fromEnum RecentFilterApplication = 8
  fromEnum RecentFilterGroup = 16
  fromEnum RecentFilterAge = 32

  toEnum 1 = RecentFilterUri
  toEnum 2 = RecentFilterDisplayName
  toEnum 4 = RecentFilterMimeType
  toEnum 8 = RecentFilterApplication
  toEnum 16 = RecentFilterGroup
  toEnum 32 = RecentFilterAge
  toEnum unmatched = error ("RecentFilterFlags.toEnum: Cannot match " ++ show unmatched)

  succ RecentFilterUri = RecentFilterDisplayName
  succ RecentFilterDisplayName = RecentFilterMimeType
  succ RecentFilterMimeType = RecentFilterApplication
  succ RecentFilterApplication = RecentFilterGroup
  succ RecentFilterGroup = RecentFilterAge
  succ _ = undefined

  pred RecentFilterDisplayName = RecentFilterUri
  pred RecentFilterMimeType = RecentFilterDisplayName
  pred RecentFilterApplication = RecentFilterMimeType
  pred RecentFilterGroup = RecentFilterApplication
  pred RecentFilterAge = RecentFilterGroup
  pred _ = undefined

  enumFromTo x y | fromEnum x == fromEnum y = [ y ]
                 | otherwise = x : enumFromTo (succ x) y
  enumFrom x = enumFromTo x RecentFilterAge
  enumFromThen _ _ =     error "Enum RecentFilterFlags: enumFromThen not implemented"
  enumFromThenTo _ _ _ =     error "Enum RecentFilterFlags: enumFromThenTo not implemented"

{-# LINE 100 "./Graphics/UI/Gtk/Recent/RecentFilter.chs" #-}

--------------------
-- Constructors

-- | Creates a new 'RecentFilter' with no rules added to it. Such filter does
-- not accept any recently used resources, so is not particularly useful until
-- you add rules with 'recentFilterAddPattern', 'recentFilterAddMimeType',
-- 'recentFilterAddApplication', 'recentFilterAddAge'. To create a filter that
-- accepts any recently used resource, use:
--
-- > filter <- recentFilterNew
-- > recentFilterAddPattern filter "*"
--
-- * Available since Gtk+ version 2.10
--
recentFilterNew :: IO RecentFilter
recentFilterNew =
  makeNewObject mkRecentFilter $
  gtk_recent_filter_new
{-# LINE 119 "./Graphics/UI/Gtk/Recent/RecentFilter.chs" #-}

--------------------
-- Methods
-- | Gets the human-readable name for the filter. See 'recentFilterSetName'.
--
-- * Available since Gtk+ version 2.10
--
recentFilterGetName :: (RecentFilterClass self, GlibString string) => self
 -> IO string -- ^ returns the name of the filter
recentFilterGetName self =
  (\(RecentFilter arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_recent_filter_get_name argPtr1)
{-# LINE 130 "./Graphics/UI/Gtk/Recent/RecentFilter.chs" #-}
    (toRecentFilter self)
  >>= peekUTFString

-- | Sets the human-readable name of the filter; this is the string that will be displayed in the
-- recently used resources selector user interface if there is a selectable list of filters.
--
-- * Available since Gtk+ version 2.10
--
recentFilterSetName :: (RecentFilterClass self, GlibString string) => self
 -> string -- ^ @name@ - then human readable name of @filter@
 -> IO ()
recentFilterSetName self name =
  withUTFString name $ \namePtr ->
  (\(RecentFilter arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_recent_filter_set_name argPtr1 arg2)
{-# LINE 144 "./Graphics/UI/Gtk/Recent/RecentFilter.chs" #-}
    (toRecentFilter self)
    namePtr

-- | Adds a rule that allows resources based on their registered MIME type.
--
-- * Available since Gtk+ version 2.10
--
recentFilterAddMimeType :: (RecentFilterClass self, GlibString string) => self
 -> string -- ^ @mimeType@ - a MIME type
 -> IO ()
recentFilterAddMimeType self mimeType =
  withUTFString mimeType $ \mimeTypePtr ->
  (\(RecentFilter arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_recent_filter_add_mime_type argPtr1 arg2)
{-# LINE 157 "./Graphics/UI/Gtk/Recent/RecentFilter.chs" #-}
    (toRecentFilter self)
    mimeTypePtr

-- | Adds a rule that allows resources based on a pattern matching their
-- display name.
--
-- * Available since Gtk+ version 2.10
--
recentFilterAddPattern :: (RecentFilterClass self, GlibString string) => self
 -> string -- ^ @pattern@ - a file pattern
 -> IO ()
recentFilterAddPattern self pattern =
  withUTFString pattern $ \patternPtr ->
  (\(RecentFilter arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_recent_filter_add_pattern argPtr1 arg2)
{-# LINE 171 "./Graphics/UI/Gtk/Recent/RecentFilter.chs" #-}
    (toRecentFilter self)
    patternPtr

-- | Adds a rule allowing image files in the formats supported by 'Pixbuf'.
--
recentFilterAddPixbufFormats :: RecentFilterClass self => self -> IO ()
recentFilterAddPixbufFormats self =
  (\(RecentFilter arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_recent_filter_add_pixbuf_formats argPtr1)
{-# LINE 179 "./Graphics/UI/Gtk/Recent/RecentFilter.chs" #-}
    (toRecentFilter self)

-- | Adds a rule that allows resources based on the name of the application
-- that has registered them.
--
--
-- * Available since Gtk+ version 2.10
--
recentFilterAddApplication :: (RecentFilterClass self, GlibString string) => self
 -> string -- ^ @application@ - an application name
 -> IO ()
recentFilterAddApplication self application =
  withUTFString application $ \applicationPtr ->
  (\(RecentFilter arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_recent_filter_add_application argPtr1 arg2)
{-# LINE 193 "./Graphics/UI/Gtk/Recent/RecentFilter.chs" #-}
    (toRecentFilter self)
    applicationPtr

-- | Adds a rule that allows resources based on the name of the group to which
-- they belong
--
--
-- * Available since Gtk+ version 2.10
--
recentFilterAddGroup :: (RecentFilterClass self, GlibString string) => self
 -> string -- ^ @group@ - a group name
 -> IO ()
recentFilterAddGroup self group =
  withUTFString group $ \groupPtr ->
  (\(RecentFilter arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_recent_filter_add_group argPtr1 arg2)
{-# LINE 208 "./Graphics/UI/Gtk/Recent/RecentFilter.chs" #-}
    (toRecentFilter self)
    groupPtr

-- | Adds a rule that allows resources based on their age - that is, the
-- number of days elapsed since they were last modified.
--
-- * Available since Gtk+ version 2.10
--
recentFilterAddAge :: RecentFilterClass self => self
 -> Int -- ^ @days@ - number of days
 -> IO ()
recentFilterAddAge self days =
  (\(RecentFilter arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_recent_filter_add_age argPtr1 arg2)
{-# LINE 221 "./Graphics/UI/Gtk/Recent/RecentFilter.chs" #-}
    (toRecentFilter self)
    (fromIntegral days)

foreign import ccall safe "gtk_recent_filter_new"
  gtk_recent_filter_new :: (IO (Ptr RecentFilter))

foreign import ccall safe "gtk_recent_filter_get_name"
  gtk_recent_filter_get_name :: ((Ptr RecentFilter) -> (IO (Ptr CChar)))

foreign import ccall safe "gtk_recent_filter_set_name"
  gtk_recent_filter_set_name :: ((Ptr RecentFilter) -> ((Ptr CChar) -> (IO ())))

foreign import ccall safe "gtk_recent_filter_add_mime_type"
  gtk_recent_filter_add_mime_type :: ((Ptr RecentFilter) -> ((Ptr CChar) -> (IO ())))

foreign import ccall safe "gtk_recent_filter_add_pattern"
  gtk_recent_filter_add_pattern :: ((Ptr RecentFilter) -> ((Ptr CChar) -> (IO ())))

foreign import ccall safe "gtk_recent_filter_add_pixbuf_formats"
  gtk_recent_filter_add_pixbuf_formats :: ((Ptr RecentFilter) -> (IO ()))

foreign import ccall safe "gtk_recent_filter_add_application"
  gtk_recent_filter_add_application :: ((Ptr RecentFilter) -> ((Ptr CChar) -> (IO ())))

foreign import ccall safe "gtk_recent_filter_add_group"
  gtk_recent_filter_add_group :: ((Ptr RecentFilter) -> ((Ptr CChar) -> (IO ())))

foreign import ccall safe "gtk_recent_filter_add_age"
  gtk_recent_filter_add_age :: ((Ptr RecentFilter) -> (CInt -> (IO ())))
