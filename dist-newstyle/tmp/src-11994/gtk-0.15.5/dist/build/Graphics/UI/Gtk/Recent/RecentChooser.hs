
{-# LINE 2 "./Graphics/UI/Gtk/Recent/RecentChooser.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Interface RecentChooser
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
-- Interface implemented by widgets displaying recently used files
--
-- * Module available since Gtk+ version 2.10
--
module Graphics.UI.Gtk.Recent.RecentChooser (

-- * Detail
--
-- | 'RecentChooser' is an interface that can be implemented by widgets
-- displaying the list of recently used files. In Gtk+, the main objects that
-- implement this interface are 'RecentChooserWidget', 'RecentChooserDialog'
-- and 'RecentChooserMenu'.
--
-- Recently used files are supported since Gtk+ 2.10.

-- * Class Hierarchy
--
-- |
-- @
-- | 'GInterface'
-- | +----RecentChooser
-- @


-- * Types
  RecentChooser,
  RecentChooserClass,
  castToRecentChooser,
  toRecentChooser,

-- * Enums
  RecentChooserError(..),
  RecentSortType(..),

-- * Methods
  recentChooserSetSortFunc,
  recentChooserSetCurrentURI,
  recentChooserGetCurrentURI,
  recentChooserGetCurrentItem,
  recentChooserSelectURI,
  recentChooserUnselectURI,
  recentChooserSelectAll,
  recentChooserUnselectAll,
  recentChooserGetItems,
  recentChooserGetURIs,
  recentChooserAddFilter,
  recentChooserRemoveFilter,
  recentChooserListFilters,

-- * Attributes
  recentChooserShowPrivate,
  recentChooserShowTips,
  recentChooserShowIcons,
  recentChooserShowNotFound,
  recentChooserSelectMultiple,
  recentChooserLocalOnly,
  recentChooserLimit,
  recentChooserSortType,
  recentChooserFilter,

-- * Signals
  recentChooserSelectionChanged,
  recentChooserItemActivated,

  ) where



import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import System.Glib.Properties
import System.Glib.GList
import System.Glib.GError (checkGError)
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.UI.Gtk.Recent.RecentInfo (RecentInfo, mkRecentInfo)
import Graphics.UI.Gtk.Types
{-# LINE 105 "./Graphics/UI/Gtk/Recent/RecentChooser.chs" #-}
import Graphics.UI.Gtk.Signals
{-# LINE 106 "./Graphics/UI/Gtk/Recent/RecentChooser.chs" #-}


{-# LINE 108 "./Graphics/UI/Gtk/Recent/RecentChooser.chs" #-}

--------------------
-- Enums
-- | These identify the various errors that can occur while calling 'RecentChooser' functions.
data RecentChooserError = RecentChooserErrorNotFound
                        | RecentChooserErrorInvalidUri
                        deriving (Enum,Bounded,Eq,Show)

{-# LINE 113 "./Graphics/UI/Gtk/Recent/RecentChooser.chs" #-}

-- | Used to specify the sorting method to be applyed to the recently used resource list.
data RecentSortType = RecentSortNone
                    | RecentSortMru
                    | RecentSortLru
                    | RecentSortCustom
                    deriving (Bounded,Eq,Show)
instance Enum RecentSortType where
  fromEnum RecentSortNone = 0
  fromEnum RecentSortMru = 1
  fromEnum RecentSortLru = 2
  fromEnum RecentSortCustom = 3

  toEnum 0 = RecentSortNone
  toEnum 1 = RecentSortMru
  toEnum 2 = RecentSortLru
  toEnum 3 = RecentSortCustom
  toEnum unmatched = error ("RecentSortType.toEnum: Cannot match " ++ show unmatched)

  succ RecentSortNone = RecentSortMru
  succ RecentSortMru = RecentSortLru
  succ RecentSortLru = RecentSortCustom
  succ _ = undefined

  pred RecentSortMru = RecentSortNone
  pred RecentSortLru = RecentSortMru
  pred RecentSortCustom = RecentSortLru
  pred _ = undefined

  enumFromTo x y | fromEnum x == fromEnum y = [ y ]
                 | otherwise = x : enumFromTo (succ x) y
  enumFrom x = enumFromTo x RecentSortCustom
  enumFromThen _ _ =     error "Enum RecentSortType: enumFromThen not implemented"
  enumFromThenTo _ _ _ =     error "Enum RecentSortType: enumFromThenTo not implemented"

{-# LINE 116 "./Graphics/UI/Gtk/Recent/RecentChooser.chs" #-}

--------------------
-- Methods

-- | Sets the comparison function used when sorting to be @sortFunc@. If the
-- @chooser@ has the sort type set to 'RecentSortCustom' then the chooser will
-- sort using this function.
--
-- To the comparison function will be passed two 'RecentInfo' structs and @sortData@; @sortFunc@ should return a positive
-- integer if the first item comes before the second, zero if the two items are
-- equal and a negative integer if the first item comes after the second.
--
--
-- * Available since Gtk+ version 2.10
--
recentChooserSetSortFunc :: RecentChooserClass self => self
 -> (Maybe (RecentInfo -> IO Int))
 -> IO ()
recentChooserSetSortFunc self Nothing =
  (\(RecentChooser arg1) arg2 arg3 arg4 -> withForeignPtr arg1 $ \argPtr1 ->gtk_recent_chooser_set_sort_func argPtr1 arg2 arg3 arg4)
{-# LINE 136 "./Graphics/UI/Gtk/Recent/RecentChooser.chs" #-}
    (toRecentChooser self) nullFunPtr nullPtr nullFunPtr
recentChooserSetSortFunc self (Just func) = do
  fPtr <- mkRecentSortFunc $ \_ infoPtr _ -> do
           info <- mkRecentInfo infoPtr
           liftM fromIntegral (func info)
  (\(RecentChooser arg1) arg2 arg3 arg4 -> withForeignPtr arg1 $ \argPtr1 ->gtk_recent_chooser_set_sort_func argPtr1 arg2 arg3 arg4)
{-# LINE 142 "./Graphics/UI/Gtk/Recent/RecentChooser.chs" #-}
    (toRecentChooser self)
    fPtr
    (castFunPtrToPtr fPtr)
    destroyFunPtr

type RecentSortFunc = FunPtr (((Ptr RecentInfo) -> ((Ptr RecentInfo) -> ((Ptr ()) -> (IO CInt)))))
{-# LINE 148 "./Graphics/UI/Gtk/Recent/RecentChooser.chs" #-}

foreign import ccall "wrapper" mkRecentSortFunc ::
  (Ptr RecentInfo -> Ptr RecentInfo -> Ptr () -> IO (CInt))
  -> IO RecentSortFunc

-- | Sets @uri@ as the current URI for @chooser@.
--
--
-- * Available since Gtk+ version 2.10
--
recentChooserSetCurrentURI :: (RecentChooserClass self, GlibString string) => self
 -> string -- ^ @uri@ - a URI
 -> IO Bool -- ^ returns @True@ if the URI was found.
recentChooserSetCurrentURI self uri =
  checkGError ( \errorPtr ->
                liftM toBool $
                withUTFString uri $ \uriPtr ->
                (\(RecentChooser arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_recent_chooser_set_current_uri argPtr1 arg2 arg3)
{-# LINE 166 "./Graphics/UI/Gtk/Recent/RecentChooser.chs" #-}
                    (toRecentChooser self)
                    uriPtr
                    errorPtr)
              (\_ -> return False)

-- | Gets the URI currently selected by @chooser@.
--
--
-- * Available since Gtk+ version 2.10
--
recentChooserGetCurrentURI :: (RecentChooserClass self, GlibString string) => self
 -> IO string -- ^ returns a newly string holding a URI.
recentChooserGetCurrentURI self =
  (\(RecentChooser arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_recent_chooser_get_current_uri argPtr1)
{-# LINE 180 "./Graphics/UI/Gtk/Recent/RecentChooser.chs" #-}
    (toRecentChooser self)
  >>= readUTFString

-- | Gets the 'RecentInfo' currently selected by
-- @chooser@.
--
--
-- * Available since Gtk+ version 2.10
--
recentChooserGetCurrentItem :: RecentChooserClass self => self
 -> IO RecentInfo -- ^ returns a 'RecentInfo'.
                          -- Use 'recentInfoUnref' when when you have finished
                          -- using it.
recentChooserGetCurrentItem self = do
  info <- (\(RecentChooser arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_recent_chooser_get_current_item argPtr1) (toRecentChooser self)
  mkRecentInfo info

-- | Selects @uri@ inside @chooser@.
--
--
-- * Available since Gtk+ version 2.10
--
recentChooserSelectURI :: (RecentChooserClass self, GlibString string) => self
 -> string -- ^ @uri@ - a URI
 -> IO Bool -- ^ returns @True@ if @uri@ was found.
recentChooserSelectURI self uri =
  checkGError ( \errorPtr ->
                liftM toBool $
                withUTFString uri $ \uriPtr ->
                (\(RecentChooser arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_recent_chooser_select_uri argPtr1 arg2 arg3)
{-# LINE 210 "./Graphics/UI/Gtk/Recent/RecentChooser.chs" #-}
                    (toRecentChooser self)
                    uriPtr
                    errorPtr)
              (\_ -> return False)

-- | Unselects @uri@ inside @chooser@.
--
--
-- * Available since Gtk+ version 2.10
--
recentChooserUnselectURI :: (RecentChooserClass self, GlibString string) => self
 -> string -- ^ @uri@ - a URI
 -> IO ()
recentChooserUnselectURI self uri =
  withUTFString uri $ \uriPtr ->
  (\(RecentChooser arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_recent_chooser_unselect_uri argPtr1 arg2)
{-# LINE 226 "./Graphics/UI/Gtk/Recent/RecentChooser.chs" #-}
    (toRecentChooser self)
    uriPtr

-- | Selects all the items inside @chooser@, if the @chooser@ supports
-- multiple selection.
--
--
-- * Available since Gtk+ version 2.10
--
recentChooserSelectAll :: RecentChooserClass self => self -> IO ()
recentChooserSelectAll self =
  (\(RecentChooser arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_recent_chooser_select_all argPtr1)
{-# LINE 238 "./Graphics/UI/Gtk/Recent/RecentChooser.chs" #-}
    (toRecentChooser self)

-- | Unselects all the items inside @chooser@.
--
--
-- * Available since Gtk+ version 2.10
--
recentChooserUnselectAll :: RecentChooserClass self => self -> IO ()
recentChooserUnselectAll self =
  (\(RecentChooser arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_recent_chooser_unselect_all argPtr1)
{-# LINE 248 "./Graphics/UI/Gtk/Recent/RecentChooser.chs" #-}
    (toRecentChooser self)

-- | Gets the list of recently used resources in form of 'RecentInfo'
--
-- The return value of this function is affected by the \"sort-type\" and
-- \"limit\" properties of @chooser@.
--
recentChooserGetItems :: RecentChooserClass self => self
 -> IO [RecentInfo] -- ^ returns A list of 'RecentInfo' objects.
recentChooserGetItems self = do
  glist <- (\(RecentChooser arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_recent_chooser_get_items argPtr1) (toRecentChooser self)
  list <- fromGList glist
  mapM mkRecentInfo list

-- | Gets the URI of the recently used resources.
--
-- The return value of this function is affected by the \"sort-type\" and
-- \"limit\" properties of @chooser@.
--
--
-- * Available since Gtk+ version 2.10
--
recentChooserGetURIs :: (RecentChooserClass self, GlibString string) => self
 -> IO [string]
recentChooserGetURIs self =
  alloca $ \lengthPtr -> do
  str <- (\(RecentChooser arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_recent_chooser_get_uris argPtr1 arg2)
{-# LINE 275 "./Graphics/UI/Gtk/Recent/RecentChooser.chs" #-}
          (toRecentChooser self)
          lengthPtr
  length <- peek lengthPtr
  mapM peekUTFString =<< peekArray (fromIntegral length) str

-- | Adds @filter@ to the list of 'RecentFilter' objects held by @chooser@.
--
-- If no previous filter objects were defined, this function will call
-- 'recentChooserSetFilter'.
--
--
-- * Available since Gtk+ version 2.10
--
recentChooserAddFilter :: (RecentChooserClass self, RecentFilterClass filter) => self
 -> filter -- ^ @filter@ - a 'RecentFilter'
 -> IO ()
recentChooserAddFilter self filter =
  (\(RecentChooser arg1) (RecentFilter arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_recent_chooser_add_filter argPtr1 argPtr2)
{-# LINE 293 "./Graphics/UI/Gtk/Recent/RecentChooser.chs" #-}
    (toRecentChooser self)
    (toRecentFilter filter)

-- | Removes @filter@ from the list of 'RecentFilter' objects held by
-- @chooser@.
--
recentChooserRemoveFilter :: (RecentChooserClass self, RecentFilterClass filter) => self
 -> filter -- ^ @filter@ - a 'RecentFilter'
 -> IO ()
recentChooserRemoveFilter self filter =
  (\(RecentChooser arg1) (RecentFilter arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_recent_chooser_remove_filter argPtr1 argPtr2)
{-# LINE 304 "./Graphics/UI/Gtk/Recent/RecentChooser.chs" #-}
    (toRecentChooser self)
    (toRecentFilter filter)

-- | Gets the 'RecentFilter' objects held by @chooser@.
--
--
-- * Available since Gtk+ version 2.10
--
recentChooserListFilters :: RecentChooserClass self => self
 -> IO [RecentFilter] -- ^ returns A singly linked list of 'RecentFilter'.
recentChooserListFilters self = do
  glist <- (\(RecentChooser arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_recent_chooser_list_filters argPtr1)
{-# LINE 316 "./Graphics/UI/Gtk/Recent/RecentChooser.chs" #-}
          (toRecentChooser self)
  list <- fromGList glist
  mapM (\x -> makeNewObject mkRecentFilter (return (castPtr x))) list

--------------------
-- Attributes

-- | Whether the private items should be displayed.
--
-- Default value: 'False'
--
-- * Available since Gtk+ version 2.10
--
recentChooserShowPrivate :: RecentChooserClass self => Attr self Bool
recentChooserShowPrivate = newAttrFromBoolProperty "show-private"

-- | Whether this 'RecentChooser' should display a tooltip containing the full path of the recently used
-- resources.
--
-- Default value: 'False'
--
--
-- * Available since Gtk+ version 2.10
--
recentChooserShowTips :: RecentChooserClass self => Attr self Bool
recentChooserShowTips = newAttrFromBoolProperty "show-tips"

-- | Whether this 'RecentChooser' should display an icon near the item.
--
-- Default value: 'True'
--
--
-- * Available since Gtk+ version 2.10
--
recentChooserShowIcons :: RecentChooserClass self => Attr self Bool
recentChooserShowIcons = newAttrFromBoolProperty "show-icons"

-- | Whether this 'RecentChooser' should display the recently used resources even if not present
-- anymore. Setting this to 'False' will perform a potentially expensive check on every local resource
-- (every remote resource will always be displayed).
--
-- Default value: 'True'
--
--
-- * Available since Gtk+ version 2.10
--
recentChooserShowNotFound :: RecentChooserClass self => Attr self Bool
recentChooserShowNotFound = newAttrFromBoolProperty "show-not-found"

-- | Allow the user to select multiple resources.
--
-- Default value: 'False'
--
--
-- * Available since Gtk+ version 2.10
--
recentChooserSelectMultiple :: RecentChooserClass self => Attr self Bool
recentChooserSelectMultiple = newAttrFromBoolProperty "select-multiple"

-- | Whether this 'RecentChooser' should display only local (file:) resources.
--
-- Default value: 'True'
--
--
-- * Available since Gtk+ version 2.10
--
recentChooserLocalOnly :: RecentChooserClass self => Attr self Bool
recentChooserLocalOnly = newAttrFromBoolProperty "local-only"

-- | The maximum number of recently used resources to be displayed, or -1 to display all items. By
-- default, the 'Setting':gtk-recent-files-limit setting is respected: you can override that limit on
-- a particular instance of 'RecentChooser' by setting this property.
--
-- Allowed values: >= 'GMaxulong'
--
-- Default value: -1
--
--
-- * Available since Gtk+ version 2.10
--
recentChooserLimit :: RecentChooserClass self => Attr self Int
recentChooserLimit = newAttrFromIntProperty "limit"

-- | Sorting order to be used when displaying the recently used resources.
--
-- Default value: ''RecentSortNone''
--
--
-- * Available since Gtk+ version 2.10
--
recentChooserSortType :: RecentChooserClass self => Attr self RecentSortType
recentChooserSortType = newAttrFromEnumProperty "sort-type"
                          gtk_recent_sort_type_get_type
{-# LINE 409 "./Graphics/UI/Gtk/Recent/RecentChooser.chs" #-}

-- | The 'RecentFilter' object to be used when displaying the recently used resources.
--
--
-- * Available since Gtk+ version 2.10
--
recentChooserFilter :: (RecentChooserClass self, RecentFilterClass recentFilter) => ReadWriteAttr self RecentFilter recentFilter
recentChooserFilter = newAttrFromObjectProperty "filter"
                        gtk_recent_filter_get_type
{-# LINE 418 "./Graphics/UI/Gtk/Recent/RecentChooser.chs" #-}

--------------------
-- Signals

-- | This signal is emitted when there is a change in the set of selected
-- recently used resources. This can happen when a user modifies the selection
-- with the mouse or the keyboard, or when explicitely calling functions to
-- change the selection.
--
--
-- * Available since Gtk+ version 2.10
--
recentChooserSelectionChanged :: RecentChooserClass self => Signal self (IO ())
recentChooserSelectionChanged = Signal (connect_NONE__NONE "selection_changed")

-- | This signal is emitted when the user \"activates\" a recent item in the
-- recent chooser. This can happen by double-clicking on an item in the
-- recently used resources list, or by pressing Enter.
--
--
-- * Available since Gtk+ version 2.10
--
recentChooserItemActivated :: RecentChooserClass self => Signal self (IO ())
recentChooserItemActivated = Signal (connect_NONE__NONE "item_activated")

foreign import ccall safe "gtk_recent_chooser_set_sort_func"
  gtk_recent_chooser_set_sort_func :: ((Ptr RecentChooser) -> ((FunPtr ((Ptr RecentInfo) -> ((Ptr RecentInfo) -> ((Ptr ()) -> (IO CInt))))) -> ((Ptr ()) -> ((FunPtr ((Ptr ()) -> (IO ()))) -> (IO ())))))

foreign import ccall safe "gtk_recent_chooser_set_current_uri"
  gtk_recent_chooser_set_current_uri :: ((Ptr RecentChooser) -> ((Ptr CChar) -> ((Ptr (Ptr ())) -> (IO CInt))))

foreign import ccall safe "gtk_recent_chooser_get_current_uri"
  gtk_recent_chooser_get_current_uri :: ((Ptr RecentChooser) -> (IO (Ptr CChar)))

foreign import ccall safe "gtk_recent_chooser_get_current_item"
  gtk_recent_chooser_get_current_item :: ((Ptr RecentChooser) -> (IO (Ptr RecentInfo)))

foreign import ccall safe "gtk_recent_chooser_select_uri"
  gtk_recent_chooser_select_uri :: ((Ptr RecentChooser) -> ((Ptr CChar) -> ((Ptr (Ptr ())) -> (IO CInt))))

foreign import ccall safe "gtk_recent_chooser_unselect_uri"
  gtk_recent_chooser_unselect_uri :: ((Ptr RecentChooser) -> ((Ptr CChar) -> (IO ())))

foreign import ccall safe "gtk_recent_chooser_select_all"
  gtk_recent_chooser_select_all :: ((Ptr RecentChooser) -> (IO ()))

foreign import ccall safe "gtk_recent_chooser_unselect_all"
  gtk_recent_chooser_unselect_all :: ((Ptr RecentChooser) -> (IO ()))

foreign import ccall safe "gtk_recent_chooser_get_items"
  gtk_recent_chooser_get_items :: ((Ptr RecentChooser) -> (IO (Ptr ())))

foreign import ccall safe "gtk_recent_chooser_get_uris"
  gtk_recent_chooser_get_uris :: ((Ptr RecentChooser) -> ((Ptr CULong) -> (IO (Ptr (Ptr CChar)))))

foreign import ccall safe "gtk_recent_chooser_add_filter"
  gtk_recent_chooser_add_filter :: ((Ptr RecentChooser) -> ((Ptr RecentFilter) -> (IO ())))

foreign import ccall safe "gtk_recent_chooser_remove_filter"
  gtk_recent_chooser_remove_filter :: ((Ptr RecentChooser) -> ((Ptr RecentFilter) -> (IO ())))

foreign import ccall safe "gtk_recent_chooser_list_filters"
  gtk_recent_chooser_list_filters :: ((Ptr RecentChooser) -> (IO (Ptr ())))

foreign import ccall unsafe "gtk_recent_sort_type_get_type"
  gtk_recent_sort_type_get_type :: CULong

foreign import ccall unsafe "gtk_recent_filter_get_type"
  gtk_recent_filter_get_type :: CULong
