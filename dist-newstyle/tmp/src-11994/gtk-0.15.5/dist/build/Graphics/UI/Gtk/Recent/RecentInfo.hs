
{-# LINE 2 "./Graphics/UI/Gtk/Recent/RecentInfo.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget RecentInfo
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
--
module Graphics.UI.Gtk.Recent.RecentInfo (


-- * Types
  RecentInfo,
  mkRecentInfo,

-- * Methods
  recentInfoExists,
  recentInfoGetAdded,
  recentInfoGetAge,
  recentInfoGetApplicationInfo,
  recentInfoGetApplications,
  recentInfoGetDescription,
  recentInfoGetDisplayName,
  recentInfoGetGroups,
  recentInfoGetIcon,
  recentInfoGetMimeType,
  recentInfoGetModified,
  recentInfoGetPrivateHint,
  recentInfoGetShortName,
  recentInfoGetURI,
  recentInfoGetURIDisplay,
  recentInfoGetVisited,
  recentInfoHasApplication,
  recentInfoHasGroup,
  recentInfoIsLocal,
  recentInfoLastApplication,
  recentInfoMatch,

  ) where



import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import Graphics.UI.Gtk.Types
{-# LINE 66 "./Graphics/UI/Gtk/Recent/RecentInfo.chs" #-}


{-# LINE 68 "./Graphics/UI/Gtk/Recent/RecentInfo.chs" #-}

--------------------
-- Types
newtype RecentInfo = RecentInfo (ForeignPtr (RecentInfo))
{-# LINE 72 "./Graphics/UI/Gtk/Recent/RecentInfo.chs" #-}

--------------------
-- Methods

-- | Helper function for build 'RecentInfo'
mkRecentInfo :: Ptr RecentInfo -> IO RecentInfo
mkRecentInfo rPtr = do
  info <- newForeignPtr rPtr gtk_recent_info_unref
  return (RecentInfo info)

foreign import ccall unsafe "&gtk_recent_info_unref"
  gtk_recent_info_unref :: FinalizerPtr RecentInfo

-- | Checks whether the resource pointed by info still exists. At the moment this check is done only on
-- resources pointing to local files.
--
-- * Available since Gtk+ version 2.10
--
recentInfoExists :: RecentInfo
                 -> IO Bool -- ^ returns 'True' if the resource exists
recentInfoExists self =
  liftM toBool $
  (\(RecentInfo arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_recent_info_exists argPtr1)
{-# LINE 95 "./Graphics/UI/Gtk/Recent/RecentInfo.chs" #-}
    self

-- | Gets the timestamp (seconds from system's Epoch) when the resource was added to the recently used
-- resources list.
--
-- * Available since Gtk+ version 2.10
--
recentInfoGetAdded :: RecentInfo
                   -> IO Int -- ^ returns the number of seconds elapsed from system's Epoch when the resource was added to the list, or -1 on failure.
recentInfoGetAdded self =
  liftM fromIntegral $
  (\(RecentInfo arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_recent_info_get_added argPtr1)
{-# LINE 107 "./Graphics/UI/Gtk/Recent/RecentInfo.chs" #-}
    self

-- | Gets the number of days elapsed since the last update of the resource pointed by info.
--
-- * Available since Gtk+ version 2.10
--
recentInfoGetAge :: RecentInfo
                 -> IO Int -- ^ returns a positive integer containing the number of days elapsed since the time this resource was last modified.
recentInfoGetAge self =
  liftM fromIntegral $
  (\(RecentInfo arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_recent_info_get_age argPtr1)
{-# LINE 118 "./Graphics/UI/Gtk/Recent/RecentInfo.chs" #-}
    self

-- | Gets the data regarding the application that has registered the resource pointed by info.
--
-- If the command line contains any escape characters defined inside the storage specification, they
-- will be expanded.
--
-- * Available since Gtk+ version 2.10
--
recentInfoGetApplicationInfo :: GlibString string => RecentInfo
                             -> string -- ^ @appName@ the name of the application that has registered this item
                             -> IO (Maybe ([string], Int, Int))
                              -- ^ @appExec@ return location for the string containing the command line. transfer none.
                              -- ^ @count@ return location for the number of times this item was registered. out.
                              -- ^ @time@ out. out.
recentInfoGetApplicationInfo self appName =
  alloca $ \countPtr ->
  alloca $ \timePtr ->
  allocaArray 0 $ \execPtr ->
  withUTFString appName $ \appNamePtr -> do
    success <- liftM toBool $
              (\(RecentInfo arg1) arg2 arg3 arg4 arg5 -> withForeignPtr arg1 $ \argPtr1 ->gtk_recent_info_get_application_info argPtr1 arg2 arg3 arg4 arg5)
{-# LINE 140 "./Graphics/UI/Gtk/Recent/RecentInfo.chs" #-}
                self
                appNamePtr
                execPtr
                countPtr
                timePtr
    if success
       then do
         exec <- mapM peekUTFString =<< peekArray 0 execPtr
         count <- peek countPtr
         time <- peek timePtr
         return (Just (exec, fromIntegral count, fromIntegral time))
       else return Nothing

-- | Retrieves the list of applications that have registered this resource.
--
-- * Available since Gtk+ version 2.10
--
recentInfoGetApplications :: GlibString string => RecentInfo -> IO [string]
recentInfoGetApplications self =
  alloca $ \lengthPtr -> do
    str <- (\(RecentInfo arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_recent_info_get_applications argPtr1 arg2) self lengthPtr
    length <- peek lengthPtr
    mapM peekUTFString =<< peekArray (fromIntegral length) str

-- | Gets the (short) description of the resource.
--
-- * Available since Gtk+ version 2.10
--
recentInfoGetDescription :: GlibString string => RecentInfo
                         -> IO string -- ^ returns the description of the resource.
recentInfoGetDescription self =
  (\(RecentInfo arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_recent_info_get_description argPtr1)
{-# LINE 172 "./Graphics/UI/Gtk/Recent/RecentInfo.chs" #-}
    self
  >>= peekUTFString

-- | Gets the name of the resource. If none has been defined, the basename of the resource is obtained.
--
-- * Available since Gtk+ version 2.10
--
recentInfoGetDisplayName :: GlibString string => RecentInfo
                         -> IO string -- ^ returns the display name of the resource.
recentInfoGetDisplayName self =
  (\(RecentInfo arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_recent_info_get_display_name argPtr1)
{-# LINE 183 "./Graphics/UI/Gtk/Recent/RecentInfo.chs" #-}
    self
  >>= peekUTFString

-- | Returns all groups registered for the recently used item info.
--
-- * Available since Gtk+ version 2.10
--
recentInfoGetGroups :: GlibString string => RecentInfo -> IO [string]
recentInfoGetGroups self =
  alloca $ \lengthPtr -> do
    str <- (\(RecentInfo arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_recent_info_get_groups argPtr1 arg2) self lengthPtr
    length <- peek lengthPtr
    mapM peekUTFString =<< peekArray (fromIntegral length) str

-- | Retrieves the icon of size size associated to the resource MIME type.
--
-- * Available since Gtk+ version 2.10
--
recentInfoGetIcon :: RecentInfo
                  -> Int -- ^ @size@ the size of the icon in pixels
                  -> IO (Maybe Pixbuf) -- ^ returns a 'Pixbuf' containing the icon, or 'Nothing'
recentInfoGetIcon self size =
  maybeNull (makeNewGObject mkPixbuf) $
  (\(RecentInfo arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_recent_info_get_icon argPtr1 arg2)
{-# LINE 207 "./Graphics/UI/Gtk/Recent/RecentInfo.chs" #-}
    self
    (fromIntegral size)

-- | Gets the MIME type of the resource.
--
-- * Available since Gtk+ version 2.10
--
recentInfoGetMimeType :: GlibString string => RecentInfo
                      -> IO string -- ^ returns the MIME type of the resource.
recentInfoGetMimeType self =
  (\(RecentInfo arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_recent_info_get_mime_type argPtr1)
{-# LINE 218 "./Graphics/UI/Gtk/Recent/RecentInfo.chs" #-}
    self
  >>= peekUTFString

-- | Gets the timestamp (seconds from system's Epoch) when the resource was last modified.
--
-- * Available since Gtk+ version 2.10
--
recentInfoGetModified :: RecentInfo
                      -> IO Int -- ^ returns the number of seconds elapsed from system's Epoch when the resource was last modified, or -1 on failure.
recentInfoGetModified self =
  liftM fromIntegral $
  (\(RecentInfo arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_recent_info_get_modified argPtr1)
{-# LINE 230 "./Graphics/UI/Gtk/Recent/RecentInfo.chs" #-}
    self

-- | Gets the value of the "private" flag. Resources in the recently used list that have this flag set to
-- 'True' should only be displayed by the applications that have registered them.
--
-- * Available since Gtk+ version 2.10
--
recentInfoGetPrivateHint :: RecentInfo
                         -> IO Bool -- ^ returns 'True' if the private flag was found, 'False' otherwise.
recentInfoGetPrivateHint self =
  liftM toBool $
  (\(RecentInfo arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_recent_info_get_private_hint argPtr1)
{-# LINE 242 "./Graphics/UI/Gtk/Recent/RecentInfo.chs" #-}
    self

-- | Computes a valid UTF-8 string that can be used as the name of the item in a menu or list. For
-- example, calling this function on an item that refers to \"file:///foo/bar.txt\" will yield \"bar.txt\".
--
-- * Available since Gtk+ version 2.10
--
recentInfoGetShortName :: GlibString string => RecentInfo
                       -> IO string
recentInfoGetShortName self =
  (\(RecentInfo arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_recent_info_get_short_name argPtr1)
{-# LINE 253 "./Graphics/UI/Gtk/Recent/RecentInfo.chs" #-}
    self
  >>= readUTFString

-- | Gets the URI of the resource.
--
-- * Available since Gtk+ version 2.10
--
recentInfoGetURI :: GlibString string => RecentInfo
                 -> IO string -- ^ returns the URI of the resource.
recentInfoGetURI self =
  (\(RecentInfo arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_recent_info_get_uri argPtr1)
{-# LINE 264 "./Graphics/UI/Gtk/Recent/RecentInfo.chs" #-}
    self
  >>= peekUTFString

-- | Gets a displayable version of the resource's URI. If the resource is local, it returns a local path;
-- if the resource is not local, it returns the UTF-8 encoded content of 'recentInfoGetUri'.
--
-- * Available since Gtk+ version 2.10
--
recentInfoGetURIDisplay :: GlibString string => RecentInfo -> IO string
recentInfoGetURIDisplay self =
  (\(RecentInfo arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_recent_info_get_uri_display argPtr1)
{-# LINE 275 "./Graphics/UI/Gtk/Recent/RecentInfo.chs" #-}
    self
  >>= readUTFString

-- | Gets the timestamp (seconds from system's Epoch) when the resource was last visited.
--
-- * Available since Gtk+ version 2.10
--
recentInfoGetVisited :: RecentInfo
                     -> IO Int -- ^ returns the number of seconds elapsed from system's Epoch when the resource was last visited, or -1 on failure.
recentInfoGetVisited self =
  liftM fromIntegral $
  (\(RecentInfo arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_recent_info_get_visited argPtr1)
{-# LINE 287 "./Graphics/UI/Gtk/Recent/RecentInfo.chs" #-}
    self

-- | Checks whether an application registered this resource using @appName@.
--
-- * Available since Gtk+ version 2.10
--
recentInfoHasApplication :: GlibString string => RecentInfo
                         -> string -- ^ @appName@ a string containing an application name
                         -> IO Bool -- ^ returns 'True' if an application with name @appName@ was found, 'False' otherwise.
recentInfoHasApplication self appName =
  liftM toBool $
  withUTFString appName $ \appNamePtr ->
  (\(RecentInfo arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_recent_info_has_application argPtr1 arg2)
{-# LINE 300 "./Graphics/UI/Gtk/Recent/RecentInfo.chs" #-}
    self
    appNamePtr

-- | Checks whether @groupName@ appears inside the groups registered for the recently used item info.
--
-- * Available since Gtk+ version 2.10
--
recentInfoHasGroup :: GlibString string => RecentInfo
                   -> string -- ^ @groupName@ name of a group
                   -> IO Bool -- ^ returns 'True' if the group was found.
recentInfoHasGroup self groupName =
  liftM toBool $
  withUTFString groupName $ \groupNamePtr ->
  (\(RecentInfo arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_recent_info_has_group argPtr1 arg2)
{-# LINE 314 "./Graphics/UI/Gtk/Recent/RecentInfo.chs" #-}
    self
    groupNamePtr

-- | Checks whether the resource is local or not by looking at the scheme of its URI.
--
-- * Available since Gtk+ version 2.10
--
recentInfoIsLocal :: RecentInfo
                  -> IO Bool -- ^ returns 'True' if the resource is local.
recentInfoIsLocal self =
  liftM toBool $
  (\(RecentInfo arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_recent_info_is_local argPtr1)
{-# LINE 326 "./Graphics/UI/Gtk/Recent/RecentInfo.chs" #-}
    self

-- | Gets the name of the last application that have registered the recently used resource represented by
-- info.
--
-- * Available since Gtk+ version 2.10
--
recentInfoLastApplication :: GlibString string => RecentInfo
                          -> IO string -- ^ returns an application name.
recentInfoLastApplication self =
  (\(RecentInfo arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_recent_info_last_application argPtr1)
{-# LINE 337 "./Graphics/UI/Gtk/Recent/RecentInfo.chs" #-}
    self
  >>= readUTFString

-- | Checks whether two 'RecentInfo' structures point to the same resource.
--
-- * Available since Gtk+ version 2.10
--
recentInfoMatch :: RecentInfo -> RecentInfo
                -> IO Bool -- ^ returns 'True' if both 'RecentInfo' structures point to se same resource, 'False' otherwise.
recentInfoMatch self infoB =
  liftM toBool $
  (\(RecentInfo arg1) (RecentInfo arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_recent_info_match argPtr1 argPtr2)
{-# LINE 349 "./Graphics/UI/Gtk/Recent/RecentInfo.chs" #-}
    self
    infoB

foreign import ccall safe "gtk_recent_info_exists"
  gtk_recent_info_exists :: ((Ptr RecentInfo) -> (IO CInt))

foreign import ccall safe "gtk_recent_info_get_added"
  gtk_recent_info_get_added :: ((Ptr RecentInfo) -> (IO CLong))

foreign import ccall safe "gtk_recent_info_get_age"
  gtk_recent_info_get_age :: ((Ptr RecentInfo) -> (IO CInt))

foreign import ccall safe "gtk_recent_info_get_application_info"
  gtk_recent_info_get_application_info :: ((Ptr RecentInfo) -> ((Ptr CChar) -> ((Ptr (Ptr CChar)) -> ((Ptr CUInt) -> ((Ptr CLong) -> (IO CInt))))))

foreign import ccall safe "gtk_recent_info_get_applications"
  gtk_recent_info_get_applications :: ((Ptr RecentInfo) -> ((Ptr CULong) -> (IO (Ptr (Ptr CChar)))))

foreign import ccall safe "gtk_recent_info_get_description"
  gtk_recent_info_get_description :: ((Ptr RecentInfo) -> (IO (Ptr CChar)))

foreign import ccall safe "gtk_recent_info_get_display_name"
  gtk_recent_info_get_display_name :: ((Ptr RecentInfo) -> (IO (Ptr CChar)))

foreign import ccall safe "gtk_recent_info_get_groups"
  gtk_recent_info_get_groups :: ((Ptr RecentInfo) -> ((Ptr CULong) -> (IO (Ptr (Ptr CChar)))))

foreign import ccall safe "gtk_recent_info_get_icon"
  gtk_recent_info_get_icon :: ((Ptr RecentInfo) -> (CInt -> (IO (Ptr Pixbuf))))

foreign import ccall safe "gtk_recent_info_get_mime_type"
  gtk_recent_info_get_mime_type :: ((Ptr RecentInfo) -> (IO (Ptr CChar)))

foreign import ccall safe "gtk_recent_info_get_modified"
  gtk_recent_info_get_modified :: ((Ptr RecentInfo) -> (IO CLong))

foreign import ccall safe "gtk_recent_info_get_private_hint"
  gtk_recent_info_get_private_hint :: ((Ptr RecentInfo) -> (IO CInt))

foreign import ccall safe "gtk_recent_info_get_short_name"
  gtk_recent_info_get_short_name :: ((Ptr RecentInfo) -> (IO (Ptr CChar)))

foreign import ccall safe "gtk_recent_info_get_uri"
  gtk_recent_info_get_uri :: ((Ptr RecentInfo) -> (IO (Ptr CChar)))

foreign import ccall safe "gtk_recent_info_get_uri_display"
  gtk_recent_info_get_uri_display :: ((Ptr RecentInfo) -> (IO (Ptr CChar)))

foreign import ccall safe "gtk_recent_info_get_visited"
  gtk_recent_info_get_visited :: ((Ptr RecentInfo) -> (IO CLong))

foreign import ccall safe "gtk_recent_info_has_application"
  gtk_recent_info_has_application :: ((Ptr RecentInfo) -> ((Ptr CChar) -> (IO CInt)))

foreign import ccall safe "gtk_recent_info_has_group"
  gtk_recent_info_has_group :: ((Ptr RecentInfo) -> ((Ptr CChar) -> (IO CInt)))

foreign import ccall safe "gtk_recent_info_is_local"
  gtk_recent_info_is_local :: ((Ptr RecentInfo) -> (IO CInt))

foreign import ccall safe "gtk_recent_info_last_application"
  gtk_recent_info_last_application :: ((Ptr RecentInfo) -> (IO (Ptr CChar)))

foreign import ccall safe "gtk_recent_info_match"
  gtk_recent_info_match :: ((Ptr RecentInfo) -> ((Ptr RecentInfo) -> (IO CInt)))
