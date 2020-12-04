
{-# LINE 2 "./Graphics/UI/Gtk/Gdk/DisplayManager.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget DisplayManager
--
-- Author : Andy Stewart
--
-- Created: 29 Mar 2010
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
-- Maintains a list of all open GdkDisplays
--
-- * Module available since Gdk version 2.2
--
module Graphics.UI.Gtk.Gdk.DisplayManager (

-- * Detail
--
-- | The purpose of the 'DisplayManager' singleton object is to offer
-- notification when displays appear or disappear or the default display
-- changes.

-- * Class Hierarchy
--
-- |
-- @
-- | 'GObject'
-- | +----DisplayManager
-- @


-- * Types
  DisplayManager,
  DisplayManagerClass,
  castToDisplayManager,
  toDisplayManager,

-- * Methods
  displayManagerGet,
  displayManagerListDisplays,

-- * Attributes
  displayManagerDefaultDisplay,

-- * Signals
  displayManagerOpened,

  ) where

import System.Glib.FFI
import System.Glib.Attributes
import System.Glib.Properties
import System.Glib.GList
import Graphics.UI.Gtk.Types
{-# LINE 70 "./Graphics/UI/Gtk/Gdk/DisplayManager.chs" #-}
import Graphics.UI.Gtk.Signals
{-# LINE 71 "./Graphics/UI/Gtk/Gdk/DisplayManager.chs" #-}


{-# LINE 73 "./Graphics/UI/Gtk/Gdk/DisplayManager.chs" #-}


--------------------
-- Methods

-- | Returns the global 'DisplayManager' singleton; 'parsePargs', 'init', or
-- 'initCheck' must have been called first.
--
displayManagerGet :: IO DisplayManager -- ^ returns the singleton 'DisplayManager' object.
displayManagerGet =
  constructNewGObject mkDisplayManager $
  gdk_display_manager_get
{-# LINE 85 "./Graphics/UI/Gtk/Gdk/DisplayManager.chs" #-}

-- | List all currently open displays.
--
displayManagerListDisplays :: DisplayManagerClass self => self
 -> IO [Display] -- ^ returns a newly allocated list of 'Display' objects.
displayManagerListDisplays self =
  (\(DisplayManager arg1) -> withForeignPtr arg1 $ \argPtr1 ->gdk_display_manager_list_displays argPtr1)
{-# LINE 92 "./Graphics/UI/Gtk/Gdk/DisplayManager.chs" #-}
    (toDisplayManager self)
  >>= fromGSList
  >>= mapM (makeNewGObject mkDisplay . return)

--------------------
-- Attributes

-- | The default display.
--
displayManagerDefaultDisplay :: DisplayManagerClass self => Attr self Display
displayManagerDefaultDisplay = newAttrFromObjectProperty "default-display"
                                 gdk_display_get_type
{-# LINE 104 "./Graphics/UI/Gtk/Gdk/DisplayManager.chs" #-}

--------------------
-- Signals

-- | The 'displayManagerOpened' signal is emitted when a display is opened.
--
displayManagerOpened :: DisplayManagerClass self => Signal self (Display -> IO ())
displayManagerOpened = Signal (connect_OBJECT__NONE "display_opened")

foreign import ccall safe "gdk_display_manager_get"
  gdk_display_manager_get :: (IO (Ptr DisplayManager))

foreign import ccall safe "gdk_display_manager_list_displays"
  gdk_display_manager_list_displays :: ((Ptr DisplayManager) -> (IO (Ptr ())))

foreign import ccall unsafe "gdk_display_get_type"
  gdk_display_get_type :: CULong
