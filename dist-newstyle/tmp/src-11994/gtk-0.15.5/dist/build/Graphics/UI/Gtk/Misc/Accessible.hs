
{-# LINE 2 "./Graphics/UI/Gtk/Misc/Accessible.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget accessible
--
-- Author : Andy Stewart
--
-- Created: 23 Oct 2010
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
-- A 'Object' representing an adjustable bounded value
--
module Graphics.UI.Gtk.Misc.Accessible (
-- * Detail
--
-- | Accessible accessibility support for widgets.
--

-- * Class Hierarchy
-- |
-- @
-- | 'GObject'
-- | +----'AtkObject'
-- | +----Accessible
-- @


-- * Types
  Accessible,
  AccessibleClass,
  castToAccessible, gTypeAccessible,
  toAccessible,

-- * Methods
  accessibleGetWidget,
  accessibleSetWidget

) where



import System.Glib.FFI
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.UI.Gtk.Types
{-# LINE 60 "./Graphics/UI/Gtk/Misc/Accessible.chs" #-}


{-# LINE 62 "./Graphics/UI/Gtk/Misc/Accessible.chs" #-}

--------------------
-- Methods

-- | Gets the 'Widget' corresponding to the 'Accessible'.
--
-- * Available since Gtk+ version 2.22
--
accessibleGetWidget :: AccessibleClass self => self
                    -> IO (Maybe Widget) -- ^ returns the 'Widget' corresponding to the 'Accessible', or 'Nothing'.
accessibleGetWidget self =
  maybeNull (makeNewObject mkWidget) $
  (\(Accessible arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_accessible_get_widget argPtr1)
{-# LINE 75 "./Graphics/UI/Gtk/Misc/Accessible.chs" #-}
    (toAccessible self)

-- | Sets the 'Widget' corresponding to the 'Accessible'.
--
-- * Available since Gtk+ version 2.22
--
accessibleSetWidget :: (AccessibleClass self, WidgetClass widget)
                      => self -- ^ @accessible@ a 'Accessible'
                      -> widget -- ^ @widget@ a 'Widget'
                      -> IO ()
accessibleSetWidget self widget =
  (\(Accessible arg1) (Widget arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_accessible_set_widget argPtr1 argPtr2)
{-# LINE 87 "./Graphics/UI/Gtk/Misc/Accessible.chs" #-}
     (toAccessible self)
     (toWidget widget)

foreign import ccall safe "gtk_accessible_get_widget"
  gtk_accessible_get_widget :: ((Ptr Accessible) -> (IO (Ptr Widget)))

foreign import ccall safe "gtk_accessible_set_widget"
  gtk_accessible_set_widget :: ((Ptr Accessible) -> ((Ptr Widget) -> (IO ())))
