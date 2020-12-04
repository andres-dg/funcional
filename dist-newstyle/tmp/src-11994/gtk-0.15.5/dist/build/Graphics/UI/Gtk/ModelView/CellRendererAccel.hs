
{-# LINE 2 "./Graphics/UI/Gtk/ModelView/CellRendererAccel.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget CellRendererAccel
--
-- Author : Andy Stewart
--
-- Created: 25 Mar 2010
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
-- Renders a keyboard accelerator in a cell
--
-- * Module available since Gtk+ version 2.10
--
module Graphics.UI.Gtk.ModelView.CellRendererAccel (

-- * Detail
--
-- | 'CellRendererAccel' displays a keyboard accelerator (i.e. a key
-- combination like \<Control>-a). If the cell renderer is editable, the
-- accelerator can be changed by simply typing the new combination.
--
-- The 'CellRendererAccel' cell renderer was added in Gtk+ 2.10.

-- * Class Hierarchy
--
-- |
-- @
-- | 'GObject'
-- | +----'Object'
-- | +----'CellRenderer'
-- | +----'CellRendererText'
-- | +----CellRendererAccel
-- @


-- * Types
  CellRendererAccel,
  CellRendererAccelClass,
  castToCellRendererAccel,
  toCellRendererAccel,

-- * Enums
  CellRendererAccelMode(..),

-- * Constructors
  cellRendererAccelNew,

-- * Attributes
  cellRendererAccelAccelKey,
  cellRendererAccelAccelMods,
  cellRendererAccelKeycode,
  cellRendererAccelAccelMode,

-- * Signals
  accelEdited,
  accelCleared,

  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Gdk.Enums (Modifier)
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.UI.Gtk.Gdk.Keys (KeyVal, KeyCode)
import Graphics.UI.Gtk.Signals
{-# LINE 86 "./Graphics/UI/Gtk/ModelView/CellRendererAccel.chs" #-}
import Graphics.UI.Gtk.Types
{-# LINE 87 "./Graphics/UI/Gtk/ModelView/CellRendererAccel.chs" #-}


{-# LINE 89 "./Graphics/UI/Gtk/ModelView/CellRendererAccel.chs" #-}


--------------------
-- Enums
data CellRendererAccelMode = CellRendererAccelModeGtk
                           | CellRendererAccelModeOther
                           deriving (Enum,Bounded,Eq,Show)

{-# LINE 94 "./Graphics/UI/Gtk/ModelView/CellRendererAccel.chs" #-}

--------------------
-- Constructors

-- | Creates a new 'CellRendererAccel'.
--
-- * Available since Gtk+ version 2.10
--
cellRendererAccelNew :: IO CellRendererAccel
cellRendererAccelNew =
  makeNewObject mkCellRendererAccel $ liftM castPtr $
  gtk_cell_renderer_accel_new
{-# LINE 106 "./Graphics/UI/Gtk/ModelView/CellRendererAccel.chs" #-}

--------------------
-- Attributes

-- | The keyval of the accelerator.
--
-- Allowed values: <= GMaxint
--
-- Default value: 0
--
-- * Available since Gtk+ version 2.10
--
cellRendererAccelAccelKey :: CellRendererAccelClass self => Attr self Int
cellRendererAccelAccelKey = newAttrFromUIntProperty "accel-key"

-- | The modifier mask of the accelerator.
--
-- * Available since Gtk+ version 2.10
--
cellRendererAccelAccelMods :: CellRendererAccelClass self => Attr self [Modifier]
cellRendererAccelAccelMods = newAttrFromFlagsProperty "accel-mods"
                               gdk_modifier_type_get_type
{-# LINE 128 "./Graphics/UI/Gtk/ModelView/CellRendererAccel.chs" #-}

-- | The hardware keycode of the accelerator. Note that the hardware keycode is only relevant if the key
-- does not have a keyval. Normally, the keyboard configuration should assign keyvals to all keys.
--
-- Allowed values: <= GMaxint
--
-- Default value: 0
--
-- * Available since Gtk+ version 2.10
--
cellRendererAccelKeycode :: CellRendererAccelClass self => Attr self Int
cellRendererAccelKeycode = newAttrFromUIntProperty "keycode"

-- | Determines if the edited accelerators are GTK+ accelerators. If they are, consumed modifiers are
-- suppressed, only accelerators accepted by GTK+ are allowed, and the accelerators are rendered in the
-- same way as they are in menus.
--
-- Default value: 'CellRendererAccelModeGtk'
--
-- * Available since Gtk+ version 2.10
--
cellRendererAccelAccelMode :: CellRendererAccelClass self => Attr self CellRendererAccelMode
cellRendererAccelAccelMode = newAttrFromEnumProperty "accel-mode"
                               gtk_cell_renderer_accel_mode_get_type
{-# LINE 152 "./Graphics/UI/Gtk/ModelView/CellRendererAccel.chs" #-}

--------------------
-- Signals

-- | Gets emitted when the user has selected a new accelerator.
--
-- * Available since Gtk+ version 2.10
--
accelEdited :: (CellRendererAccelClass self, GlibString string) => Signal self (string -> KeyVal -> Modifier -> KeyCode -> IO ())
accelEdited = Signal (\after obj user -> connect_GLIBSTRING_INT_ENUM_INT__NONE "accel_edited" after obj
                      (\ path keyval modifier keycode ->
                        user path (fromIntegral keyval) modifier (fromIntegral keycode)))

-- | Gets emitted when the user has removed the accelerator.
--
-- * Available since Gtk+ version 2.10
--
accelCleared :: (CellRendererAccelClass self, GlibString string) => Signal self (string -> IO ())
accelCleared = Signal (connect_GLIBSTRING__NONE "accel_cleared")

foreign import ccall safe "gtk_cell_renderer_accel_new"
  gtk_cell_renderer_accel_new :: (IO (Ptr CellRenderer))

foreign import ccall unsafe "gdk_modifier_type_get_type"
  gdk_modifier_type_get_type :: CULong

foreign import ccall unsafe "gtk_cell_renderer_accel_mode_get_type"
  gtk_cell_renderer_accel_mode_get_type :: CULong
