
{-# LINE 2 "./Graphics/UI/Gtk/Embedding/Plug.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget Plug
--
-- Author : Axel Simon, Andy Stewart
--
-- Created: 23 May 2001
--
-- Copyright (C) 1999-2005 Axel Simon
-- Copyright (C) 2009 Andy Stewart
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
-- Toplevel for embedding into other processes
--
module Graphics.UI.Gtk.Embedding.Plug (
-- * Detail
--
-- | Together with 'Socket', 'Plug' provides the ability to embed widgets from
-- one process into another process in a fashion that is transparent to the
-- user. One process creates a 'Socket' widget and, passes the ID of that
-- widgets window to the other process, which then creates a 'Plug' with that
-- window ID. Any widgets contained in the 'Plug' then will appear inside the
-- first applications window.

-- * Class Hierarchy
-- |
-- @
-- | 'GObject'
-- | +----'Object'
-- | +----'Widget'
-- | +----'Container'
-- | +----'Bin'
-- | +----'Window'
-- | +----Plug
-- @


-- * Types
  Plug,
  PlugClass,
  castToPlug, gTypePlug,
  toPlug,
  NativeWindowId,

-- * Constructors
  plugNew,

  plugNewForDisplay,


-- * Methods
  plugGetId,

  plugGetEmbedded,
  plugGetSocketWindow,


-- * Attributes
  plugAttrEmbedded,
  plugAttrSocketWindow,

-- * Signals
  plugEmbedded,

  ) where



import Control.Monad (liftM)
import Data.Maybe (fromMaybe)

import System.Glib.FFI
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.UI.Gtk.Types
{-# LINE 91 "./Graphics/UI/Gtk/Embedding/Plug.chs" #-}
import Graphics.UI.Gtk.Embedding.Types
{-# LINE 92 "./Graphics/UI/Gtk/Embedding/Plug.chs" #-}
import Graphics.UI.Gtk.Signals
{-# LINE 93 "./Graphics/UI/Gtk/Embedding/Plug.chs" #-}

import Graphics.UI.Gtk.Embedding.Embedding

import Graphics.UI.Gtk.General.Structs


{-# LINE 99 "./Graphics/UI/Gtk/Embedding/Plug.chs" #-}

--------------------
-- Constructors

-- | Creates a new plug widget inside the 'Socket' identified by @socketId@.
-- If @socketId@ is @Nothing@, the plug is left \"unplugged\" and can later be
-- plugged into a 'Socket' by 'Graphics.UI.Gtk.Embedding.Socket.socketAddId'.
--
-- If a NativeWindowId is supplied the foreign application window will
-- immediatly appear in this 'Plug' once it is shown. If @Nothing@ is passed
-- then a 'NativeWindowId' can be extracted from this 'Plug' using 'plugGetId'
-- and be passed to the application which is to be embedded.
--
plugNew ::
  Maybe NativeWindowId -- ^ @socketId@ - the window ID of the socket, or
                       -- @Nothing@.
 -> IO Plug
plugNew socketId =
  makeNewObject mkPlug $
  liftM (castPtr :: Ptr Widget -> Ptr Plug) $
  gtk_plug_new
{-# LINE 120 "./Graphics/UI/Gtk/Embedding/Plug.chs" #-}
    (fromNativeWindowId (fromMaybe nativeWindowIdNone socketId))


-- | Create a new plug widget inside the 'Socket' identified by socket_id.
--
-- * Available since Gtk+ version 2.2
--
plugNewForDisplay ::
    Display -- ^ @display@ - the 'Display' on which @socketId@ is
                        -- displayed
 -> Maybe NativeWindowId -- ^ @socketId@ - the XID of the socket's window.
 -> IO Plug
plugNewForDisplay display socketId =
  makeNewObject mkPlug $
  liftM (castPtr :: Ptr Widget -> Ptr Plug) $
  (\(Display arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_plug_new_for_display argPtr1 arg2)
{-# LINE 136 "./Graphics/UI/Gtk/Embedding/Plug.chs" #-}
    display
    (fromNativeWindowId (fromMaybe nativeWindowIdNone socketId))


--------------------
-- Methods

-- | Gets the window ID of a 'Plug' widget, which can then be used to embed
-- this window inside another window, for instance with
-- 'Graphics.UI.Gtk.Embedding.Socket.socketAddId'.
--
plugGetId :: PlugClass self => self
 -> IO NativeWindowId -- ^ returns the window ID for the plug
plugGetId self =
  liftM toNativeWindowId $
  (\(Plug arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_plug_get_id argPtr1)
{-# LINE 152 "./Graphics/UI/Gtk/Embedding/Plug.chs" #-}
    (toPlug self)


-- | Determines whether the plug is embedded in a socket.
--
-- * Available since Gtk+ version 2.14
--
plugGetEmbedded :: PlugClass self => self
 -> IO Bool -- ^ returns @True@ if the plug is embedded in a socket
plugGetEmbedded self =
  liftM toBool $
  (\(Plug arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_plug_get_embedded argPtr1)
{-# LINE 164 "./Graphics/UI/Gtk/Embedding/Plug.chs" #-}
    (toPlug self)

-- | Retrieves the socket the plug is embedded in.
--
-- * Available since Gtk+ version 2.14
--
plugGetSocketWindow :: PlugClass self => self
 -> IO (Maybe DrawWindow) -- ^ returns the window of the socket
plugGetSocketWindow self =
  maybeNull (makeNewGObject mkDrawWindow) $
  (\(Plug arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_plug_get_socket_window argPtr1)
{-# LINE 175 "./Graphics/UI/Gtk/Embedding/Plug.chs" #-}
    (toPlug self)


--------------------
-- Attributes

-- | @True@ if the plug is embedded in a socket.
--
-- Default value: @False@
--
-- * Available since Gtk+ version 2.12
--
plugAttrEmbedded :: PlugClass self => ReadAttr self Bool
plugAttrEmbedded = readAttrFromBoolProperty "embedded"

-- | The window of the socket the plug is embedded in.
--
-- * Available since Gtk+ version 2.14
--
plugAttrSocketWindow :: PlugClass self => ReadAttr self (Maybe DrawWindow)
plugAttrSocketWindow = readAttrFromMaybeObjectProperty "socket-window"

                       gdk_window_object_get_type
{-# LINE 198 "./Graphics/UI/Gtk/Embedding/Plug.chs" #-}




--------------------
-- Signals

-- | Gets emitted when the plug becomes embedded in a socket and when the
-- embedding ends.
--
plugEmbedded :: PlugClass self => Signal self (IO ())
plugEmbedded = Signal (connect_NONE__NONE "embedded")

foreign import ccall unsafe "gtk_plug_new"
  gtk_plug_new :: ((Ptr ()) -> (IO (Ptr Widget)))

foreign import ccall safe "gtk_plug_new_for_display"
  gtk_plug_new_for_display :: ((Ptr Display) -> ((Ptr ()) -> (IO (Ptr Widget))))

foreign import ccall unsafe "gtk_plug_get_id"
  gtk_plug_get_id :: ((Ptr Plug) -> (IO (Ptr ())))

foreign import ccall safe "gtk_plug_get_embedded"
  gtk_plug_get_embedded :: ((Ptr Plug) -> (IO CInt))

foreign import ccall safe "gtk_plug_get_socket_window"
  gtk_plug_get_socket_window :: ((Ptr Plug) -> (IO (Ptr DrawWindow)))

foreign import ccall unsafe "gdk_window_object_get_type"
  gdk_window_object_get_type :: CULong
