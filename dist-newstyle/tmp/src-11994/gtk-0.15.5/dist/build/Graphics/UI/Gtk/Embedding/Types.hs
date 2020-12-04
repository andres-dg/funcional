
{-# LINE 2 "./Graphics/UI/Gtk/Embedding/Types.chs" #-}
{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- -*-haskell-*-
-- -------------------- automatically generated file - do not edit ----------
-- Object hierarchy for the GIMP Toolkit (GTK) Binding for Haskell
--
-- Author : Hamish Mackenzie
--
-- Copyright (C) 2001-2005 Axel Simon
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
-- #hide

-- |
-- Maintainer : gtk2hs-users@lists.sourceforge.net
-- Stability : provisional
-- Portability : portable (depends on GHC)
--
module Graphics.UI.Gtk.Embedding.Types (


  Socket(Socket), SocketClass,
  toSocket,
  mkSocket, unSocket,
  castToSocket, gTypeSocket,
  Plug(Plug), PlugClass,
  toPlug,
  mkPlug, unPlug,
  castToPlug, gTypePlug,

  ) where


import Foreign.ForeignPtr (ForeignPtr, castForeignPtr)
-- TODO work around cpphs https:

import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)



import Foreign.C.Types (CULong(..), CUInt(..), CULLong(..))
import System.Glib.GType (GType, typeInstanceIsA)
import System.Glib.GObject
{-# LINE 54 "./Graphics/UI/Gtk/Embedding/Types.chs" #-}
import Graphics.UI.Gtk.General.Threading
import Graphics.UI.Gtk.Types
{-# LINE 56 "./Graphics/UI/Gtk/Embedding/Types.chs" #-}


{-# LINE 58 "./Graphics/UI/Gtk/Embedding/Types.chs" #-}

-- The usage of foreignPtrToPtr should be safe as the evaluation will only be
-- forced if the object is used afterwards
--
castTo :: (GObjectClass obj, GObjectClass obj') => GType -> String
                                                -> (obj -> obj')
castTo gtype objTypeName obj =
  case toGObject obj of
    gobj@(GObject objFPtr)
      | typeInstanceIsA ((unsafeForeignPtrToPtr.castForeignPtr) objFPtr) gtype
                  -> unsafeCastGObject gobj
      | otherwise -> error $ "Cannot cast object to " ++ objTypeName


-- ****************************************************************** Socket

newtype Socket = Socket (ForeignPtr (Socket)) deriving (Eq,Ord)

mkSocket = (Socket, objectUnrefFromMainloop)
unSocket (Socket o) = o

class ContainerClass o => SocketClass o
toSocket :: SocketClass o => o -> Socket
toSocket = unsafeCastGObject . toGObject

instance SocketClass Socket
instance ContainerClass Socket
instance WidgetClass Socket

instance ObjectClass Socket

instance GObjectClass Socket where
  toGObject = GObject . castForeignPtr . unSocket
  unsafeCastGObject = Socket . castForeignPtr . unGObject

castToSocket :: GObjectClass obj => obj -> Socket
castToSocket = castTo gTypeSocket "Socket"

gTypeSocket :: GType
gTypeSocket =
  gtk_socket_get_type
{-# LINE 99 "./Graphics/UI/Gtk/Embedding/Types.chs" #-}

-- ****************************************************************** Plug

newtype Plug = Plug (ForeignPtr (Plug)) deriving (Eq,Ord)

mkPlug = (Plug, objectUnrefFromMainloop)
unPlug (Plug o) = o

class WindowClass o => PlugClass o
toPlug :: PlugClass o => o -> Plug
toPlug = unsafeCastGObject . toGObject

instance PlugClass Plug
instance WindowClass Plug
instance BinClass Plug
instance ContainerClass Plug
instance WidgetClass Plug

instance ObjectClass Plug

instance GObjectClass Plug where
  toGObject = GObject . castForeignPtr . unPlug
  unsafeCastGObject = Plug . castForeignPtr . unGObject

castToPlug :: GObjectClass obj => obj -> Plug
castToPlug = castTo gTypePlug "Plug"

gTypePlug :: GType
gTypePlug =
  gtk_plug_get_type
{-# LINE 129 "./Graphics/UI/Gtk/Embedding/Types.chs" #-}

foreign import ccall unsafe "gtk_socket_get_type"
  gtk_socket_get_type :: CULong

foreign import ccall unsafe "gtk_plug_get_type"
  gtk_plug_get_type :: CULong
