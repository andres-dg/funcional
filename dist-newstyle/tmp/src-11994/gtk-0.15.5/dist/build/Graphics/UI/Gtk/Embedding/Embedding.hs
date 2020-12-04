{-# LINE 1 "Graphics/UI/Gtk/Embedding/Embedding.hsc" #-}
-- -*-haskell-*-




--  GIMP Toolkit (GTK) Widget Socket
--
--  Author : Axel Simon
--
--  Created: 20 January 2003
--
--  Copyright (C) 1999-2005 Axel Simon
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public
--  License as published by the Free Software Foundation; either
--  version 2.1 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
module Graphics.UI.Gtk.Embedding.Embedding (

{-# LINE 31 "Graphics/UI/Gtk/Embedding/Embedding.hsc" #-}
  socketHasPlug,

{-# LINE 33 "Graphics/UI/Gtk/Embedding/Embedding.hsc" #-}
  ) where


{-# LINE 36 "Graphics/UI/Gtk/Embedding/Embedding.hsc" #-}
import System.Glib.FFI
import Graphics.UI.Gtk.Types
import Graphics.UI.Gtk.Embedding.Types

-- | Test if a Plug is connected to the socket.
--
socketHasPlug :: SocketClass s => s -> IO Bool
socketHasPlug socket = do
  plugPtr <- withForeignPtr (unSocket (toSocket socket))
             (\hsc_ptr -> peekByteOff hsc_ptr 120)
{-# LINE 46 "Graphics/UI/Gtk/Embedding/Embedding.hsc" #-}
  return (plugPtr/=nullPtr)


{-# LINE 49 "Graphics/UI/Gtk/Embedding/Embedding.hsc" #-}
