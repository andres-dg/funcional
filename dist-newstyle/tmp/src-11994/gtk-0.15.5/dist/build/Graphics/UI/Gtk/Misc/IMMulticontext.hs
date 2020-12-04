
{-# LINE 2 "./Graphics/UI/Gtk/Misc/IMMulticontext.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget IMMulticontext
--
-- Author : Colin McQuillan
--
-- Created: 30 April 2009
--
-- Copyright (C) 2009 Colin McQuillan
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
-- An input method context supporting multiple, loadable input methods
--
module Graphics.UI.Gtk.Misc.IMMulticontext (

-- * Class Hierarchy
--
-- |
-- @
-- | 'GObject'
-- | +----'IMContext'
-- | +----IMMulticontext
-- @

-- * Types
  IMMulticontext,
  IMMulticontextClass,
  castToIMMulticontext, gTypeIMMulticontext,
  toIMMulticontext,

-- * Constructors
  imMulticontextNew,

-- * Methods
  imMulticontextAppendMenuitems,
  ) where

import System.Glib.FFI
import Graphics.UI.Gtk.Types
{-# LINE 54 "./Graphics/UI/Gtk/Misc/IMMulticontext.chs" #-}


{-# LINE 56 "./Graphics/UI/Gtk/Misc/IMMulticontext.chs" #-}

--------------------
-- Constructors

-- | Creates a new 'IMMulticontext'.
--
imMulticontextNew :: IO IMContext
imMulticontextNew =
  wrapNewGObject mkIMContext $
  gtk_im_multicontext_new
{-# LINE 66 "./Graphics/UI/Gtk/Misc/IMMulticontext.chs" #-}

--------------------
-- Methods

-- | Add menuitems for various available input methods to a menu; the
-- menuitems, when selected, will switch the input method for the context and
-- the global default input method.
--
imMulticontextAppendMenuitems :: (IMMulticontextClass self, MenuShellClass menushell) => self
 -> menushell -- ^ @menushell@ - a 'MenuShell'
 -> IO ()
imMulticontextAppendMenuitems self menushell =
  (\(IMMulticontext arg1) (MenuShell arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_im_multicontext_append_menuitems argPtr1 argPtr2)
{-# LINE 79 "./Graphics/UI/Gtk/Misc/IMMulticontext.chs" #-}
    (toIMMulticontext self)
    (toMenuShell menushell)

foreign import ccall safe "gtk_im_multicontext_new"
  gtk_im_multicontext_new :: (IO (Ptr IMContext))

foreign import ccall safe "gtk_im_multicontext_append_menuitems"
  gtk_im_multicontext_append_menuitems :: ((Ptr IMMulticontext) -> ((Ptr MenuShell) -> (IO ())))
