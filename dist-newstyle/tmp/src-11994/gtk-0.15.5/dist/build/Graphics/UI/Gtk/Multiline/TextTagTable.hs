
{-# LINE 2 "./Graphics/UI/Gtk/Multiline/TextTagTable.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget TextTagTable
--
-- Author : Duncan Coutts
--
-- Created: 4 August 2004
--
-- Copyright (C) 2004-2005 Duncan Coutts
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
-- Collection of tags that can be used together
--
module Graphics.UI.Gtk.Multiline.TextTagTable (
-- * Detail
--
-- | You may wish to begin by reading the text widget conceptual overview
-- which gives an overview of all the objects and data types related to the
-- text widget and how they work together.

-- * Class Hierarchy
-- |
-- @
-- | 'GObject'
-- | +----TextTagTable
-- @

-- * Types
  TextTagTable,
  TextTagTableClass,
  castToTextTagTable, gTypeTextTagTable,
  toTextTagTable,

-- * Constructors
  textTagTableNew,

-- * Methods
  textTagTableAdd,
  textTagTableRemove,
  textTagTableLookup,
  textTagTableForeach,
  textTagTableGetSize
  ) where

import Control.Monad (liftM, void)

import System.Glib.FFI (withForeignPtr, nullPtr, Ptr(..), CInt(..), CChar(..), FunPtr(..), maybeNull)
import System.Glib.UTFString
import Graphics.UI.Gtk.Types
{-# LINE 64 "./Graphics/UI/Gtk/Multiline/TextTagTable.chs" #-}


{-# LINE 66 "./Graphics/UI/Gtk/Multiline/TextTagTable.chs" #-}

--------------------
-- Constructors

-- | Creates a new 'TextTagTable'. The table contains no tags by default.
--
textTagTableNew :: IO TextTagTable
textTagTableNew =
  wrapNewGObject mkTextTagTable $
  gtk_text_tag_table_new
{-# LINE 76 "./Graphics/UI/Gtk/Multiline/TextTagTable.chs" #-}

--------------------
-- Methods

-- | Add a tag to the table. The tag is assigned the highest priority in the
-- table.
--
-- The tag must not be in a tag table already, and may not have the same name as
-- an already-added tag.
--
textTagTableAdd :: (TextTagTableClass self, TextTagClass tag) => self -> tag -> IO ()
textTagTableAdd self tag = void $
  (\(TextTagTable arg1) (TextTag arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_text_tag_table_add argPtr1 argPtr2)
{-# LINE 89 "./Graphics/UI/Gtk/Multiline/TextTagTable.chs" #-}
    (toTextTagTable self)
    (toTextTag tag)

-- | Remove a tag from the table.
--
textTagTableRemove :: (TextTagTableClass self, TextTagClass tag) => self -> tag -> IO ()
textTagTableRemove self tag =
  (\(TextTagTable arg1) (TextTag arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_text_tag_table_remove argPtr1 argPtr2)
{-# LINE 97 "./Graphics/UI/Gtk/Multiline/TextTagTable.chs" #-}
    (toTextTagTable self)
    (toTextTag tag)

-- | Look up a named tag.
--
textTagTableLookup :: (TextTagTableClass self, GlibString string) => self
 -> string -- ^ @name@ - name of a tag
 -> IO (Maybe TextTag) -- ^ returns The tag, or @Nothing@ if none by that name
                       -- is in the table.
textTagTableLookup self name =
  maybeNull (makeNewGObject mkTextTag) $
  withUTFString name $ \namePtr ->
  (\(TextTagTable arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_text_tag_table_lookup argPtr1 arg2)
{-# LINE 110 "./Graphics/UI/Gtk/Multiline/TextTagTable.chs" #-}
    (toTextTagTable self)
    namePtr

-- | Maps over each tag in the table.
--
textTagTableForeach :: TextTagTableClass self => self
 -> (TextTag -> IO ())
 -> IO ()
textTagTableForeach self func = do
  funcPtr <- mkTextTagTableForeach (\tagPtr _ -> do
    tag <- makeNewGObject mkTextTag (return tagPtr)
    func tag)
  (\(TextTagTable arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_text_tag_table_foreach argPtr1 arg2 arg3)
{-# LINE 123 "./Graphics/UI/Gtk/Multiline/TextTagTable.chs" #-}
    (toTextTagTable self)
    funcPtr
    nullPtr

type TextTagTableForeach = FunPtr (((Ptr TextTag) -> ((Ptr ()) -> (IO ()))))
{-# LINE 128 "./Graphics/UI/Gtk/Multiline/TextTagTable.chs" #-}

foreign import ccall "wrapper" mkTextTagTableForeach ::
  (Ptr TextTag -> Ptr () -> IO ()) -> IO TextTagTableForeach

-- | Returns the size of the table (the number of tags).
--
textTagTableGetSize :: TextTagTableClass self => self -> IO Int
textTagTableGetSize self =
  liftM fromIntegral $
  (\(TextTagTable arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_text_tag_table_get_size argPtr1)
{-# LINE 138 "./Graphics/UI/Gtk/Multiline/TextTagTable.chs" #-}
    (toTextTagTable self)

foreign import ccall unsafe "gtk_text_tag_table_new"
  gtk_text_tag_table_new :: (IO (Ptr TextTagTable))

foreign import ccall safe "gtk_text_tag_table_add"
  gtk_text_tag_table_add :: ((Ptr TextTagTable) -> ((Ptr TextTag) -> (IO ())))

foreign import ccall safe "gtk_text_tag_table_remove"
  gtk_text_tag_table_remove :: ((Ptr TextTagTable) -> ((Ptr TextTag) -> (IO ())))

foreign import ccall unsafe "gtk_text_tag_table_lookup"
  gtk_text_tag_table_lookup :: ((Ptr TextTagTable) -> ((Ptr CChar) -> (IO (Ptr TextTag))))

foreign import ccall safe "gtk_text_tag_table_foreach"
  gtk_text_tag_table_foreach :: ((Ptr TextTagTable) -> ((FunPtr ((Ptr TextTag) -> ((Ptr ()) -> (IO ())))) -> ((Ptr ()) -> (IO ()))))

foreign import ccall unsafe "gtk_text_tag_table_get_size"
  gtk_text_tag_table_get_size :: ((Ptr TextTagTable) -> (IO CInt))
