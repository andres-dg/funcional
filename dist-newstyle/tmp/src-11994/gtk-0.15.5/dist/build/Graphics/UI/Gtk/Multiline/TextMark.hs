
{-# LINE 2 "./Graphics/UI/Gtk/Multiline/TextMark.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) TextMark TextBuffer
--
-- Author : Axel Simon
--
-- Created: 23 February 2002
--
-- Copyright (C) 2002-2005 Axel Simon
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
-- A position in the buffer preserved across buffer modifications
--
module Graphics.UI.Gtk.Multiline.TextMark (
-- * Detail
--
-- | You may wish to begin by reading the text widget conceptual overview
-- which gives an overview of all the objects and data types related to the
-- text widget and how they work together.
--
-- A 'TextMark' is like a bookmark in a text buffer; it preserves a position
-- in the text. You can convert the mark to an iterator using
-- 'textBufferGetIterAtMark'. Unlike
-- iterators, marks remain valid across buffer mutations, because their
-- behavior is defined when text is inserted or deleted. When text containing
-- a mark is deleted, the mark remains in the position originally occupied by
-- the deleted text. When text is inserted at a mark, a mark with left
-- gravity will be moved to the beginning of the newly-inserted text, and a
-- mark with right gravity will be moved to the end.
--
-- Marks can be deleted from the buffer at any time with
-- 'textBufferDeleteMark'. Once deleted
-- from the buffer, a mark is essentially useless.
--
-- Marks optionally have names; these can be convenient to avoid passing the
-- 'TextMark' object around.
--
-- Marks are typically created using the
-- 'textBufferCreateMark' function.

-- * Class Hierarchy
-- |
-- @
-- | 'GObject'
-- | +----TextMark
-- @

-- * Types
  TextMark,
  TextMarkClass,
  castToTextMark, gTypeTextMark,
  toTextMark,
  MarkName,

-- * Constructors

  textMarkNew,


-- * Methods
  textMarkSetVisible,
  textMarkGetVisible,
  textMarkGetDeleted,
  textMarkGetName,
  textMarkGetBuffer,
  textMarkGetLeftGravity,

-- * Attributes
  textMarkName,
  textMarkVisible,
  textMarkLeftGravity
  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Types
{-# LINE 95 "./Graphics/UI/Gtk/Multiline/TextMark.chs" #-}


{-# LINE 97 "./Graphics/UI/Gtk/Multiline/TextMark.chs" #-}

-- | The name of a mark.
type MarkName = DefaultGlibString

--------------------
-- Constructors


-- | Creates a text mark. Add it to a buffer using 'textBufferAddMark'. If
-- @name@ is @Nothing@, the mark is anonymous; otherwise, the mark can be retrieved by
-- this name
-- using 'textBufferGetMark'. If a mark has left gravity, and text is inserted
-- at the mark's current location, the mark will be moved to the left of the
-- newly-inserted text. If the mark has right gravity (@leftGravity@ =
-- @False@), the mark will end up on the right of newly-inserted text. The
-- standard left-to-right cursor is a mark with right gravity (when you type,
-- the cursor stays on the right side of the text you\'re typing).
--
-- * Available since Gtk+ version 2.12
--
textMarkNew ::
  Maybe MarkName -- ^ @markName@ - name for mark, or @Nothing@
 -> Bool -- ^ @leftGravity@ - whether the mark has left gravity
 -> IO TextMark
textMarkNew markName leftGravity =
  wrapNewGObject mkTextMark $
  maybeWith withUTFString markName $ \markNamePtr ->
  gtk_text_mark_new
{-# LINE 125 "./Graphics/UI/Gtk/Multiline/TextMark.chs" #-}
    markNamePtr
    (fromBool leftGravity)


--------------------
-- Methods

-- | Sets the visibility of @mark@; the insertion point is normally visible,
-- i.e. you can see it as a vertical bar. Also, the text widget uses a visible
-- mark to indicate where a drop will occur when dragging-and-dropping text.
-- Most other marks are not visible. Marks are not visible by default.
--
textMarkSetVisible :: TextMarkClass self => self -> Bool -> IO ()
textMarkSetVisible self setting =
  (\(TextMark arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_text_mark_set_visible argPtr1 arg2)
{-# LINE 140 "./Graphics/UI/Gtk/Multiline/TextMark.chs" #-}
    (toTextMark self)
    (fromBool setting)

-- | Returns @True@ if the mark is visible (i.e. a cursor is displayed for it)
--
textMarkGetVisible :: TextMarkClass self => self -> IO Bool
textMarkGetVisible self =
  liftM toBool $
  (\(TextMark arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_text_mark_get_visible argPtr1)
{-# LINE 149 "./Graphics/UI/Gtk/Multiline/TextMark.chs" #-}
    (toTextMark self)

-- | Returns @True@ if the mark has been removed from its buffer with
-- 'textBufferDeleteMark'. Marks can't
-- be used once deleted.
--
textMarkGetDeleted :: TextMarkClass self => self -> IO Bool
textMarkGetDeleted self =
  liftM toBool $
  (\(TextMark arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_text_mark_get_deleted argPtr1)
{-# LINE 159 "./Graphics/UI/Gtk/Multiline/TextMark.chs" #-}
    (toTextMark self)

-- | Returns the mark name; returns @Nothing@ for anonymous marks.
--
textMarkGetName :: TextMarkClass self => self -> IO (Maybe MarkName)
textMarkGetName self =
  (\(TextMark arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_text_mark_get_name argPtr1)
{-# LINE 166 "./Graphics/UI/Gtk/Multiline/TextMark.chs" #-}
    (toTextMark self)
  >>= maybePeek peekUTFString

-- | Gets the buffer this mark is located inside, or @Nothing@ if the mark is
-- deleted.
--
textMarkGetBuffer :: TextMarkClass self => self -> IO (Maybe TextBuffer)
textMarkGetBuffer self =
  maybeNull (makeNewGObject mkTextBuffer) $
  (\(TextMark arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_text_mark_get_buffer argPtr1)
{-# LINE 176 "./Graphics/UI/Gtk/Multiline/TextMark.chs" #-}
    (toTextMark self)

-- | Determines whether the mark has left gravity.
--
-- The name is misleading as Arabic, Hebrew and some other languages have the
-- beginning of a line towards the right.
--
textMarkGetLeftGravity :: TextMarkClass self => self -> IO Bool
textMarkGetLeftGravity self =
  liftM toBool $
  (\(TextMark arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_text_mark_get_left_gravity argPtr1)
{-# LINE 187 "./Graphics/UI/Gtk/Multiline/TextMark.chs" #-}
    (toTextMark self)

--------------------
-- Attributes

-- | Retreives the name of a mark.
--
textMarkName :: TextMarkClass self => ReadAttr self (Maybe MarkName)
textMarkName = readAttrFromMaybeStringProperty "name"

-- | The \'visible\' property. See 'textMarkGetVisible' and 'textMarkSetVisible'
--
textMarkVisible :: TextMarkClass self => Attr self Bool
textMarkVisible = newAttr
  textMarkGetVisible
  textMarkSetVisible

-- | Determines whether the mark keeps to the left when text is inserted at its position.
--
textMarkLeftGravity :: TextMarkClass self => ReadAttr self Bool
textMarkLeftGravity = readAttrFromBoolProperty "left-gravity"

foreign import ccall safe "gtk_text_mark_new"
  gtk_text_mark_new :: ((Ptr CChar) -> (CInt -> (IO (Ptr TextMark))))

foreign import ccall unsafe "gtk_text_mark_set_visible"
  gtk_text_mark_set_visible :: ((Ptr TextMark) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_text_mark_get_visible"
  gtk_text_mark_get_visible :: ((Ptr TextMark) -> (IO CInt))

foreign import ccall unsafe "gtk_text_mark_get_deleted"
  gtk_text_mark_get_deleted :: ((Ptr TextMark) -> (IO CInt))

foreign import ccall unsafe "gtk_text_mark_get_name"
  gtk_text_mark_get_name :: ((Ptr TextMark) -> (IO (Ptr CChar)))

foreign import ccall unsafe "gtk_text_mark_get_buffer"
  gtk_text_mark_get_buffer :: ((Ptr TextMark) -> (IO (Ptr TextBuffer)))

foreign import ccall unsafe "gtk_text_mark_get_left_gravity"
  gtk_text_mark_get_left_gravity :: ((Ptr TextMark) -> (IO CInt))