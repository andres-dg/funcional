
{-# LINE 2 "./Graphics/UI/Gtk/Gdk/Keys.chs" #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LINE 3 "./Graphics/UI/Gtk/Gdk/Keys.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Keys
--
-- Author : Jens Petersen
--
-- Created: 24 May 2002
--
-- Copyright (C) 2002-2005 Jens Petersen
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
-- #prune

-- |
-- Maintainer : gtk2hs-users@lists.sourceforge.net
-- Stability : provisional
-- Portability : portable (depends on GHC)
--
-- A 'KeyVal' is a numeric value identifying a keyboard key. The defined
-- values can be found at <http:
-- The names of the keys are the names of the macros without the prefix.
--
module Graphics.UI.Gtk.Gdk.Keys (
  KeyVal,
  KeyCode,
  keyName,
  keyFromName,
  keyToChar,

  keyvalName,
  keyvalFromName,
  keyvalToChar,
  keyvalConvertCase,
  keyvalToUpper,
  keyvalToLower,
  keyvalIsUpper,
  keyvalIsLower,
  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.UTFString


{-# LINE 55 "./Graphics/UI/Gtk/Gdk/Keys.chs" #-}

-- | Key values are the codes which are sent whenever a key is pressed or
-- released.
--
type KeyVal = Word32
type KeyCode = Word16

-- | Converts a key value into a symbolic name.
--
keyName :: KeyVal -> DefaultGlibString
keyName k = unsafePerformIO $ keyvalName k

-- | Converts a key name to a key value.
--
keyFromName :: DefaultGlibString -> KeyVal
keyFromName k = unsafePerformIO $ keyvalFromName k

-- | Convert from a Gdk key symbol to the corresponding Unicode character.
--
keyToChar ::
    KeyVal -- ^ @keyval@ - a Gdk key symbol
 -> Maybe Char -- ^ returns the corresponding unicode character, or
               -- Nothing if there is no corresponding character.
keyToChar k = unsafePerformIO $ keyvalToChar k

keyvalName :: KeyVal -> IO DefaultGlibString
keyvalName keyval = do
  strPtr <- gdk_keyval_name (fromIntegral keyval)
  if strPtr==nullPtr then return "" else peekUTFString strPtr

keyvalFromName :: DefaultGlibString -> IO KeyVal
keyvalFromName keyvalName =
  liftM fromIntegral $
  withUTFString keyvalName $ \keyvalNamePtr ->
  gdk_keyval_from_name
{-# LINE 90 "./Graphics/UI/Gtk/Gdk/Keys.chs" #-}
    keyvalNamePtr

keyvalToChar :: KeyVal -> IO (Maybe Char)
keyvalToChar keyval =
  gdk_keyval_to_unicode (fromIntegral keyval)
  >>= \code -> if code == 0 then return Nothing
                            else return $ Just $ toEnum $ fromIntegral code

-- | Obtains the upper- and lower-case versions of the keyval symbol. Examples of keyvals are GDK_a,
-- 'Enter', 'F1', etc.
keyvalConvertCase :: KeyVal -- ^ @symbol@ a keyval
                  -> (KeyVal, KeyVal) -- ^ @(lower, upper)@
                                        -- ^ lower is the lowercase version of symbol.
                                        -- ^ upper is uppercase version of symbol.
keyvalConvertCase keyval =
  unsafePerformIO $
  alloca $ \ lowerPtr ->
  alloca $ \ upperPtr -> do
  gdk_keyval_convert_case
{-# LINE 109 "./Graphics/UI/Gtk/Gdk/Keys.chs" #-}
    (fromIntegral keyval)
    lowerPtr
    upperPtr
  lower <- peek lowerPtr
  upper <- peek upperPtr
  return (fromIntegral lower, fromIntegral upper)

-- | Converts a key value to upper case, if applicable.
keyvalToUpper :: KeyVal -- ^ @keyval@ a key value.
              -> KeyVal -- ^ returns the upper case form of keyval,
                          -- or keyval itself if it is already in upper case or it is not subject to case
keyvalToUpper keyval =
  unsafePerformIO $
  liftM fromIntegral $
  gdk_keyval_to_upper
{-# LINE 124 "./Graphics/UI/Gtk/Gdk/Keys.chs" #-}
     (fromIntegral keyval)

-- | Converts a key value to lower case, if applicable.
keyvalToLower :: KeyVal -- ^ @keyval@ a key value.
              -> KeyVal -- ^ returns the lower case form of keyval,
                          -- or keyval itself if it is already in lower case or it is not subject to case
keyvalToLower keyval =
  unsafePerformIO $
  liftM fromIntegral $
  gdk_keyval_to_lower
{-# LINE 134 "./Graphics/UI/Gtk/Gdk/Keys.chs" #-}
     (fromIntegral keyval)

-- | Returns 'True' if the given key value is in upper case.
keyvalIsLower :: KeyVal
              -> Bool -- ^ returns 'True' if keyval is in upper case, or if keyval is not subject to case conversion.
keyvalIsLower keyval =
  unsafePerformIO $
  liftM toBool $
  gdk_keyval_is_lower
{-# LINE 143 "./Graphics/UI/Gtk/Gdk/Keys.chs" #-}
     (fromIntegral keyval)

-- | Returns 'True' if the given key value is in upper case.
keyvalIsUpper :: KeyVal
              -> Bool -- ^ returns 'True' if keyval is in upper case, or if keyval is not subject to case conversion.
keyvalIsUpper keyval =
  unsafePerformIO $
  liftM toBool $
  gdk_keyval_is_upper
{-# LINE 152 "./Graphics/UI/Gtk/Gdk/Keys.chs" #-}
     (fromIntegral keyval)

foreign import ccall safe "gdk_keyval_name"
  gdk_keyval_name :: (CUInt -> (IO (Ptr CChar)))

foreign import ccall safe "gdk_keyval_from_name"
  gdk_keyval_from_name :: ((Ptr CChar) -> (IO CUInt))

foreign import ccall safe "gdk_keyval_to_unicode"
  gdk_keyval_to_unicode :: (CUInt -> (IO CUInt))

foreign import ccall safe "gdk_keyval_convert_case"
  gdk_keyval_convert_case :: (CUInt -> ((Ptr CUInt) -> ((Ptr CUInt) -> (IO ()))))

foreign import ccall safe "gdk_keyval_to_upper"
  gdk_keyval_to_upper :: (CUInt -> (IO CUInt))

foreign import ccall safe "gdk_keyval_to_lower"
  gdk_keyval_to_lower :: (CUInt -> (IO CUInt))

foreign import ccall safe "gdk_keyval_is_lower"
  gdk_keyval_is_lower :: (CUInt -> (IO CInt))

foreign import ccall safe "gdk_keyval_is_upper"
  gdk_keyval_is_upper :: (CUInt -> (IO CInt))
