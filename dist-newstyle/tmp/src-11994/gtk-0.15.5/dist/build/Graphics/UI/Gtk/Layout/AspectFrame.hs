
{-# LINE 2 "./Graphics/UI/Gtk/Layout/AspectFrame.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget AspectFrame
--
-- Author : Axel Simon
--
-- Created: 15 May 2001
--
-- Copyright (C) 1999-2005 Axel Simon
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
-- A frame that constrains its child to a particular aspect ratio
--
module Graphics.UI.Gtk.Layout.AspectFrame (
-- * Detail
--
-- | The 'AspectFrame' is useful when you want pack a widget so that it can
-- resize but always retains the same aspect ratio. For instance, one might be
-- drawing a small preview of a larger image. 'AspectFrame' derives from
-- 'Frame', so it can draw a label and a frame around the child. The frame will
-- be \"shrink-wrapped\" to the size of the child.

-- * Class Hierarchy
-- |
-- @
-- | 'GObject'
-- | +----'Object'
-- | +----'Widget'
-- | +----'Container'
-- | +----'Bin'
-- | +----'Frame'
-- | +----AspectFrame
-- @

-- * Types
  AspectFrame,
  AspectFrameClass,
  castToAspectFrame, gTypeAspectFrame,
  toAspectFrame,

-- * Constructors
  aspectFrameNew,

-- * Methods
  aspectFrameSet,

-- * Attributes
  aspectFrameXAlign,
  aspectFrameYAlign,
  aspectFrameRatio,
  aspectFrameObeyChild,
  ) where

import Control.Monad (liftM)
import Data.Maybe (isNothing)

import System.Glib.FFI
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.UI.Gtk.Types
{-# LINE 76 "./Graphics/UI/Gtk/Layout/AspectFrame.chs" #-}


{-# LINE 78 "./Graphics/UI/Gtk/Layout/AspectFrame.chs" #-}

--------------------
-- Constructors

-- | Create a new 'AspectFrame'.
--
-- The frame may be augmented with a label which can be set by @frameSetLabel@.
--
aspectFrameNew ::
    Float -- ^ @xalign@ - Horizontal alignment of the child within
                   -- the allocation of the 'AspectFrame'. This ranges from 0.0
                   -- (left aligned) to 1.0 (right aligned)
 -> Float -- ^ @yalign@ - Vertical alignment of the child within the
                   -- allocation of the 'AspectFrame'. This ranges from 0.0
                   -- (left aligned) to 1.0 (right aligned)
 -> Maybe Float -- ^ @ratio@ - The desired aspect ratio. If @Nothing@ the
                   -- aspect ratio is taken from the requistion of the child.
 -> IO AspectFrame
aspectFrameNew xalign yalign ratio =
  makeNewObject mkAspectFrame $
  liftM (castPtr :: Ptr Widget -> Ptr AspectFrame) $
  gtk_aspect_frame_new
{-# LINE 100 "./Graphics/UI/Gtk/Layout/AspectFrame.chs" #-}
    nullPtr
    (realToFrac xalign)
    (realToFrac yalign)
    (maybe 0.0 realToFrac ratio)
    (fromBool $ isNothing ratio)

--------------------
-- Methods

-- | Set parameters for an existing 'AspectFrame'.
--
aspectFrameSet :: AspectFrameClass self => self
 -> Float -- ^ @xalign@ - Horizontal alignment of the child within the
          -- allocation of the 'AspectFrame'. This ranges from 0.0 (left
          -- aligned) to 1.0 (right aligned)
 -> Float -- ^ @yalign@ - Vertical alignment of the child within the
          -- allocation of the 'AspectFrame'. This ranges from 0.0 (left
          -- aligned) to 1.0 (right aligned)
 -> Maybe Float -- ^ @ratio@ - The desired aspect ratio. If @Nothing@ the
                -- aspect ratio is taken from the requistion of the child.
 -> IO ()
aspectFrameSet self xalign yalign ratio =
  (\(AspectFrame arg1) arg2 arg3 arg4 arg5 -> withForeignPtr arg1 $ \argPtr1 ->gtk_aspect_frame_set argPtr1 arg2 arg3 arg4 arg5)
{-# LINE 123 "./Graphics/UI/Gtk/Layout/AspectFrame.chs" #-}
    (toAspectFrame self)
    (realToFrac xalign)
    (realToFrac yalign)
    (maybe 0.0 realToFrac ratio)
    (fromBool $ isNothing ratio)

--------------------
-- Attributes

-- | X alignment of the child.
--
-- Allowed values: [0,1]
--
-- Default value: 0.5
--
aspectFrameXAlign :: AspectFrameClass self => Attr self Float
aspectFrameXAlign = newAttrFromFloatProperty "xalign"

-- | Y alignment of the child.
--
-- Allowed values: [0,1]
--
-- Default value: 0.5
--
aspectFrameYAlign :: AspectFrameClass self => Attr self Float
aspectFrameYAlign = newAttrFromFloatProperty "yalign"

-- | Aspect ratio if obey_child is @False@.
--
-- Allowed values: [1e-04,10000]
--
-- Default value: 0.5
--
aspectFrameRatio :: AspectFrameClass self => Attr self Float
aspectFrameRatio = newAttrFromFloatProperty "ratio"

-- | Force aspect ratio to match that of the frame's child.
--
-- Default value: @True@
--
aspectFrameObeyChild :: AspectFrameClass self => Attr self Bool
aspectFrameObeyChild = newAttrFromBoolProperty "obey-child"

foreign import ccall unsafe "gtk_aspect_frame_new"
  gtk_aspect_frame_new :: ((Ptr CChar) -> (CFloat -> (CFloat -> (CFloat -> (CInt -> (IO (Ptr Widget)))))))

foreign import ccall safe "gtk_aspect_frame_set"
  gtk_aspect_frame_set :: ((Ptr AspectFrame) -> (CFloat -> (CFloat -> (CFloat -> (CInt -> (IO ()))))))
