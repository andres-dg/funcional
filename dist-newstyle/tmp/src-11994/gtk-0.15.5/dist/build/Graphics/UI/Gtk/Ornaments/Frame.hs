
{-# LINE 2 "./Graphics/UI/Gtk/Ornaments/Frame.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget Frame
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
-- A bin with a decorative frame and optional label
--
module Graphics.UI.Gtk.Ornaments.Frame (
-- * Detail
--
-- | The frame widget is a Bin that surrounds its child with a decorative
-- frame and an optional label. If present, the label is drawn in a gap in the
-- top side of the frame. The position of the label can be controlled with
-- 'frameSetLabelAlign'.

-- * Class Hierarchy
-- |
-- @
-- | 'GObject'
-- | +----'Object'
-- | +----'Widget'
-- | +----'Container'
-- | +----'Bin'
-- | +----Frame
-- | +----'AspectFrame'
-- @

-- * Types
  Frame,
  FrameClass,
  castToFrame, gTypeFrame,
  toFrame,

-- * Constructors
  frameNew,

-- * Methods
  frameSetLabel,
  frameGetLabel,
  frameSetLabelWidget,
  frameGetLabelWidget,
  frameSetLabelAlign,
  frameGetLabelAlign,
  ShadowType(..),
  frameSetShadowType,
  frameGetShadowType,

-- * Attributes
  frameLabel,
  frameLabelXAlign,
  frameLabelYAlign,
  frameShadowType,
  frameLabelWidget,
  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.UI.Gtk.Types
{-# LINE 84 "./Graphics/UI/Gtk/Ornaments/Frame.chs" #-}
import Graphics.UI.Gtk.General.Enums (ShadowType(..))


{-# LINE 87 "./Graphics/UI/Gtk/Ornaments/Frame.chs" #-}

--------------------
-- Constructors

-- | Creates a new 'Frame' without a label.
--
-- * A label can later be set by calling 'frameSetLabel'.
--
frameNew :: IO Frame
frameNew =
  makeNewObject mkFrame $
  liftM (castPtr :: Ptr Widget -> Ptr Frame) $
  gtk_frame_new
{-# LINE 100 "./Graphics/UI/Gtk/Ornaments/Frame.chs" #-}
    nullPtr

--------------------
-- Methods

-- | Sets the text of the label.
--
frameSetLabel :: (FrameClass self, GlibString string) => self
 -> string -- ^ @label@ - the text to use as the label of the frame
 -> IO ()
frameSetLabel self label =
  withUTFString label $ \labelPtr ->
  (\(Frame arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_frame_set_label argPtr1 arg2)
{-# LINE 113 "./Graphics/UI/Gtk/Ornaments/Frame.chs" #-}
    (toFrame self)
    labelPtr

-- | Sets the label widget for the frame. This is the widget that will appear
-- embedded in the top edge of the frame as a title.
--
frameSetLabelWidget :: (FrameClass self, WidgetClass labelWidget) => self
 -> labelWidget
 -> IO ()
frameSetLabelWidget self labelWidget =
  (\(Frame arg1) (Widget arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_frame_set_label_widget argPtr1 argPtr2)
{-# LINE 124 "./Graphics/UI/Gtk/Ornaments/Frame.chs" #-}
    (toFrame self)
    (toWidget labelWidget)

-- | Retrieves the label widget for the frame. See 'frameSetLabelWidget'.
--
frameGetLabelWidget :: FrameClass self => self
 -> IO (Maybe Widget) -- ^ returns the label widget, or @Nothing@ if there is
                      -- none.
frameGetLabelWidget self =
  maybeNull (makeNewObject mkWidget) $
  (\(Frame arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_frame_get_label_widget argPtr1)
{-# LINE 135 "./Graphics/UI/Gtk/Ornaments/Frame.chs" #-}
    (toFrame self)

-- | Sets the alignment of the frame widget's label. The default values for a
-- newly created frame are 0.0 and 0.5.
--
frameSetLabelAlign :: FrameClass self => self
 -> Float -- ^ @xalign@ - The position of the label along the top edge of the
          -- widget. A value of 0.0 represents left alignment; 1.0 represents
          -- right alignment.
 -> Float -- ^ @yalign@ - The y alignment of the label. A value of 0.0 aligns
          -- under the frame; 1.0 aligns above the frame.
 -> IO ()
frameSetLabelAlign self xalign yalign =
  (\(Frame arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_frame_set_label_align argPtr1 arg2 arg3)
{-# LINE 149 "./Graphics/UI/Gtk/Ornaments/Frame.chs" #-}
    (toFrame self)
    (realToFrac xalign)
    (realToFrac yalign)

-- | Retrieves the X and Y alignment of the frame's label. See
-- 'frameSetLabelAlign'.
--
frameGetLabelAlign :: FrameClass self => self
 -> IO (Float, Float) -- ^ @(xalign, yalign)@
frameGetLabelAlign self =
  alloca $ \xalignPtr ->
  alloca $ \yalignPtr -> do
  (\(Frame arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_frame_get_label_align argPtr1 arg2 arg3)
{-# LINE 162 "./Graphics/UI/Gtk/Ornaments/Frame.chs" #-}
    (toFrame self)
    xalignPtr
    yalignPtr
  xalign <- peek xalignPtr
  yalign <- peek yalignPtr
  return (realToFrac xalign, realToFrac yalign)

-- | Sets the shadow type of the frame.
--
frameSetShadowType :: FrameClass self => self -> ShadowType -> IO ()
frameSetShadowType self type_ =
  (\(Frame arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_frame_set_shadow_type argPtr1 arg2)
{-# LINE 174 "./Graphics/UI/Gtk/Ornaments/Frame.chs" #-}
    (toFrame self)
    ((fromIntegral . fromEnum) type_)

-- | Retrieves the shadow type of the frame. See 'frameSetShadowType'.
--
frameGetShadowType :: FrameClass self => self -> IO ShadowType
frameGetShadowType self =
  liftM (toEnum . fromIntegral) $
  (\(Frame arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_frame_get_shadow_type argPtr1)
{-# LINE 183 "./Graphics/UI/Gtk/Ornaments/Frame.chs" #-}
    (toFrame self)

-- | If the frame's label widget is a 'Label', returns the text in the label
-- widget.
--
frameGetLabel :: (FrameClass self, GlibString string) => self
 -> IO string -- ^ returns the text in the label, or if there was no label
              -- widget or the lable widget was not a 'Label' then an
              -- exception is thrown
frameGetLabel self =
  throwIfNull "frameGetLabel: the title of the frame was not a Label widget."
  ((\(Frame arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_frame_get_label argPtr1)
{-# LINE 195 "./Graphics/UI/Gtk/Ornaments/Frame.chs" #-}
    (toFrame self))
  >>= peekUTFString

--------------------
-- Attributes

-- | Text of the frame's label.
--
frameLabel :: (FrameClass self, GlibString string) => Attr self string
frameLabel = newAttr
  frameGetLabel
  frameSetLabel

-- | The horizontal alignment of the label.
--
-- Allowed values: [0,1]
--
-- Default value: 0.5
--
frameLabelXAlign :: FrameClass self => Attr self Float
frameLabelXAlign = newAttrFromFloatProperty "label-xalign"

-- | The vertical alignment of the label.
--
-- Allowed values: [0,1]
--
-- Default value: 0.5
--
frameLabelYAlign :: FrameClass self => Attr self Float
frameLabelYAlign = newAttrFromFloatProperty "label-yalign"

-- | Appearance of the frame border.
--
-- Default value: 'ShadowEtchedIn'
--
frameShadowType :: FrameClass self => Attr self ShadowType
frameShadowType = newAttr
  frameGetShadowType
  frameSetShadowType

-- | A widget to display in place of the usual frame label.
--
frameLabelWidget :: (FrameClass self, WidgetClass labelWidget) => ReadWriteAttr self (Maybe Widget) labelWidget
frameLabelWidget = newAttr
  frameGetLabelWidget
  frameSetLabelWidget

foreign import ccall unsafe "gtk_frame_new"
  gtk_frame_new :: ((Ptr CChar) -> (IO (Ptr Widget)))

foreign import ccall safe "gtk_frame_set_label"
  gtk_frame_set_label :: ((Ptr Frame) -> ((Ptr CChar) -> (IO ())))

foreign import ccall safe "gtk_frame_set_label_widget"
  gtk_frame_set_label_widget :: ((Ptr Frame) -> ((Ptr Widget) -> (IO ())))

foreign import ccall safe "gtk_frame_get_label_widget"
  gtk_frame_get_label_widget :: ((Ptr Frame) -> (IO (Ptr Widget)))

foreign import ccall safe "gtk_frame_set_label_align"
  gtk_frame_set_label_align :: ((Ptr Frame) -> (CFloat -> (CFloat -> (IO ()))))

foreign import ccall unsafe "gtk_frame_get_label_align"
  gtk_frame_get_label_align :: ((Ptr Frame) -> ((Ptr CFloat) -> ((Ptr CFloat) -> (IO ()))))

foreign import ccall safe "gtk_frame_set_shadow_type"
  gtk_frame_set_shadow_type :: ((Ptr Frame) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_frame_get_shadow_type"
  gtk_frame_get_shadow_type :: ((Ptr Frame) -> (IO CInt))

foreign import ccall unsafe "gtk_frame_get_label"
  gtk_frame_get_label :: ((Ptr Frame) -> (IO (Ptr CChar)))
