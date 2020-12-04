
{-# LINE 2 "./Graphics/UI/Gtk/Layout/Expander.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget Expander
--
-- Author : Duncan Coutts
--
-- Created: 24 April 2004
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
-- A container which can hide its child
--
-- * Module available since Gtk+ version 2.4
--
module Graphics.UI.Gtk.Layout.Expander (
-- * Detail
--
-- | A 'Expander' allows the user to hide or show its child by clicking on an
-- expander triangle similar to the triangles used in a 'TreeView'.
--
-- Normally you use an expander as you would use any other descendant of
-- 'Bin'; you create the child widget and use
-- 'Graphics.UI.Gtk.Abstract.Container.containerAdd' to add it to the
-- expander. When the expander is toggled, it will take care of showing and
-- hiding the child automatically.

-- * Class Hierarchy
-- |
-- @
-- | 'GObject'
-- | +----'Object'
-- | +----'Widget'
-- | +----'Container'
-- | +----'Bin'
-- | +----Expander
-- @


-- * Types
  Expander,
  ExpanderClass,
  castToExpander, gTypeExpander,
  toExpander,

-- * Constructors
  expanderNew,
  expanderNewWithMnemonic,

-- * Methods
  expanderSetExpanded,
  expanderGetExpanded,
  expanderSetSpacing,
  expanderGetSpacing,
  expanderSetLabel,
  expanderGetLabel,
  expanderSetUseUnderline,
  expanderGetUseUnderline,
  expanderSetUseMarkup,
  expanderGetUseMarkup,
  expanderSetLabelWidget,
  expanderGetLabelWidget,

-- * Attributes
  expanderExpanded,
  expanderLabel,
  expanderUseUnderline,
  expanderUseMarkup,
  expanderSpacing,
  expanderLabelWidget,

  expanderLabelFill,


-- * Signals
  onActivate,
  afterActivate,

  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object
import Graphics.UI.Gtk.Types
{-# LINE 103 "./Graphics/UI/Gtk/Layout/Expander.chs" #-}
import Graphics.UI.Gtk.Signals


{-# LINE 106 "./Graphics/UI/Gtk/Layout/Expander.chs" #-}


--------------------
-- Constructors

-- | Creates a new expander using the given string as the text of the label.
--
expanderNew :: GlibString string => string -> IO Expander
expanderNew label =
  makeNewObject mkExpander $
  liftM (castPtr :: Ptr Widget -> Ptr Expander) $
  withUTFString label $ \labelPtr ->
  gtk_expander_new
{-# LINE 119 "./Graphics/UI/Gtk/Layout/Expander.chs" #-}
    labelPtr

-- | Creates a new expander using @label@ as the text of the label. If
-- characters in @label@ are preceded by an underscore, they are underlined. If
-- you need a literal underscore character in a label, use \'__\' (two
-- underscores). The first underlined character represents a keyboard
-- accelerator called a mnemonic. Pressing Alt and that key activates the
-- button.
--
expanderNewWithMnemonic :: GlibString string
 => string -- ^ @label@ - the text of the label with an underscore in
                -- front of the mnemonic character
 -> IO Expander
expanderNewWithMnemonic label =
  makeNewObject mkExpander $
  liftM (castPtr :: Ptr Widget -> Ptr Expander) $
  withUTFString label $ \labelPtr ->
  gtk_expander_new_with_mnemonic
{-# LINE 137 "./Graphics/UI/Gtk/Layout/Expander.chs" #-}
    labelPtr

--------------------
-- Methods

-- | Sets the state of the expander. Set to @True@, if you want the child
-- widget to be revealed, and @False@ if you want the child widget to be
-- hidden.
--
expanderSetExpanded :: Expander -> Bool -> IO ()
expanderSetExpanded self expanded =
  (\(Expander arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_expander_set_expanded argPtr1 arg2)
{-# LINE 149 "./Graphics/UI/Gtk/Layout/Expander.chs" #-}
    self
    (fromBool expanded)

-- | Queries a 'Expander' and returns its current state. Returns @True@ if the
-- child widget is revealed.
--
-- See 'expanderSetExpanded'.
--
expanderGetExpanded :: Expander -> IO Bool
expanderGetExpanded self =
  liftM toBool $
  (\(Expander arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_expander_get_expanded argPtr1)
{-# LINE 161 "./Graphics/UI/Gtk/Layout/Expander.chs" #-}
    self

-- | Sets the spacing field of @expander@, which is the number of pixels to
-- place between expander and the child.
--
expanderSetSpacing :: Expander -> Int -> IO ()
expanderSetSpacing self spacing =
  (\(Expander arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_expander_set_spacing argPtr1 arg2)
{-# LINE 169 "./Graphics/UI/Gtk/Layout/Expander.chs" #-}
    self
    (fromIntegral spacing)

-- | Gets the value set by 'expanderSetSpacing'.
--
expanderGetSpacing :: Expander
 -> IO Int -- ^ returns spacing between the expander and child.
expanderGetSpacing self =
  liftM fromIntegral $
  (\(Expander arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_expander_get_spacing argPtr1)
{-# LINE 179 "./Graphics/UI/Gtk/Layout/Expander.chs" #-}
    self

-- | Sets the text of the label of the expander to @label@.
--
-- This will also clear any previously set labels.
--
expanderSetLabel :: GlibString string => Expander -> string -> IO ()
expanderSetLabel self label =
  withUTFString label $ \labelPtr ->
  (\(Expander arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_expander_set_label argPtr1 arg2)
{-# LINE 189 "./Graphics/UI/Gtk/Layout/Expander.chs" #-}
    self
    labelPtr

-- | Fetches the text from the label of the expander, as set by
-- 'expanderSetLabel'.
--
expanderGetLabel :: GlibString string => Expander -> IO string
expanderGetLabel self =
  (\(Expander arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_expander_get_label argPtr1)
{-# LINE 198 "./Graphics/UI/Gtk/Layout/Expander.chs" #-}
    self
  >>= peekUTFString

-- | If true, an underline in the text of the expander label indicates the
-- next character should be used for the mnemonic accelerator key.
--
expanderSetUseUnderline :: Expander
 -> Bool -- ^ @useUnderline@ - @True@ if underlines in the text indicate
             -- mnemonics
 -> IO ()
expanderSetUseUnderline self useUnderline =
  (\(Expander arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_expander_set_use_underline argPtr1 arg2)
{-# LINE 210 "./Graphics/UI/Gtk/Layout/Expander.chs" #-}
    self
    (fromBool useUnderline)

-- | Returns whether an embedded underline in the expander label indicates a
-- mnemonic. See 'expanderSetUseUnderline'.
--
expanderGetUseUnderline :: Expander
 -> IO Bool -- ^ returns @True@ if an embedded underline in the expander
             -- label indicates the mnemonic accelerator keys.
expanderGetUseUnderline self =
  liftM toBool $
  (\(Expander arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_expander_get_use_underline argPtr1)
{-# LINE 222 "./Graphics/UI/Gtk/Layout/Expander.chs" #-}
    self

-- | Sets whether the text of the label contains markup in Pango's text markup
-- language. See 'Graphics.UI.Gtk.Display.Label.labelSetMarkup'.
--
expanderSetUseMarkup :: Expander
 -> Bool -- ^ @useMarkup@ - @True@ if the label's text should be parsed
             -- for markup
 -> IO ()
expanderSetUseMarkup self useMarkup =
  (\(Expander arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_expander_set_use_markup argPtr1 arg2)
{-# LINE 233 "./Graphics/UI/Gtk/Layout/Expander.chs" #-}
    self
    (fromBool useMarkup)

-- | Returns whether the label's text is interpreted as marked up with the
-- Pango text markup language. See 'expanderSetUseMarkup'.
--
expanderGetUseMarkup :: Expander -> IO Bool
expanderGetUseMarkup self =
  liftM toBool $
  (\(Expander arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_expander_get_use_markup argPtr1)
{-# LINE 243 "./Graphics/UI/Gtk/Layout/Expander.chs" #-}
    self

-- | Set the label widget for the expander. This is the widget that will
-- appear embedded alongside the expander arrow.
--
expanderSetLabelWidget :: WidgetClass labelWidget => Expander
 -> labelWidget -- ^ @labelWidget@ - the new label widget
 -> IO ()
expanderSetLabelWidget self labelWidget =
  (\(Expander arg1) (Widget arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_expander_set_label_widget argPtr1 argPtr2)
{-# LINE 253 "./Graphics/UI/Gtk/Layout/Expander.chs" #-}
    self
    (toWidget labelWidget)

-- | Retrieves the label widget for the frame. See 'expanderSetLabelWidget'.
--
expanderGetLabelWidget :: Expander
 -> IO Widget -- ^ returns the label widget
expanderGetLabelWidget self =
  makeNewObject mkWidget $
  (\(Expander arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_expander_get_label_widget argPtr1)
{-# LINE 263 "./Graphics/UI/Gtk/Layout/Expander.chs" #-}
    self

--------------------
-- Attributes

-- | Whether the expander has been opened to reveal the child widget.
--
-- Default value: @False@
--
expanderExpanded :: Attr Expander Bool
expanderExpanded = newAttr
  expanderGetExpanded
  expanderSetExpanded

-- | Text of the expander's label.
--
expanderLabel :: GlibString string => Attr Expander string
expanderLabel = newAttr
  expanderGetLabel
  expanderSetLabel

-- | If set, an underline in the text indicates the next character should be
-- used for the mnemonic accelerator key.
--
-- Default value: @False@
--
expanderUseUnderline :: Attr Expander Bool
expanderUseUnderline = newAttr
  expanderGetUseUnderline
  expanderSetUseUnderline

-- | The text of the label includes XML markup. See pango_parse_markup().
--
-- Default value: @False@
--
expanderUseMarkup :: Attr Expander Bool
expanderUseMarkup = newAttr
  expanderGetUseMarkup
  expanderSetUseMarkup

-- | Space to put between the label and the child.
--
-- Allowed values: >= 0
--
-- Default value: 0
--
expanderSpacing :: Attr Expander Int
expanderSpacing = newAttr
  expanderGetSpacing
  expanderSetSpacing

-- | A widget to display in place of the usual expander label.
--
expanderLabelWidget :: WidgetClass labelWidget => ReadWriteAttr Expander Widget labelWidget
expanderLabelWidget = newAttr
  expanderGetLabelWidget
  expanderSetLabelWidget


-- | Whether the label widget should fill all available horizontal space.
--
-- Default value: 'False'
--
expanderLabelFill :: Attr Expander Bool
expanderLabelFill = newAttrFromBoolProperty "label-fill"


--------------------
-- Signals

onActivate, afterActivate :: Expander
 -> IO ()
 -> IO (ConnectId Expander)
onActivate = connect_NONE__NONE "activate" False
afterActivate = connect_NONE__NONE "activate" True

foreign import ccall safe "gtk_expander_new"
  gtk_expander_new :: ((Ptr CChar) -> (IO (Ptr Widget)))

foreign import ccall safe "gtk_expander_new_with_mnemonic"
  gtk_expander_new_with_mnemonic :: ((Ptr CChar) -> (IO (Ptr Widget)))

foreign import ccall safe "gtk_expander_set_expanded"
  gtk_expander_set_expanded :: ((Ptr Expander) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_expander_get_expanded"
  gtk_expander_get_expanded :: ((Ptr Expander) -> (IO CInt))

foreign import ccall safe "gtk_expander_set_spacing"
  gtk_expander_set_spacing :: ((Ptr Expander) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_expander_get_spacing"
  gtk_expander_get_spacing :: ((Ptr Expander) -> (IO CInt))

foreign import ccall safe "gtk_expander_set_label"
  gtk_expander_set_label :: ((Ptr Expander) -> ((Ptr CChar) -> (IO ())))

foreign import ccall safe "gtk_expander_get_label"
  gtk_expander_get_label :: ((Ptr Expander) -> (IO (Ptr CChar)))

foreign import ccall safe "gtk_expander_set_use_underline"
  gtk_expander_set_use_underline :: ((Ptr Expander) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_expander_get_use_underline"
  gtk_expander_get_use_underline :: ((Ptr Expander) -> (IO CInt))

foreign import ccall safe "gtk_expander_set_use_markup"
  gtk_expander_set_use_markup :: ((Ptr Expander) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_expander_get_use_markup"
  gtk_expander_get_use_markup :: ((Ptr Expander) -> (IO CInt))

foreign import ccall safe "gtk_expander_set_label_widget"
  gtk_expander_set_label_widget :: ((Ptr Expander) -> ((Ptr Widget) -> (IO ())))

foreign import ccall safe "gtk_expander_get_label_widget"
  gtk_expander_get_label_widget :: ((Ptr Expander) -> (IO (Ptr Widget)))
