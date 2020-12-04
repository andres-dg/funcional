
{-# LINE 2 "./Graphics/UI/Gtk/Abstract/ButtonBox.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget ButtonBox
--
-- Author : Matthew Walton
--
-- Created: 28 April 2004
--
-- Copyright (C) 2004-2005 Matthew Walton
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
-- Base class for 'HButtonBox' and 'VButtonBox'
--
module Graphics.UI.Gtk.Abstract.ButtonBox (
-- * Detail
--
-- | The primary purpose of this class is to keep track of the various
-- properties of 'HButtonBox' and 'VButtonBox' widgets.
--
-- 'buttonBoxGetChildSize' retrieves the minimum width and height for
-- widgets in a given button box. 'buttonBoxSetChildSize' allows those
-- properties to be changed.
--
-- The internal padding of buttons can be retrieved and changed per button
-- box using 'buttonBoxGetChildIpadding' and 'buttonBoxSetChildIpadding'
-- respectively.
--
-- 'buttonBoxGetSpacing' and 'buttonBoxSetSpacing' retrieve and change
-- default number of pixels between buttons, respectively.
--
-- 'buttonBoxGetLayout' and 'buttonBoxSetLayout' retrieve and alter the
-- method used to spread the buttons in a button box across the container,
-- respectively.
--
-- The main purpose of 'ButtonBox' is to make sure the children have all the
-- same size. Therefore it ignores the homogeneous property which it inherited
-- from 'Box', and always behaves as if homogeneous was @True@.

-- * Class Hierarchy
-- |
-- @
-- | 'GObject'
-- | +----'Object'
-- | +----'Widget'
-- | +----'Container'
-- | +----'Box'
-- | +----ButtonBox
-- | +----'HButtonBox'
-- | +----'VButtonBox'
-- @

-- * Types
  ButtonBox,
  ButtonBoxClass,
  castToButtonBox, gTypeButtonBox,
  toButtonBox,
  ButtonBoxStyle(..),

-- * Methods
  buttonBoxGetLayout,
  buttonBoxSetLayout,
  buttonBoxSetChildSecondary,

  buttonBoxGetChildSecondary,






-- * Attributes
  buttonBoxLayoutStyle,

-- * Child Attributes
  buttonBoxChildSecondary,



  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.Attributes
import Graphics.UI.Gtk.Types
{-# LINE 100 "./Graphics/UI/Gtk/Abstract/ButtonBox.chs" #-}
import Graphics.UI.Gtk.General.Enums (ButtonBoxStyle(..))
import Graphics.UI.Gtk.Abstract.ContainerChildProperties


{-# LINE 104 "./Graphics/UI/Gtk/Abstract/ButtonBox.chs" #-}

--------------------
-- Methods

-- | Retrieves the method being used to arrange the buttons in the button box.
--
buttonBoxGetLayout :: ButtonBoxClass self => self -> IO ButtonBoxStyle
buttonBoxGetLayout self =
  liftM (toEnum . fromIntegral) $
  (\(ButtonBox arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_button_box_get_layout argPtr1)
{-# LINE 114 "./Graphics/UI/Gtk/Abstract/ButtonBox.chs" #-}
    (toButtonBox self)


-- | Returns whether @child@ should appear in a secondary group of children.
--
-- * Available since Gtk+ version 2.4
--
buttonBoxGetChildSecondary :: (ButtonBoxClass self, WidgetClass child) => self
 -> child -- ^ @child@ - a child of the button box widget
 -> IO Bool -- ^ returns whether @child@ should appear in a secondary group of
            -- children.
buttonBoxGetChildSecondary self child =
  liftM toBool $
  (\(ButtonBox arg1) (Widget arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_button_box_get_child_secondary argPtr1 argPtr2)
{-# LINE 128 "./Graphics/UI/Gtk/Abstract/ButtonBox.chs" #-}
    (toButtonBox self)
    (toWidget child)
{-# LINE 157 "./Graphics/UI/Gtk/Abstract/ButtonBox.chs" #-}
-- | Changes the way buttons are arranged in their container.
--
buttonBoxSetLayout :: ButtonBoxClass self => self
 -> ButtonBoxStyle -- ^ @layoutStyle@ - the new layout style.
 -> IO ()
buttonBoxSetLayout self layoutStyle =
  (\(ButtonBox arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_button_box_set_layout argPtr1 arg2)
{-# LINE 164 "./Graphics/UI/Gtk/Abstract/ButtonBox.chs" #-}
    (toButtonBox self)
    ((fromIntegral . fromEnum) layoutStyle)

-- | Sets whether @child@ should appear in a secondary group of children. A
-- typical use of a secondary child is the help button in a dialog.
--
-- This group appears after the other children if the style is
-- 'ButtonboxStart', 'ButtonboxSpread' or 'ButtonboxEdge', and before the other
-- children if the style is 'ButtonboxEnd'. For horizontal button boxes, the
-- definition of before\/after depends on direction of the widget (see
-- 'widgetSetDirection'). If the style is 'ButtonboxStart' or 'ButtonboxEnd',
-- then the secondary children are aligned at the other end of the button box
-- from the main children. For the other styles, they appear immediately next
-- to the main children.
--
buttonBoxSetChildSecondary :: (ButtonBoxClass self, WidgetClass child) => self
 -> child -- ^ @child@ - a child of the button box widget
 -> Bool -- ^ @isSecondary@ - if @True@, the @child@ appears in a secondary
          -- group of the button box.
 -> IO ()
buttonBoxSetChildSecondary self child isSecondary =
  (\(ButtonBox arg1) (Widget arg2) arg3 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_button_box_set_child_secondary argPtr1 argPtr2 arg3)
{-# LINE 186 "./Graphics/UI/Gtk/Abstract/ButtonBox.chs" #-}
    (toButtonBox self)
    (toWidget child)
    (fromBool isSecondary)

--------------------
-- Attributes

-- | How to layout the buttons in the box. Possible values are default,
-- spread, edge, start and end.
--
-- Default value: 'ButtonboxDefaultStyle'
--
buttonBoxLayoutStyle :: ButtonBoxClass self => Attr self ButtonBoxStyle
buttonBoxLayoutStyle = newAttr
  buttonBoxGetLayout
  buttonBoxSetLayout

--------------------
-- Child Attributes

-- | If @True@, the child appears in a secondary group of children, suitable
-- for, e.g., help buttons.
--
-- Default value: @False@
--
buttonBoxChildSecondary :: (ButtonBoxClass self, WidgetClass child) => child -> Attr self Bool
buttonBoxChildSecondary = newAttrFromContainerChildBoolProperty "secondary"

foreign import ccall safe "gtk_button_box_get_layout"
  gtk_button_box_get_layout :: ((Ptr ButtonBox) -> (IO CInt))

foreign import ccall safe "gtk_button_box_get_child_secondary"
  gtk_button_box_get_child_secondary :: ((Ptr ButtonBox) -> ((Ptr Widget) -> (IO CInt)))

foreign import ccall safe "gtk_button_box_set_layout"
  gtk_button_box_set_layout :: ((Ptr ButtonBox) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_button_box_set_child_secondary"
  gtk_button_box_set_child_secondary :: ((Ptr ButtonBox) -> ((Ptr Widget) -> (CInt -> (IO ()))))