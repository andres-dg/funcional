
{-# LINE 2 "./Graphics/UI/Gtk/Abstract/Paned.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget Paned
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
-- Base class for widgets with two adjustable panes
--
module Graphics.UI.Gtk.Abstract.Paned (
-- * Detail
--
-- | 'Paned' is the base class for widgets with two panes, arranged either
-- horizontally ('HPaned') or vertically ('VPaned'). Child widgets are added to
-- the panes of the widget with 'panedPack1' and 'panedPack2'. The division
-- beween the two children is set by default from the size requests of the
-- children, but it can be adjusted by the user.
--
-- A paned widget draws a separator between the two child widgets and a
-- small handle that the user can drag to adjust the division. It does not draw
-- any relief around the children or around the separator. (The space in which
-- the separator is called the gutter.) Often, it is useful to put each child
-- inside a 'Frame' with the shadow type set to
-- 'Graphics.UI.Gtk.General.Enums.ShadowIn' so that the gutter appears as a
-- ridge.
--
-- Each child has two options that can be set, @resize@ and @shrink@. If
-- @resize@ is true, then when the 'Paned' is resized, that child will expand
-- or shrink along with the paned widget. If @shrink@ is true, then when that
-- child can be made smaller than its requisition by the user. Setting @shrink@
-- to @False@ allows the application to set a minimum size. If @resize@ is
-- false for both children, then this is treated as if @resize@ is true for
-- both children.
--
-- The application can set the position of the slider as if it were set by
-- the user, by calling 'panedSetPosition'.

-- * Class Hierarchy
-- |
-- @
-- | 'GObject'
-- | +----'Object'
-- | +----'Widget'
-- | +----'Container'
-- | +----Paned
-- | +----'HPaned'
-- | +----'VPaned'
-- @

-- * Types
  Paned,
  PanedClass,
  castToPaned, gTypePaned,
  toPaned,

-- * Methods
  panedAdd1,
  panedAdd2,
  panedPack1,
  panedPack2,
  panedSetPosition,
  panedGetPosition,

  panedGetChild1,
  panedGetChild2,


  panedGetHandleWindow,


-- * Attributes
  panedPosition,
  panedPositionSet,

  panedMinPosition,
  panedMaxPosition,


-- * Child Attributes

  panedChildResize,
  panedChildShrink,



-- * Deprecated Signals

  onCycleChildFocus,
  afterCycleChildFocus,
  onToggleHandleFocus,
  afterToggleHandleFocus,
  onMoveHandle,
  afterMoveHandle,
  onCycleHandleFocus,
  afterCycleHandleFocus,
  onAcceptPosition,
  afterAcceptPosition,
  onCancelPosition,
  afterCancelPosition,

  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.UI.Gtk.Types
{-# LINE 128 "./Graphics/UI/Gtk/Abstract/Paned.chs" #-}

import Graphics.UI.Gtk.Signals
{-# LINE 130 "./Graphics/UI/Gtk/Abstract/Paned.chs" #-}
import Graphics.UI.Gtk.General.Enums (ScrollType)

import Graphics.UI.Gtk.Abstract.ContainerChildProperties


{-# LINE 135 "./Graphics/UI/Gtk/Abstract/Paned.chs" #-}

--------------------
-- Methods

-- | Adds a child to the top or left pane with default parameters. This is
-- equivalent to @'panedPack1' paned child False True@.
--
panedAdd1 :: (PanedClass self, WidgetClass child) => self
 -> child -- ^ @child@ - the child to add
 -> IO ()
panedAdd1 self child =
  (\(Paned arg1) (Widget arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_paned_add1 argPtr1 argPtr2)
{-# LINE 147 "./Graphics/UI/Gtk/Abstract/Paned.chs" #-}
    (toPaned self)
    (toWidget child)

-- | Adds a child to the bottom or right pane with default parameters. This is
-- equivalent to @'panedPack2' paned child True True@.
--
panedAdd2 :: (PanedClass self, WidgetClass child) => self
 -> child -- ^ @child@ - the child to add
 -> IO ()
panedAdd2 self child =
  (\(Paned arg1) (Widget arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_paned_add2 argPtr1 argPtr2)
{-# LINE 158 "./Graphics/UI/Gtk/Abstract/Paned.chs" #-}
    (toPaned self)
    (toWidget child)

-- | Adds a child to the top or left pane.
--
panedPack1 :: (PanedClass self, WidgetClass child) => self
 -> child -- ^ @child@ - the child to add
 -> Bool -- ^ @resize@ - should this child expand when the paned widget is
          -- resized.
 -> Bool -- ^ @shrink@ - can this child be made smaller than its requsition.
 -> IO ()
panedPack1 self child resize shrink =
  (\(Paned arg1) (Widget arg2) arg3 arg4 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_paned_pack1 argPtr1 argPtr2 arg3 arg4)
{-# LINE 171 "./Graphics/UI/Gtk/Abstract/Paned.chs" #-}
    (toPaned self)
    (toWidget child)
    (fromBool resize)
    (fromBool shrink)

-- | Adds a child to the bottom or right pane.
--
panedPack2 :: (PanedClass self, WidgetClass child) => self
 -> child -- ^ @child@ - the child to add
 -> Bool -- ^ @resize@ - should this child expand when the paned widget is
          -- resized.
 -> Bool -- ^ @shrink@ - can this child be made smaller than its requsition.
 -> IO ()
panedPack2 self child resize shrink =
  (\(Paned arg1) (Widget arg2) arg3 arg4 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_paned_pack2 argPtr1 argPtr2 arg3 arg4)
{-# LINE 186 "./Graphics/UI/Gtk/Abstract/Paned.chs" #-}
    (toPaned self)
    (toWidget child)
    (fromBool resize)
    (fromBool shrink)

-- | Sets the position of the divider between the two panes.
--
panedSetPosition :: PanedClass self => self
 -> Int -- ^ @position@ - pixel position of divider, a negative value means
          -- that the position is unset.
 -> IO ()
panedSetPosition self position =
  (\(Paned arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_paned_set_position argPtr1 arg2)
{-# LINE 199 "./Graphics/UI/Gtk/Abstract/Paned.chs" #-}
    (toPaned self)
    (fromIntegral position)

-- | Obtains the position of the divider between the two panes.
--
panedGetPosition :: PanedClass self => self
 -> IO Int -- ^ returns position of the divider
panedGetPosition self =
  liftM fromIntegral $
  (\(Paned arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_paned_get_position argPtr1)
{-# LINE 209 "./Graphics/UI/Gtk/Abstract/Paned.chs" #-}
    (toPaned self)


-- | Obtains the first child of the paned widget.
--
-- * Available since Gtk+ version 2.4
--
panedGetChild1 :: PanedClass self => self
 -> IO (Maybe Widget) -- ^ returns first child, or @Nothing@ if it is not set.
panedGetChild1 self =
  maybeNull (makeNewObject mkWidget) $
  (\(Paned arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_paned_get_child1 argPtr1)
{-# LINE 221 "./Graphics/UI/Gtk/Abstract/Paned.chs" #-}
    (toPaned self)

-- | Obtains the second child of the paned widget.
--
-- * Available since Gtk+ version 2.4
--
panedGetChild2 :: PanedClass self => self
 -> IO (Maybe Widget) -- ^ returns second child, or @Nothing@ if it is not
                      -- set.
panedGetChild2 self =
  maybeNull (makeNewObject mkWidget) $
  (\(Paned arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_paned_get_child2 argPtr1)
{-# LINE 233 "./Graphics/UI/Gtk/Abstract/Paned.chs" #-}
    (toPaned self)



-- | Returns the 'Window' of the handle. This function is useful when handling button or motion events
-- because it enables the callback to distinguish between the window of the paned, a child and the
-- handle.
panedGetHandleWindow :: PanedClass self => self
                     -> IO DrawWindow
panedGetHandleWindow self =
    makeNewGObject mkDrawWindow $
    (\(Paned arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_paned_get_handle_window argPtr1)
{-# LINE 245 "./Graphics/UI/Gtk/Abstract/Paned.chs" #-}
      (toPaned self)


--------------------
-- Attributes

-- | Position of paned separator in pixels (0 means all the way to the
-- left\/top).
--
-- Allowed values: >= 0
--
-- Default value: 0
--
panedPosition :: PanedClass self => Attr self Int
panedPosition = newAttr
  panedGetPosition
  panedSetPosition

-- | @True@ if the Position property should be used.
--
-- Default value: @False@
--
panedPositionSet :: PanedClass self => Attr self Bool
panedPositionSet = newAttrFromBoolProperty "position-set"


-- | The smallest possible value for the position property. This property is
-- derived from the size and shrinkability of the widget's children.
--
-- Allowed values: >= 0
--
-- Default value: 0
--
panedMinPosition :: PanedClass self => ReadAttr self Int
panedMinPosition = readAttrFromIntProperty "min-position"

-- | The largest possible value for the position property. This property is
-- derived from the size and shrinkability of the widget's children.
--
-- Allowed values: >= 0
--
-- Default value: 2147483647
--
panedMaxPosition :: PanedClass self => ReadAttr self Int
panedMaxPosition = readAttrFromIntProperty "max-position"


--------------------
-- Child Attributes


-- | The \"resize\" child property determines whether the child expands and
-- shrinks along with the paned widget.
--
-- Default value: @True@
--
panedChildResize :: (PanedClass self, WidgetClass child) => child -> Attr self Bool
panedChildResize = newAttrFromContainerChildBoolProperty "resize"

-- | The \"shrink\" child property determines whether the child can be made
-- smaller than its requisition.
--
-- Default value: @True@
--
panedChildShrink :: (PanedClass self, WidgetClass child) => child -> Attr self Bool
panedChildShrink = newAttrFromContainerChildBoolProperty "shrink"


--------------------
-- Deprecated Signals


-- |
--
onCycleChildFocus, afterCycleChildFocus :: PanedClass self => self
 -> (Bool -> IO Bool)
 -> IO (ConnectId self)
onCycleChildFocus = connect_BOOL__BOOL "cycle_child_focus" False
afterCycleChildFocus = connect_BOOL__BOOL "cycle_child_focus" True

-- |
--
onToggleHandleFocus, afterToggleHandleFocus :: PanedClass self => self
 -> IO Bool
 -> IO (ConnectId self)
onToggleHandleFocus = connect_NONE__BOOL "toggle_handle_focus" False
afterToggleHandleFocus = connect_NONE__BOOL "toggle_handle_focus" True

-- |
--
onMoveHandle, afterMoveHandle :: PanedClass self => self
 -> (ScrollType -> IO Bool)
 -> IO (ConnectId self)
onMoveHandle = connect_ENUM__BOOL "move_handle" False
afterMoveHandle = connect_ENUM__BOOL "move_handle" True

-- |
--
onCycleHandleFocus, afterCycleHandleFocus :: PanedClass self => self
 -> (Bool -> IO Bool)
 -> IO (ConnectId self)
onCycleHandleFocus = connect_BOOL__BOOL "cycle_handle_focus" False
afterCycleHandleFocus = connect_BOOL__BOOL "cycle_handle_focus" True

-- |
--
onAcceptPosition, afterAcceptPosition :: PanedClass self => self
 -> IO Bool
 -> IO (ConnectId self)
onAcceptPosition = connect_NONE__BOOL "accept_position" False
afterAcceptPosition = connect_NONE__BOOL "accept_position" True

-- |
--
onCancelPosition, afterCancelPosition :: PanedClass self => self
 -> IO Bool
 -> IO (ConnectId self)
onCancelPosition = connect_NONE__BOOL "cancel_position" False
afterCancelPosition = connect_NONE__BOOL "cancel_position" True

foreign import ccall safe "gtk_paned_add1"
  gtk_paned_add1 :: ((Ptr Paned) -> ((Ptr Widget) -> (IO ())))

foreign import ccall safe "gtk_paned_add2"
  gtk_paned_add2 :: ((Ptr Paned) -> ((Ptr Widget) -> (IO ())))

foreign import ccall safe "gtk_paned_pack1"
  gtk_paned_pack1 :: ((Ptr Paned) -> ((Ptr Widget) -> (CInt -> (CInt -> (IO ())))))

foreign import ccall safe "gtk_paned_pack2"
  gtk_paned_pack2 :: ((Ptr Paned) -> ((Ptr Widget) -> (CInt -> (CInt -> (IO ())))))

foreign import ccall safe "gtk_paned_set_position"
  gtk_paned_set_position :: ((Ptr Paned) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_paned_get_position"
  gtk_paned_get_position :: ((Ptr Paned) -> (IO CInt))

foreign import ccall unsafe "gtk_paned_get_child1"
  gtk_paned_get_child1 :: ((Ptr Paned) -> (IO (Ptr Widget)))

foreign import ccall unsafe "gtk_paned_get_child2"
  gtk_paned_get_child2 :: ((Ptr Paned) -> (IO (Ptr Widget)))

foreign import ccall safe "gtk_paned_get_handle_window"
  gtk_paned_get_handle_window :: ((Ptr Paned) -> (IO (Ptr DrawWindow)))
