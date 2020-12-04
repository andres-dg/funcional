
{-# LINE 2 "./Graphics/UI/Gtk/Scrolling/ScrolledWindow.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget ScrolledWindow
--
-- Author : Axel Simon
--
-- Created: 23 May 2001
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
-- Adds scrollbars to its child widget
--
module Graphics.UI.Gtk.Scrolling.ScrolledWindow (
-- * Detail
--
-- | 'ScrolledWindow' is a 'Bin' subclass: it's a container the accepts a
-- single child widget. 'ScrolledWindow' adds scrollbars to the child widget
-- and optionally draws a beveled frame around the child widget.
--
-- The scrolled window can work in two ways. Some widgets have native
-- scrolling support; these widgets have \"slots\" for 'Adjustment' objects.
-- Widgets with native scroll support include 'TreeView', 'TextView', and
-- 'Layout'.
--
-- For widgets that lack native scrolling support, the 'Viewport' widget
-- acts as an adaptor class, implementing scrollability for child widgets that
-- lack their own scrolling capabilities. Use 'Viewport' to scroll child
-- widgets such as 'Table', 'Box', and so on.
--
-- If a widget has native scrolling abilities, it can be added to the
-- 'ScrolledWindow' with 'Graphics.UI.Gtk.Abstract.Container.containerAdd'.
-- If a widget does not, you must first add the widget to a 'Viewport', then
-- add the 'Viewport' to the scrolled window. The convenience function
-- 'scrolledWindowAddWithViewport' does exactly this, so you can ignore the
-- presence of the viewport.
--
-- The position of the scrollbars is controlled by the scroll adjustments.
-- See 'Adjustment' for the fields in an adjustment - for 'Scrollbar', used by
-- 'ScrolledWindow', the \"value\" field represents the position of the
-- scrollbar, which must be between the \"lower\" field and \"upper -
-- page_size.\" The \"page_size\" field represents the size of the visible
-- scrollable area. The \"step_increment\" and \"page_increment\" fields are
-- used when the user asks to step down (using the small stepper arrows) or
-- page down (using for example the PageDown key).
--
-- If a 'ScrolledWindow' doesn't behave quite as you would like, or doesn't
-- have exactly the right layout, it's very possible to set up your own
-- scrolling with 'Scrollbar' and for example a 'Table'.

-- * Class Hierarchy
-- |
-- @
-- | 'GObject'
-- | +----'Object'
-- | +----'Widget'
-- | +----'Container'
-- | +----'Bin'
-- | +----ScrolledWindow
-- @

-- * Types
  ScrolledWindow,
  ScrolledWindowClass,
  castToScrolledWindow, gTypeScrolledWindow,
  toScrolledWindow,

-- * Constructors
  scrolledWindowNew,

-- * Methods
  scrolledWindowGetHAdjustment,
  scrolledWindowGetVAdjustment,
  PolicyType(..),
  scrolledWindowSetPolicy,
  scrolledWindowGetPolicy,
  scrolledWindowAddWithViewport,
  CornerType(..),
  scrolledWindowSetPlacement,
  scrolledWindowGetPlacement,
  ShadowType(..),
  scrolledWindowSetShadowType,
  scrolledWindowGetShadowType,






  scrolledWindowSetHAdjustment,
  scrolledWindowSetVAdjustment,

  scrolledWindowGetHScrollbar,
  scrolledWindowGetVScrollbar,
{-# LINE 117 "./Graphics/UI/Gtk/Scrolling/ScrolledWindow.chs" #-}
-- * Attributes
  scrolledWindowHAdjustment,
  scrolledWindowVAdjustment,
  scrolledWindowHscrollbarPolicy,
  scrolledWindowVscrollbarPolicy,
  scrolledWindowWindowPlacement,
  scrolledWindowShadowType,




  scrolledWindowPlacement,



  ) where

import Control.Monad (liftM)
import Data.Maybe (fromMaybe)

import System.Glib.FFI
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.UI.Gtk.Types
{-# LINE 142 "./Graphics/UI/Gtk/Scrolling/ScrolledWindow.chs" #-}
import Graphics.UI.Gtk.General.Enums (PolicyType(..), CornerType(..), ShadowType(..))


{-# LINE 145 "./Graphics/UI/Gtk/Scrolling/ScrolledWindow.chs" #-}

--------------------
-- Constructors

-- | Creates a new scrolled window. The two arguments are the scrolled
-- window's adjustments; these will be shared with the scrollbars and the child
-- widget to keep the bars in sync with the child. Usually you want to pass
-- @Nothing@ for the adjustments, which will cause the scrolled window to
-- create them for you.
--
scrolledWindowNew ::
    Maybe Adjustment -- ^ @hadjustment@ - Horizontal adjustment.
 -> Maybe Adjustment -- ^ @vadjustment@ - Vertical adjustment.
 -> IO ScrolledWindow
scrolledWindowNew hadjustment vadjustment =
  makeNewObject mkScrolledWindow $
  liftM (castPtr :: Ptr Widget -> Ptr ScrolledWindow) $
  (\(Adjustment arg1) (Adjustment arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_scrolled_window_new argPtr1 argPtr2)
{-# LINE 163 "./Graphics/UI/Gtk/Scrolling/ScrolledWindow.chs" #-}
    (fromMaybe (Adjustment nullForeignPtr) hadjustment)
    (fromMaybe (Adjustment nullForeignPtr) vadjustment)

--------------------
-- Methods

-- | Returns the horizontal scrollbar's adjustment, used to connect the
-- horizontal scrollbar to the child widget's horizontal scroll functionality.
--
scrolledWindowGetHAdjustment :: ScrolledWindowClass self => self -> IO Adjustment
scrolledWindowGetHAdjustment self =
  makeNewObject mkAdjustment $
  (\(ScrolledWindow arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_scrolled_window_get_hadjustment argPtr1)
{-# LINE 176 "./Graphics/UI/Gtk/Scrolling/ScrolledWindow.chs" #-}
    (toScrolledWindow self)

-- | Returns the vertical scrollbar's adjustment, used to connect the vertical
-- scrollbar to the child widget's vertical scroll functionality.
--
scrolledWindowGetVAdjustment :: ScrolledWindowClass self => self -> IO Adjustment
scrolledWindowGetVAdjustment self =
  makeNewObject mkAdjustment $
  (\(ScrolledWindow arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_scrolled_window_get_vadjustment argPtr1)
{-# LINE 185 "./Graphics/UI/Gtk/Scrolling/ScrolledWindow.chs" #-}
    (toScrolledWindow self)

-- | Sets the scrollbar policy for the horizontal and vertical scrollbars. The
-- policy determines when the scrollbar should appear; it is a value from the
-- 'PolicyType' enumeration. If 'PolicyAlways', the scrollbar is always
-- present; if 'PolicyNever', the scrollbar is never present; if
-- 'PolicyAutomatic', the scrollbar is present only if needed (that is, if the
-- slider part of the bar would be smaller than the trough - the display is
-- larger than the page size).
--
scrolledWindowSetPolicy :: ScrolledWindowClass self => self
 -> PolicyType -- ^ @hscrollbarPolicy@ - Policy for horizontal bar.
 -> PolicyType -- ^ @vscrollbarPolicy@ - Policy for vertical bar.
 -> IO ()
scrolledWindowSetPolicy self hscrollbarPolicy vscrollbarPolicy =
  (\(ScrolledWindow arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_scrolled_window_set_policy argPtr1 arg2 arg3)
{-# LINE 201 "./Graphics/UI/Gtk/Scrolling/ScrolledWindow.chs" #-}
    (toScrolledWindow self)
    ((fromIntegral . fromEnum) hscrollbarPolicy)
    ((fromIntegral . fromEnum) vscrollbarPolicy)

-- | Retrieves the current policy values for the horizontal and vertical
-- scrollbars. See 'scrolledWindowSetPolicy'.
--
scrolledWindowGetPolicy :: ScrolledWindowClass self => self
 -> IO (PolicyType, PolicyType) -- ^ @(hscrollbarPolicy, vscrollbarPolicy)@
scrolledWindowGetPolicy self =
  alloca $ \hPolPtr ->
  alloca $ \vPolPtr -> do
  (\(ScrolledWindow arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_scrolled_window_get_policy argPtr1 arg2 arg3)
{-# LINE 214 "./Graphics/UI/Gtk/Scrolling/ScrolledWindow.chs" #-}
    (toScrolledWindow self)
    hPolPtr vPolPtr
  hPol <- liftM (toEnum.fromIntegral) $ peek hPolPtr
  vPol <- liftM (toEnum.fromIntegral) $ peek vPolPtr
  return (hPol, vPol)

-- | Used to add children without native scrolling capabilities. This is
-- simply a convenience function; it is equivalent to adding the unscrollable
-- child to a viewport, then adding the viewport to the scrolled window. If a
-- child has native scrolling, use
-- 'Graphics.UI.Gtk.Abstract.Container.containerAdd' instead of this function.
--
-- The viewport scrolls the child by moving its 'DrawWindow', and takes the
-- size of the child to be the size of its toplevel 'DrawWindow'. This will be
-- very wrong for most widgets that support native scrolling; for example, if
-- you add a widget such as 'TreeView' with a viewport, the whole widget will
-- scroll, including the column headings. Thus, widgets with native scrolling
-- support should not be used with the 'Viewport' proxy.
--
scrolledWindowAddWithViewport :: (ScrolledWindowClass self, WidgetClass child) => self
 -> child -- ^ @child@ - Widget you want to scroll.
 -> IO ()
scrolledWindowAddWithViewport self child =
  (\(ScrolledWindow arg1) (Widget arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_scrolled_window_add_with_viewport argPtr1 argPtr2)
{-# LINE 238 "./Graphics/UI/Gtk/Scrolling/ScrolledWindow.chs" #-}
    (toScrolledWindow self)
    (toWidget child)

-- | Determines the location of the child widget with respect to the
-- scrollbars. The default is 'CornerTopLeft', meaning the child is in the top
-- left, with the scrollbars underneath and to the right. Other values in
-- 'CornerType' are 'CornerTopRight', 'CornerBottomLeft', and
-- 'CornerBottomRight'.
--
scrolledWindowSetPlacement :: ScrolledWindowClass self => self
 -> CornerType -- ^ @windowPlacement@ - Position of the child window.
 -> IO ()
scrolledWindowSetPlacement self windowPlacement =
  (\(ScrolledWindow arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_scrolled_window_set_placement argPtr1 arg2)
{-# LINE 252 "./Graphics/UI/Gtk/Scrolling/ScrolledWindow.chs" #-}
    (toScrolledWindow self)
    ((fromIntegral . fromEnum) windowPlacement)

-- | Gets the placement of the scrollbars for the scrolled window. See
-- 'scrolledWindowSetPlacement'.
--
scrolledWindowGetPlacement :: ScrolledWindowClass self => self -> IO CornerType
scrolledWindowGetPlacement self =
  liftM (toEnum . fromIntegral) $
  (\(ScrolledWindow arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_scrolled_window_get_placement argPtr1)
{-# LINE 262 "./Graphics/UI/Gtk/Scrolling/ScrolledWindow.chs" #-}
    (toScrolledWindow self)

-- | Changes the type of shadow drawn around the contents of @scrolledWindow@.
--
scrolledWindowSetShadowType :: ScrolledWindowClass self => self -> ShadowType -> IO ()
scrolledWindowSetShadowType self type_ =
  (\(ScrolledWindow arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_scrolled_window_set_shadow_type argPtr1 arg2)
{-# LINE 269 "./Graphics/UI/Gtk/Scrolling/ScrolledWindow.chs" #-}
    (toScrolledWindow self)
    ((fromIntegral . fromEnum) type_)

-- | Gets the shadow type of the scrolled window. See
-- 'scrolledWindowSetShadowType'.
--
scrolledWindowGetShadowType :: ScrolledWindowClass self => self -> IO ShadowType
scrolledWindowGetShadowType self =
  liftM (toEnum . fromIntegral) $
  (\(ScrolledWindow arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_scrolled_window_get_shadow_type argPtr1)
{-# LINE 279 "./Graphics/UI/Gtk/Scrolling/ScrolledWindow.chs" #-}
    (toScrolledWindow self)
{-# LINE 317 "./Graphics/UI/Gtk/Scrolling/ScrolledWindow.chs" #-}
-- | Sets the 'Adjustment' for the horizontal scrollbar.
--
scrolledWindowSetHAdjustment :: ScrolledWindowClass self => self -> Adjustment -> IO ()
scrolledWindowSetHAdjustment self hadjustment =
  (\(ScrolledWindow arg1) (Adjustment arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_scrolled_window_set_hadjustment argPtr1 argPtr2)
{-# LINE 322 "./Graphics/UI/Gtk/Scrolling/ScrolledWindow.chs" #-}
    (toScrolledWindow self)
    hadjustment

-- | Sets the 'Adjustment' for the vertical scrollbar.
--
scrolledWindowSetVAdjustment :: ScrolledWindowClass self => self
 -> Adjustment -- ^ @vadjustment@ - Vertical scroll adjustment.
 -> IO ()
scrolledWindowSetVAdjustment self vadjustment =
  (\(ScrolledWindow arg1) (Adjustment arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_scrolled_window_set_vadjustment argPtr1 argPtr2)
{-# LINE 332 "./Graphics/UI/Gtk/Scrolling/ScrolledWindow.chs" #-}
    (toScrolledWindow self)
    vadjustment


-- | Returns the horizontal scrollbar of @scrolledWindow@.
--
-- * Available since Gtk+ version 2.8
--
scrolledWindowGetHScrollbar :: ScrolledWindowClass self => self
 -> IO (Maybe HScrollbar) -- ^ returns the horizontal scrollbar of the scrolled
                          -- window, or @Nothing@ if it does not have one.
scrolledWindowGetHScrollbar self =
  maybeNull (makeNewObject mkHScrollbar) $
  liftM (castPtr :: Ptr Widget -> Ptr HScrollbar) $
  (\(ScrolledWindow arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_scrolled_window_get_hscrollbar argPtr1)
{-# LINE 347 "./Graphics/UI/Gtk/Scrolling/ScrolledWindow.chs" #-}
    (toScrolledWindow self)

-- | Returns the vertical scrollbar of @scrolledWindow@.
--
-- * Available since Gtk+ version 2.8
--
scrolledWindowGetVScrollbar :: ScrolledWindowClass self => self
 -> IO (Maybe VScrollbar) -- ^ returns the vertical scrollbar of the scrolled
                          -- window, or @Nothing@ if it does not have one.
scrolledWindowGetVScrollbar self =
  maybeNull (makeNewObject mkVScrollbar) $
  liftM (castPtr :: Ptr Widget -> Ptr VScrollbar) $
  (\(ScrolledWindow arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_scrolled_window_get_vscrollbar argPtr1)
{-# LINE 360 "./Graphics/UI/Gtk/Scrolling/ScrolledWindow.chs" #-}
    (toScrolledWindow self)
{-# LINE 408 "./Graphics/UI/Gtk/Scrolling/ScrolledWindow.chs" #-}
--------------------
-- Attributes

-- | The 'Adjustment' for the horizontal position.
--
scrolledWindowHAdjustment :: ScrolledWindowClass self => Attr self Adjustment
scrolledWindowHAdjustment = newAttr
  scrolledWindowGetHAdjustment
  scrolledWindowSetHAdjustment

-- | The 'Adjustment' for the vertical position.
--
scrolledWindowVAdjustment :: ScrolledWindowClass self => Attr self Adjustment
scrolledWindowVAdjustment = newAttr
  scrolledWindowGetVAdjustment
  scrolledWindowSetVAdjustment

-- | When the horizontal scrollbar is displayed.
--
-- Default value: 'PolicyAlways'
--
scrolledWindowHscrollbarPolicy :: ScrolledWindowClass self => Attr self PolicyType
scrolledWindowHscrollbarPolicy = newAttrFromEnumProperty "hscrollbar-policy"
  gtk_policy_type_get_type
{-# LINE 432 "./Graphics/UI/Gtk/Scrolling/ScrolledWindow.chs" #-}

-- | When the vertical scrollbar is displayed.
--
-- Default value: 'PolicyAlways'
--
scrolledWindowVscrollbarPolicy :: ScrolledWindowClass self => Attr self PolicyType
scrolledWindowVscrollbarPolicy = newAttrFromEnumProperty "vscrollbar-policy"
  gtk_policy_type_get_type
{-# LINE 440 "./Graphics/UI/Gtk/Scrolling/ScrolledWindow.chs" #-}

-- | Where the contents are located with respect to the scrollbars.
--
-- Default value: 'CornerTopLeft'
--
scrolledWindowWindowPlacement :: ScrolledWindowClass self => Attr self CornerType
scrolledWindowWindowPlacement = newAttrFromEnumProperty "window-placement"
  gtk_corner_type_get_type
{-# LINE 448 "./Graphics/UI/Gtk/Scrolling/ScrolledWindow.chs" #-}

-- | Style of bevel around the contents.
--
-- Default value: 'ShadowNone'
--
scrolledWindowShadowType :: ScrolledWindowClass self => Attr self ShadowType
scrolledWindowShadowType = newAttr
  scrolledWindowGetShadowType
  scrolledWindowSetShadowType
{-# LINE 478 "./Graphics/UI/Gtk/Scrolling/ScrolledWindow.chs" #-}
-- | \'placement\' property. See 'scrolledWindowGetPlacement' and
-- 'scrolledWindowSetPlacement'
--
scrolledWindowPlacement :: ScrolledWindowClass self => Attr self CornerType
scrolledWindowPlacement = newAttr
  scrolledWindowGetPlacement
  scrolledWindowSetPlacement

foreign import ccall unsafe "gtk_scrolled_window_new"
  gtk_scrolled_window_new :: ((Ptr Adjustment) -> ((Ptr Adjustment) -> (IO (Ptr Widget))))

foreign import ccall unsafe "gtk_scrolled_window_get_hadjustment"
  gtk_scrolled_window_get_hadjustment :: ((Ptr ScrolledWindow) -> (IO (Ptr Adjustment)))

foreign import ccall unsafe "gtk_scrolled_window_get_vadjustment"
  gtk_scrolled_window_get_vadjustment :: ((Ptr ScrolledWindow) -> (IO (Ptr Adjustment)))

foreign import ccall safe "gtk_scrolled_window_set_policy"
  gtk_scrolled_window_set_policy :: ((Ptr ScrolledWindow) -> (CInt -> (CInt -> (IO ()))))

foreign import ccall unsafe "gtk_scrolled_window_get_policy"
  gtk_scrolled_window_get_policy :: ((Ptr ScrolledWindow) -> ((Ptr CInt) -> ((Ptr CInt) -> (IO ()))))

foreign import ccall safe "gtk_scrolled_window_add_with_viewport"
  gtk_scrolled_window_add_with_viewport :: ((Ptr ScrolledWindow) -> ((Ptr Widget) -> (IO ())))

foreign import ccall safe "gtk_scrolled_window_set_placement"
  gtk_scrolled_window_set_placement :: ((Ptr ScrolledWindow) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_scrolled_window_get_placement"
  gtk_scrolled_window_get_placement :: ((Ptr ScrolledWindow) -> (IO CInt))

foreign import ccall safe "gtk_scrolled_window_set_shadow_type"
  gtk_scrolled_window_set_shadow_type :: ((Ptr ScrolledWindow) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_scrolled_window_get_shadow_type"
  gtk_scrolled_window_get_shadow_type :: ((Ptr ScrolledWindow) -> (IO CInt))

foreign import ccall safe "gtk_scrolled_window_set_hadjustment"
  gtk_scrolled_window_set_hadjustment :: ((Ptr ScrolledWindow) -> ((Ptr Adjustment) -> (IO ())))

foreign import ccall safe "gtk_scrolled_window_set_vadjustment"
  gtk_scrolled_window_set_vadjustment :: ((Ptr ScrolledWindow) -> ((Ptr Adjustment) -> (IO ())))

foreign import ccall safe "gtk_scrolled_window_get_hscrollbar"
  gtk_scrolled_window_get_hscrollbar :: ((Ptr ScrolledWindow) -> (IO (Ptr Widget)))

foreign import ccall safe "gtk_scrolled_window_get_vscrollbar"
  gtk_scrolled_window_get_vscrollbar :: ((Ptr ScrolledWindow) -> (IO (Ptr Widget)))

foreign import ccall unsafe "gtk_policy_type_get_type"
  gtk_policy_type_get_type :: CULong

foreign import ccall unsafe "gtk_corner_type_get_type"
  gtk_corner_type_get_type :: CULong
