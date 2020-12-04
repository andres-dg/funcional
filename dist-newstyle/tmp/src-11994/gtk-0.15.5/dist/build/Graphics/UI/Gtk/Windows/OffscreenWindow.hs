
{-# LINE 2 "./Graphics/UI/Gtk/Windows/OffscreenWindow.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget OffscreenWindow
--
-- Author : Andy Stewart
--
-- Created: 25 Mar 2010
--
-- Copyright (C) 2010 Andy Stewart
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
-- A toplevel container widget used to manage offscreen rendering of child widgets.
--
-- * Module available since Gtk+ version 2.20
--
module Graphics.UI.Gtk.Windows.OffscreenWindow (
-- * Detail
-- | 'OffscreenWindow' is strictly intended to be used for obtaining snapshots of widgets that are not
-- part of a normal widget hierarchy. It differs from 'widgetGetSnapshot' in that the widget you
-- want to get a snapshot of need not be displayed on the user's screen as a part of a widget
-- hierarchy. However, since 'OffscreenWindow' is a toplevel widget you cannot obtain snapshots of a
-- full window with it since you cannot pack a toplevel widget in another toplevel.
--
-- The idea is to take a widget and manually set the state of it, add it to a 'OffscreenWindow' and
-- then retrieve the snapshot as a 'Pixmap' or 'Pixbuf'.
--
-- 'OffscreenWindow' derives from 'Window' only as an implementation detail. Applications should not
-- use any API specific to 'Window' to operate on this object. It should be treated as a 'Bin' that
-- has no parent widget.
--
-- When contained offscreen widgets are redrawn, 'OffscreenWindow' will emit a 'damageEvent' signal.

-- * Class Hierarchy
-- |
-- @
-- | 'GObject'
-- | +----'Object'
-- | +----'Widget'
-- | +----'Container'
-- | +----'Bin'
-- | +----Window
-- | +----'OffscreenWindow'
-- @


-- * Types
  OffscreenWindow,
  OffscreenWindowClass,
  castToOffscreenWindow, gTypeOffscreenWindow,
  toOffscreenWindow,

-- * Constructors
  offscreenWindowNew,

-- * Methods

  offscreenWindowGetPixmap,

  offscreenWindowGetPixbuf,

) where

import Control.Monad (liftM)

import System.Glib.FFI
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.UI.Gtk.Types
{-# LINE 82 "./Graphics/UI/Gtk/Windows/OffscreenWindow.chs" #-}


{-# LINE 84 "./Graphics/UI/Gtk/Windows/OffscreenWindow.chs" #-}


-- | Creates a toplevel container widget that is used to retrieve snapshots of widgets without showing
-- them on the screen. For widgets that are on the screen and part of a normal widget hierarchy,
-- 'widgetGetSnapshot' can be used instead.
--
-- * Available since Gtk+ version 2.20
--
offscreenWindowNew :: IO OffscreenWindow
offscreenWindowNew =
  makeNewObject mkOffscreenWindow $
  liftM (castPtr :: Ptr Widget -> Ptr OffscreenWindow) $
  gtk_offscreen_window_new
{-# LINE 97 "./Graphics/UI/Gtk/Windows/OffscreenWindow.chs" #-}


-- | Retrieves a snapshot of the contained widget in the form of a 'Pixmap'. If you need to keep this
-- around over window resizes then you should add a reference to it.
--
-- * Available since Gtk+ version 2.20
--
offscreenWindowGetPixmap :: OffscreenWindowClass self
                           => self -- ^ @offscreen@ the 'OffscreenWindow' contained widget.
                           -> IO (Maybe Pixmap) -- ^ returns A 'Pixmap' pointer to the offscreen pixmap, or 'Nothing'.
offscreenWindowGetPixmap offscreen =
  maybeNull (makeNewGObject mkPixmap) $
  (\(OffscreenWindow arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_offscreen_window_get_pixmap argPtr1)
{-# LINE 110 "./Graphics/UI/Gtk/Windows/OffscreenWindow.chs" #-}
     (toOffscreenWindow offscreen)


-- | Retrieves a snapshot of the contained widget in the form of a 'Pixbuf'.
--
-- * Available since Gtk+ version 2.20
--
offscreenWindowGetPixbuf :: OffscreenWindowClass self
                           => self -- ^ @offscreen@ the 'OffscreenWindow' contained widget.
                           -> IO (Maybe Pixbuf) -- ^ returns A 'Pixbuf' pointer to the offscreen pixbuf, or 'Nothing'.
offscreenWindowGetPixbuf offscreen =
  maybeNull (wrapNewGObject mkPixbuf) $
  (\(OffscreenWindow arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_offscreen_window_get_pixbuf argPtr1)
{-# LINE 123 "./Graphics/UI/Gtk/Windows/OffscreenWindow.chs" #-}
     (toOffscreenWindow offscreen)

foreign import ccall safe "gtk_offscreen_window_new"
  gtk_offscreen_window_new :: (IO (Ptr Widget))

foreign import ccall safe "gtk_offscreen_window_get_pixmap"
  gtk_offscreen_window_get_pixmap :: ((Ptr OffscreenWindow) -> (IO (Ptr Pixmap)))

foreign import ccall safe "gtk_offscreen_window_get_pixbuf"
  gtk_offscreen_window_get_pixbuf :: ((Ptr OffscreenWindow) -> (IO (Ptr Pixbuf)))
