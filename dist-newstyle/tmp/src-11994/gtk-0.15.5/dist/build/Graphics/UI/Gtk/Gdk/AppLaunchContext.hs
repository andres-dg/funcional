
{-# LINE 2 "./Graphics/UI/Gtk/Gdk/AppLaunchContext.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget AppLaunchContext
--
-- Author : Andy Stewart
--
-- Created: 30 Mar 2010
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
--
module Graphics.UI.Gtk.Gdk.AppLaunchContext (

-- * Types
  AppLaunchContext,
  AppLaunchContextClass,
  castToAppLaunchContext,
  gTypeAppLaunchContext,
  toAppLaunchContext,

-- * Constructors
  appLaunchContextNew,

-- * Methods
  appLaunchContextSetDisplay,
  appLaunchContextSetScreen,
  appLaunchContextSetDesktop,
  appLaunchContextSetTimestamp,
  appLaunchContextSetIconName,

  appLaunchContextSetIcon,


  ) where

import System.Glib.FFI
import System.Glib.UTFString
import Graphics.UI.Gtk.Gdk.EventM (TimeStamp)
import Graphics.UI.Gtk.Types
{-# LINE 55 "./Graphics/UI/Gtk/Gdk/AppLaunchContext.chs" #-}

import System.GIO.Types (Icon (..), IconClass, toIcon)



{-# LINE 60 "./Graphics/UI/Gtk/Gdk/AppLaunchContext.chs" #-}


--------------------
-- Constructors

-- | Creates a new 'AppLaunchContext'.
appLaunchContextNew :: IO AppLaunchContext
appLaunchContextNew =
  wrapNewGObject mkAppLaunchContext $
  gdk_app_launch_context_new
{-# LINE 70 "./Graphics/UI/Gtk/Gdk/AppLaunchContext.chs" #-}

--------------------
-- Methods

-- | Sets the workspace on which applications will be launched when using this context when running under
-- a window manager that supports multiple workspaces, as described in the Extended Window Manager
-- Hints.
--
-- When the workspace is not specified or desktop is set to -1, it is up to the window manager to pick
-- one, typically it will be the current workspace.
appLaunchContextSetDesktop :: AppLaunchContext -> Int -> IO ()
appLaunchContextSetDesktop self desktop =
  (\(AppLaunchContext arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gdk_app_launch_context_set_desktop argPtr1 arg2)
{-# LINE 83 "./Graphics/UI/Gtk/Gdk/AppLaunchContext.chs" #-}
    self
    (fromIntegral desktop)

-- | Sets the display on which applications will be launched when using this context. See also
-- 'appLaunchContextSetScreen'.
appLaunchContextSetDisplay :: AppLaunchContext -> Display -> IO ()
appLaunchContextSetDisplay self display =
  (\(AppLaunchContext arg1) (Display arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gdk_app_launch_context_set_display argPtr1 argPtr2)
{-# LINE 91 "./Graphics/UI/Gtk/Gdk/AppLaunchContext.chs" #-}
    self
    display


-- | Sets the icon for applications that are launched with this context.
--
-- Window Managers can use this information when displaying startup notification.
appLaunchContextSetIcon :: IconClass icon => AppLaunchContext -> icon -> IO ()
appLaunchContextSetIcon self icon =
  (\(AppLaunchContext arg1) (Icon arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gdk_app_launch_context_set_icon argPtr1 argPtr2)
{-# LINE 101 "./Graphics/UI/Gtk/Gdk/AppLaunchContext.chs" #-}
    self
    (toIcon icon)


-- | Sets the icon for applications that are launched with this context. The @iconName@ will be
-- interpreted in the same way as the Icon field in desktop files. See also
-- 'appLaunchContextSetIcon'.
--
-- If both icon and @iconName@ are set, the @iconName@ takes priority. If neither icon or @iconName@ is
-- set, the icon is taken from either the file that is passed to launched application or from the
-- GAppInfo for the launched application itself.
appLaunchContextSetIconName :: GlibString string => AppLaunchContext -> string -> IO ()
appLaunchContextSetIconName self iconName =
  withUTFString iconName $ \iconNamePtr ->
  (\(AppLaunchContext arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gdk_app_launch_context_set_icon_name argPtr1 arg2)
{-# LINE 116 "./Graphics/UI/Gtk/Gdk/AppLaunchContext.chs" #-}
    self
    iconNamePtr

-- | Sets the screen on which applications will be launched when using this context. See also
-- 'appLaunchContextSetDisplay'.
--
-- If both screen and display are set, the screen takes priority. If neither screen or display are set,
-- the default screen and display are used.
appLaunchContextSetScreen :: AppLaunchContext -> Screen -> IO ()
appLaunchContextSetScreen self screen =
  (\(AppLaunchContext arg1) (Screen arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gdk_app_launch_context_set_screen argPtr1 argPtr2)
{-# LINE 127 "./Graphics/UI/Gtk/Gdk/AppLaunchContext.chs" #-}
    self
    screen

-- | Sets the timestamp of context. The timestamp should ideally be taken from the event that triggered
-- the launch.
--
-- Window managers can use this information to avoid moving the focus to the newly launched application
-- when the user is busy typing in another window. This is also known as 'focus stealing prevention'.
appLaunchContextSetTimestamp :: AppLaunchContext -> TimeStamp -> IO ()
appLaunchContextSetTimestamp self timestamp =
  (\(AppLaunchContext arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gdk_app_launch_context_set_timestamp argPtr1 arg2)
{-# LINE 138 "./Graphics/UI/Gtk/Gdk/AppLaunchContext.chs" #-}
    self
    (fromIntegral timestamp)

foreign import ccall safe "gdk_app_launch_context_new"
  gdk_app_launch_context_new :: (IO (Ptr AppLaunchContext))

foreign import ccall safe "gdk_app_launch_context_set_desktop"
  gdk_app_launch_context_set_desktop :: ((Ptr AppLaunchContext) -> (CInt -> (IO ())))

foreign import ccall safe "gdk_app_launch_context_set_display"
  gdk_app_launch_context_set_display :: ((Ptr AppLaunchContext) -> ((Ptr Display) -> (IO ())))

foreign import ccall safe "gdk_app_launch_context_set_icon"
  gdk_app_launch_context_set_icon :: ((Ptr AppLaunchContext) -> ((Ptr Icon) -> (IO ())))

foreign import ccall safe "gdk_app_launch_context_set_icon_name"
  gdk_app_launch_context_set_icon_name :: ((Ptr AppLaunchContext) -> ((Ptr CChar) -> (IO ())))

foreign import ccall safe "gdk_app_launch_context_set_screen"
  gdk_app_launch_context_set_screen :: ((Ptr AppLaunchContext) -> ((Ptr Screen) -> (IO ())))

foreign import ccall safe "gdk_app_launch_context_set_timestamp"
  gdk_app_launch_context_set_timestamp :: ((Ptr AppLaunchContext) -> (CUInt -> (IO ())))
