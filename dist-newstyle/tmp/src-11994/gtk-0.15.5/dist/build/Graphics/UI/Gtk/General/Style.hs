
{-# LINE 2 "./Graphics/UI/Gtk/General/Style.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Styles
--
-- Author : Axel Simon
--
-- Created: 13 February 2003
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
-- TODO
--
-- It seems sensible to treat Styles as read only. The only way to modify
-- a style should be for the programmer to apply the RcStyle patches directly
-- to the widget.
--
-- Bind the draw... functions, they might be useful.
--
-- |
-- Maintainer : gtk2hs-users@lists.sourceforge.net
-- Stability : provisional
-- Portability : portable (depends on GHC)
--
-- Customization of widgets.
--
module Graphics.UI.Gtk.General.Style (
-- * Description
--
-- | Styles are attached to widgets and determine how particular parts are
-- drawn and with what color. Thus they are should be seen as mandatory when
-- one implements a new custom widgets via 'DrawingArea'. Although the
-- parameterized drawing function don't have to be used, it is strongly
-- advisable (and more robust) to make use of the predefined graphics contexts
-- for the different states of a widget (retrieved by
-- 'Graphics.UI.Gtk.Abstract.Widget.widgetGetState').
--

-- * Types
  Style,
  StyleClass,
  castToStyle, gTypeStyle,
  toStyle,

-- * Methods
  styleGetForeground,
  styleGetBackground,
  styleGetLight,
  styleGetMiddle,
  styleGetDark,
  styleGetText,
  styleGetBase,
  styleGetAntiAliasing,


  stylePaintFlatBox,
  stylePaintLayout,


  ) where


{-# LINE 72 "./Graphics/UI/Gtk/General/Style.chs" #-}


import System.Glib.FFI
import Graphics.Rendering.Pango.Types
{-# LINE 76 "./Graphics/UI/Gtk/General/Style.chs" #-}
import Graphics.Rendering.Pango.BasicTypes
import Graphics.UI.Gtk.General.Structs (Rectangle)
import Graphics.UI.Gtk.General.Enums (StateType, ShadowType)


import Graphics.UI.Gtk.Types
{-# LINE 82 "./Graphics/UI/Gtk/General/Style.chs" #-}
import Graphics.UI.Gtk.General.Structs (styleGetForeground,
                         styleGetBackground,
                         styleGetLight,
                         styleGetMiddle,
                         styleGetDark,
                         styleGetText,
                         styleGetBase,
                         styleGetAntiAliasing)


stylePaintFlatBox :: WidgetClass widget
                  => Style
                  -> DrawWindow
                  -> StateType
                  -> ShadowType
                  -> Rectangle
                  -> widget
                  -> String
                  -> Int -> Int -> Int -> Int
                  -> IO ()
stylePaintFlatBox style window stateType shadowType
                  clipRect widget detail x y width height =
  with clipRect $ \rectPtr ->
  withCString detail $ \detailPtr ->
  (\(Style arg1) (DrawWindow arg2) arg3 arg4 arg5 (Widget arg6) arg7 arg8 arg9 arg10 arg11 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->withForeignPtr arg6 $ \argPtr6 ->gtk_paint_flat_box argPtr1 argPtr2 arg3 arg4 arg5 argPtr6 arg7 arg8 arg9 arg10 arg11)
{-# LINE 107 "./Graphics/UI/Gtk/General/Style.chs" #-}
    style
    window
    ((fromIntegral.fromEnum) stateType)
    ((fromIntegral.fromEnum) shadowType)
    (castPtr rectPtr)
    (toWidget widget)
    detailPtr
    (fromIntegral x) (fromIntegral y)
    (fromIntegral width) (fromIntegral height)

stylePaintLayout :: WidgetClass widget
                 => Style
                 -> DrawWindow
                 -> StateType
                 -> Bool
                 -> Rectangle
                 -> widget
                 -> String
                 -> Int -> Int
                 -> PangoLayout
                 -> IO ()
stylePaintLayout style window stateType useText
                  clipRect widget detail x y (PangoLayout _ layout) =
  with clipRect $ \rectPtr ->
  withCString detail $ \detailPtr ->
  (\(Style arg1) (DrawWindow arg2) arg3 arg4 arg5 (Widget arg6) arg7 arg8 arg9 (PangoLayoutRaw arg10) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->withForeignPtr arg6 $ \argPtr6 ->withForeignPtr arg10 $ \argPtr10 ->gtk_paint_layout argPtr1 argPtr2 arg3 arg4 arg5 argPtr6 arg7 arg8 arg9 argPtr10)
{-# LINE 133 "./Graphics/UI/Gtk/General/Style.chs" #-}
    style
    window
    ((fromIntegral.fromEnum) stateType)
    (fromBool useText)
    (castPtr rectPtr)
    (toWidget widget)
    detailPtr
    (fromIntegral x) (fromIntegral y)
    layout

foreign import ccall safe "gtk_paint_flat_box"
  gtk_paint_flat_box :: ((Ptr Style) -> ((Ptr DrawWindow) -> (CInt -> (CInt -> ((Ptr ()) -> ((Ptr Widget) -> ((Ptr CChar) -> (CInt -> (CInt -> (CInt -> (CInt -> (IO ()))))))))))))

foreign import ccall safe "gtk_paint_layout"
  gtk_paint_layout :: ((Ptr Style) -> ((Ptr DrawWindow) -> (CInt -> (CInt -> ((Ptr ()) -> ((Ptr Widget) -> ((Ptr CChar) -> (CInt -> (CInt -> ((Ptr PangoLayoutRaw) -> (IO ())))))))))))
