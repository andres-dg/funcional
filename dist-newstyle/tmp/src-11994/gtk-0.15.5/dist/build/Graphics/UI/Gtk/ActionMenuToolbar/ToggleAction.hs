
{-# LINE 2 "./Graphics/UI/Gtk/ActionMenuToolbar/ToggleAction.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget ToggleAction
--
-- Author : Duncan Coutts
--
-- Created: 6 April 2005
--
-- Copyright (C) 2005 Duncan Coutts
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
-- An action which can be toggled between two states
--
-- * Module available since Gtk+ version 2.4
--
module Graphics.UI.Gtk.ActionMenuToolbar.ToggleAction (
-- * Detail
--
-- | A 'ToggleAction' corresponds roughly to a 'CheckMenuItem'. It has an
-- \"active\" state specifying whether the action has been checked or not.

-- * Class Hierarchy
-- |
-- @
-- | 'GObject'
-- | +----'Action'
-- | +----ToggleAction
-- | +----'RadioAction'
-- @


-- * Types
  ToggleAction,
  ToggleActionClass,
  castToToggleAction, gTypeToggleAction,
  toToggleAction,

-- * Constructors
  toggleActionNew,

-- * Methods
  toggleActionToggled,
  toggleActionSetActive,
  toggleActionGetActive,
  toggleActionSetDrawAsRadio,
  toggleActionGetDrawAsRadio,

-- * Attributes
  toggleActionDrawAsRadio,

  toggleActionActive,


-- * Signals
  actionToggled,

-- * Deprecated

  onActionToggled,
  afterActionToggled,



  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Types
{-# LINE 87 "./Graphics/UI/Gtk/ActionMenuToolbar/ToggleAction.chs" #-}
import Graphics.UI.Gtk.Signals
{-# LINE 88 "./Graphics/UI/Gtk/ActionMenuToolbar/ToggleAction.chs" #-}
import Graphics.UI.Gtk.General.StockItems


{-# LINE 91 "./Graphics/UI/Gtk/ActionMenuToolbar/ToggleAction.chs" #-}


--------------------
-- Constructors

-- | Creates a new 'ToggleAction' object. To add the action to a 'ActionGroup'
-- and set the accelerator for the action, call
-- 'Graphics.UI.Gtk.ActionMenuToolbar.ActionGroup.actionGroupAddActionWithAccel'.
--
toggleActionNew :: GlibString string
 => string -- ^ @name@ - A unique name for the action
 -> string -- ^ @label@ - The label displayed in menu items and on
                    -- buttons
 -> Maybe string -- ^ @tooltip@ - A tooltip for the action
 -> Maybe StockId -- ^ @stockId@ - The stock icon to display in widgets
                    -- representing the action
 -> IO ToggleAction
toggleActionNew name label tooltip stockId =
  wrapNewGObject mkToggleAction $
  maybeWith withUTFString stockId $ \stockIdPtr ->
  maybeWith withUTFString tooltip $ \tooltipPtr ->
  withUTFString label $ \labelPtr ->
  withUTFString name $ \namePtr ->
  gtk_toggle_action_new
{-# LINE 115 "./Graphics/UI/Gtk/ActionMenuToolbar/ToggleAction.chs" #-}
    namePtr
    labelPtr
    tooltipPtr
    stockIdPtr

--------------------
-- Methods

-- | Emits the \"toggled\" signal on the toggle action.
--
toggleActionToggled :: ToggleActionClass self => self -> IO ()
toggleActionToggled self =
  (\(ToggleAction arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_toggle_action_toggled argPtr1)
{-# LINE 128 "./Graphics/UI/Gtk/ActionMenuToolbar/ToggleAction.chs" #-}
    (toToggleAction self)

-- | Sets the checked state on the toggle action.
--
toggleActionSetActive :: ToggleActionClass self => self
 -> Bool -- ^ @isActive@ - whether the action should be checked or not
 -> IO ()
toggleActionSetActive self isActive =
  (\(ToggleAction arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_toggle_action_set_active argPtr1 arg2)
{-# LINE 137 "./Graphics/UI/Gtk/ActionMenuToolbar/ToggleAction.chs" #-}
    (toToggleAction self)
    (fromBool isActive)

-- | Returns the checked state of the toggle action.
--
toggleActionGetActive :: ToggleActionClass self => self -> IO Bool
toggleActionGetActive self =
  liftM toBool $
  (\(ToggleAction arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_toggle_action_get_active argPtr1)
{-# LINE 146 "./Graphics/UI/Gtk/ActionMenuToolbar/ToggleAction.chs" #-}
    (toToggleAction self)

-- | Sets whether the action should have proxies like a radio action.
--
toggleActionSetDrawAsRadio :: ToggleActionClass self => self -> Bool -> IO ()
toggleActionSetDrawAsRadio self drawAsRadio =
  (\(ToggleAction arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_toggle_action_set_draw_as_radio argPtr1 arg2)
{-# LINE 153 "./Graphics/UI/Gtk/ActionMenuToolbar/ToggleAction.chs" #-}
    (toToggleAction self)
    (fromBool drawAsRadio)

-- | Returns whether the action should have proxies like a radio action.
--
toggleActionGetDrawAsRadio :: ToggleActionClass self => self -> IO Bool
toggleActionGetDrawAsRadio self =
  liftM toBool $
  (\(ToggleAction arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_toggle_action_get_draw_as_radio argPtr1)
{-# LINE 162 "./Graphics/UI/Gtk/ActionMenuToolbar/ToggleAction.chs" #-}
    (toToggleAction self)

--------------------
-- Attributes

-- | Whether the proxies for this action look like radio action proxies.
--
-- Default value: @False@
--
toggleActionDrawAsRadio :: ToggleActionClass self => Attr self Bool
toggleActionDrawAsRadio = newAttr
  toggleActionGetDrawAsRadio
  toggleActionSetDrawAsRadio


-- %hash c:cd0e d:4024
-- | If the toggle action should be active in or not.
--
-- Default value: @False@
--
-- * Available since Gtk+ version 2.10
--
toggleActionActive :: ToggleActionClass self => Attr self Bool
toggleActionActive = newAttrFromBoolProperty "active"


--------------------
-- Signals

-- %hash c:3829 d:af3f
-- |
--
actionToggled :: ToggleActionClass self => Signal self (IO ())
actionToggled = Signal (connect_NONE__NONE "toggled")

--------------------
-- Deprecated Signals


-- %hash c:9cc4
onActionToggled :: ToggleActionClass self => self
 -> IO ()
 -> IO (ConnectId self)
onActionToggled = connect_NONE__NONE "toggled" False
{-# DEPRECATED onActionToggled "instead of 'onActionToggled obj' use 'on obj actionToggled'" #-}

-- %hash c:61e3
afterActionToggled :: ToggleActionClass self => self
 -> IO ()
 -> IO (ConnectId self)
afterActionToggled = connect_NONE__NONE "toggled" True
{-# DEPRECATED afterActionToggled "instead of 'afterActionToggled obj' use 'after obj actionToggled'" #-}

foreign import ccall safe "gtk_toggle_action_new"
  gtk_toggle_action_new :: ((Ptr CChar) -> ((Ptr CChar) -> ((Ptr CChar) -> ((Ptr CChar) -> (IO (Ptr ToggleAction))))))

foreign import ccall safe "gtk_toggle_action_toggled"
  gtk_toggle_action_toggled :: ((Ptr ToggleAction) -> (IO ()))

foreign import ccall safe "gtk_toggle_action_set_active"
  gtk_toggle_action_set_active :: ((Ptr ToggleAction) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_toggle_action_get_active"
  gtk_toggle_action_get_active :: ((Ptr ToggleAction) -> (IO CInt))

foreign import ccall safe "gtk_toggle_action_set_draw_as_radio"
  gtk_toggle_action_set_draw_as_radio :: ((Ptr ToggleAction) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_toggle_action_get_draw_as_radio"
  gtk_toggle_action_get_draw_as_radio :: ((Ptr ToggleAction) -> (IO CInt))
