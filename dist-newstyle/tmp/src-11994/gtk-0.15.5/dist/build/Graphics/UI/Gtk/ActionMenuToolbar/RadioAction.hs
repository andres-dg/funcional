
{-# LINE 2 "./Graphics/UI/Gtk/ActionMenuToolbar/RadioAction.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget RadioAction
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
-- TODO
-- I don't know what the elemnt type of the group GSList is for
-- radioActionSetGroup / radioActionGetGroup
--
-- Also, the signals clash with those from other modules
--
-- |
-- Maintainer : gtk2hs-users@lists.sourceforge.net
-- Stability : provisional
-- Portability : portable (depends on GHC)
--
-- An action of which only one in a group can be active
--
-- * Module available since Gtk+ version 2.4
--
module Graphics.UI.Gtk.ActionMenuToolbar.RadioAction (
-- * Detail
--
-- | A 'RadioAction' is similar to 'RadioMenuItem'. A number of radio actions
-- can be linked together so that only one may be active at any one time.

-- * Class Hierarchy
-- |
-- @
-- | 'GObject'
-- | +----'Action'
-- | +----'ToggleAction'
-- | +----RadioAction
-- @


-- * Types
  RadioAction,
  RadioActionClass,
  castToRadioAction, gTypeRadioAction,
  toRadioAction,

-- * Constructors
  radioActionNew,

-- * Methods
  radioActionGetGroup,
  radioActionSetGroup,
  radioActionGetCurrentValue,

-- * Attributes
  radioActionValueAttr,
  radioActionGroup,

  radioActionCurrentValue,


-- * Signals
  radioActionChanged,


-- * Deprecated
  onRadioActionChanged,
  afterRadioActionChanged,


  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.GList
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Types
{-# LINE 92 "./Graphics/UI/Gtk/ActionMenuToolbar/RadioAction.chs" #-}
import Graphics.UI.Gtk.Signals
{-# LINE 93 "./Graphics/UI/Gtk/ActionMenuToolbar/RadioAction.chs" #-}
import Graphics.UI.Gtk.General.StockItems


{-# LINE 96 "./Graphics/UI/Gtk/ActionMenuToolbar/RadioAction.chs" #-}


--------------------
-- Constructors

-- | Creates a new 'RadioAction' object. To add the action to a 'ActionGroup'
-- and set the accelerator for the action, call
-- 'Graphics.UI.Gtk.ActionMenuToolbar.ActionGroup.actionGroupAddActionWithAccel'.
--
radioActionNew :: GlibString string
 => string -- ^ @name@ - A unique name for the action
 -> string -- ^ @label@ - The label displayed in menu items and on
                   -- buttons
 -> Maybe string -- ^ @tooltip@ - A tooltip for this action
 -> Maybe StockId -- ^ @stockId@ - The stock icon to display in widgets
                   -- representing this action
 -> Int -- ^ @value@ - The value which 'radioActionGetCurrentValue'
                   -- should return if this action is selected.
 -> IO RadioAction
radioActionNew name label tooltip stockId value =
  wrapNewGObject mkRadioAction $
  maybeWith withUTFString stockId $ \stockIdPtr ->
  maybeWith withUTFString tooltip $ \tooltipPtr ->
  withUTFString label $ \labelPtr ->
  withUTFString name $ \namePtr ->
  gtk_radio_action_new
{-# LINE 122 "./Graphics/UI/Gtk/ActionMenuToolbar/RadioAction.chs" #-}
    namePtr
    labelPtr
    tooltipPtr
    stockIdPtr
    (fromIntegral value)

--------------------
-- Methods

-- | Returns the list representing the radio group for this object
--
radioActionGetGroup :: RadioActionClass self => self
 -> IO [RadioAction] -- ^ returns the members of the radio group
radioActionGetGroup self =
  (\(RadioAction arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_radio_action_get_group argPtr1)
{-# LINE 137 "./Graphics/UI/Gtk/ActionMenuToolbar/RadioAction.chs" #-}
    (toRadioAction self)
  >>= readGSList
  >>= mapM (\elemPtr -> makeNewGObject mkRadioAction (return elemPtr))

-- | Sets the radio group for the radio action object.
--
radioActionSetGroup :: (RadioActionClass self, RadioActionClass groupMember) => self
 -> groupMember -- ^ @groupMember@ - an existing member of the radio group
 -> IO ()
radioActionSetGroup self group = do
  groupPtr <- (\(RadioAction arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_radio_action_get_group argPtr1) (toRadioAction group)
  (\(RadioAction arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_radio_action_set_group argPtr1 arg2)
{-# LINE 149 "./Graphics/UI/Gtk/ActionMenuToolbar/RadioAction.chs" #-}
    (toRadioAction self)
    groupPtr

-- | Obtains the value property of the currently active member of the group to
-- which the action belongs.
--
radioActionGetCurrentValue :: RadioActionClass self => self
 -> IO Int -- ^ returns the value of the currently active group member
radioActionGetCurrentValue self =
  liftM fromIntegral $
  (\(RadioAction arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_radio_action_get_current_value argPtr1)
{-# LINE 160 "./Graphics/UI/Gtk/ActionMenuToolbar/RadioAction.chs" #-}
    (toRadioAction self)

--------------------
-- Attributes

-- %hash d:1bcf
-- | The value is an arbitrary integer which can be used as a convenient way
-- to determine which action in the group is currently active in an ::activate
-- or ::changed signal handler. See 'radioActionGetCurrentValue' and
-- 'RadioActionEntry' for convenient ways to get and set
-- this property.
--
-- Default value: 0
--
radioActionValueAttr :: RadioActionClass self => Attr self Int
radioActionValueAttr = newAttrFromIntProperty "value"

-- %hash c:a380
-- | Sets a new group for a radio action.
--
radioActionGroup :: RadioActionClass self => ReadWriteAttr self [RadioAction] RadioAction
radioActionGroup = newAttr
  radioActionGetGroup
  radioActionSetGroup


-- %hash c:4cec d:1710
-- | The value property of the currently active member of the group to which
-- this action belongs.
--
-- Default value: 0
--
-- * Available since Gtk+ version 2.10
--
radioActionCurrentValue :: RadioActionClass self => Attr self Int
radioActionCurrentValue = newAttrFromIntProperty "current-value"


-- | The 'radioActionChanged' signal is emitted on every member of a radio group when the
-- active member is changed. The signal gets emitted after the 'actionActivated' signals for the
-- previous and current active members.
--
radioActionChanged :: RadioActionClass self => Signal self (RadioAction -> IO ())
radioActionChanged = Signal (connect_OBJECT__NONE "changed")

--------------------
-- Deprecated Signals


-- | The changed signal is emitted on every member of a radio group when the
-- active member is changed. The signal gets emitted after the activate
-- signals for the previous and current active members.
--
onRadioActionChanged, afterRadioActionChanged :: RadioActionClass self => self
 -> (RadioAction -> IO ())
 -> IO (ConnectId self)
onRadioActionChanged = connect_OBJECT__NONE "changed" False
afterRadioActionChanged = connect_OBJECT__NONE "changed" True

foreign import ccall safe "gtk_radio_action_new"
  gtk_radio_action_new :: ((Ptr CChar) -> ((Ptr CChar) -> ((Ptr CChar) -> ((Ptr CChar) -> (CInt -> (IO (Ptr RadioAction)))))))

foreign import ccall unsafe "gtk_radio_action_get_group"
  gtk_radio_action_get_group :: ((Ptr RadioAction) -> (IO (Ptr ())))

foreign import ccall safe "gtk_radio_action_set_group"
  gtk_radio_action_set_group :: ((Ptr RadioAction) -> ((Ptr ()) -> (IO ())))

foreign import ccall safe "gtk_radio_action_get_current_value"
  gtk_radio_action_get_current_value :: ((Ptr RadioAction) -> (IO CInt))
