
{-# LINE 2 "./Graphics/UI/Gtk/Abstract/Object.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Object
--
-- Author : Axel Simon
--
-- Created: 9 April 2001
--
-- Copyright (C) 2001-2005 Axel Simon
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
-- The base class of the Gtk+ type hierarchy.
--
-- * Each widget is a represented as a purely abstract data type. It can only
-- be accessed through and the special access functions that are defined
-- in each widget file.
--
module Graphics.UI.Gtk.Abstract.Object (
-- * Detail
--
-- | 'Object' is the base class for all widgets, and for a few non-widget
-- objects such as 'Adjustment'. 'Object' predates 'GObject'; non-widgets that
-- derive from 'Object' rather than 'GObject' do so for backward compatibility
-- reasons.
--
-- Object has been removed in Gt3k, but this module still provides useful
-- functions.

-- * Class Hierarchy
-- |
-- @
-- | 'GObject'
-- | +----Object
-- | +----'Widget'
-- | +----'Adjustment'
-- | +----'CellRenderer'
-- | +----'FileFilter'
-- | +----'ItemFactory'
-- | +----'Tooltips'
-- | +----'TreeViewColumn'
-- @

-- * Types
  Object,
  ObjectClass,
  castToObject, gTypeObject,
  toObject,


-- * Methods
  makeNewObject,

-- * Weak references
  GWeakNotify,
  objectWeakref,
  objectWeakunref,

-- * Signals
  objectDestroy,
  notifyProperty
  ) where
import Control.Monad (when)

import System.Glib.FFI
import System.Glib.Attributes (ReadWriteAttr)
import Graphics.UI.Gtk.Types
{-# LINE 81 "./Graphics/UI/Gtk/Abstract/Object.chs" #-}
import Graphics.UI.Gtk.Signals
{-# LINE 82 "./Graphics/UI/Gtk/Abstract/Object.chs" #-}
import Data.IORef


{-# LINE 85 "./Graphics/UI/Gtk/Abstract/Object.chs" #-}

--------------------
-- Methods

-- turn the initial floating state to sunk
--
-- * The floating\/sunk concept of a GTK object is not very useful to us.
-- The following procedure circumvents the whole subject and ensures
-- proper cleanup:
-- on creation: objectRef, objectSink
-- on finalization: objectUnref
--
-- * This function cannot be bound by c2hs because it is not possible to
-- override the pointer hook.




-- This is a convenience function to generate a new widget. It adds the
-- finalizer with the method described under objectSink.
--
-- * The constr argument is the contructor of the specific object.
--

makeNewObject :: ObjectClass obj =>



  (ForeignPtr obj -> obj, FinalizerPtr obj) -> IO (Ptr obj) -> IO obj
makeNewObject (constr, objectUnref) generator = do
  objPtr <- generator
  when (objPtr == nullPtr) (fail "makeNewObject: object is NULL")

  objectRefSink objPtr




  obj <- newForeignPtr objPtr objectUnref
  return $! constr obj

type GWeakNotify = FunPtr (((Ptr ()) -> ((Ptr GObject) -> (IO ()))))
{-# LINE 127 "./Graphics/UI/Gtk/Abstract/Object.chs" #-}

foreign import ccall "wrapper" mkDestructor
  :: (Ptr () -> Ptr GObject -> IO ()) -> IO GWeakNotify

-- | Attach a callback that will be called after the
-- destroy hooks have been called
--

objectWeakref :: ObjectClass o => o -> IO () -> IO GWeakNotify



objectWeakref obj uFun = do
  funPtrContainer <- newIORef nullFunPtr
  uFunPtr <- mkDestructor $ \_ _ -> do
    uFun
    funPtr <- readIORef funPtrContainer
    freeHaskellFunPtr funPtr
  writeIORef funPtrContainer uFunPtr
  (\(GObject arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->g_object_weak_ref argPtr1 arg2 arg3) (toGObject obj) uFunPtr nullPtr
  return uFunPtr

-- | Detach a weak destroy callback function
--

objectWeakunref :: ObjectClass o => o -> GWeakNotify -> IO ()



objectWeakunref obj fun =
  (\(GObject arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->g_object_weak_unref argPtr1 arg2 arg3) (toGObject obj) fun nullPtr


--------------------
-- Signals

-- | Signals that all holders of a reference to the 'Object' should release
-- the reference that they hold. May result in finalization of the object if
-- all references are released.
--

objectDestroy :: ObjectClass self => Signal self (IO ())



objectDestroy = Signal (connect_NONE__NONE "destroy")

-- | Register a notify callback that is triggered when the given property
-- has been modified.
--
-- * Note that this callback is triggered even if the actual value of
-- the property has not changed.
-- * Not all attributes are properties. A warning will be generated at
-- runtime if the passed-in attribute is not a property of the class
-- with which it was registered.
--

notifyProperty :: ObjectClass self => ReadWriteAttr self a b -> Signal self (IO ())



notifyProperty attr = Signal (\on obj cb -> connect_PTR__NONE ("notify::"++show attr) on obj (const cb))

foreign import ccall unsafe "g_object_weak_ref"
  g_object_weak_ref :: ((Ptr GObject) -> ((FunPtr ((Ptr ()) -> ((Ptr GObject) -> (IO ())))) -> ((Ptr ()) -> (IO ()))))

foreign import ccall unsafe "g_object_weak_unref"
  g_object_weak_unref :: ((Ptr GObject) -> ((FunPtr ((Ptr ()) -> ((Ptr GObject) -> (IO ())))) -> ((Ptr ()) -> (IO ()))))
