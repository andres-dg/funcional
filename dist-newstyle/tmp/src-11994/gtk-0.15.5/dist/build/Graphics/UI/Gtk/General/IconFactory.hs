
{-# LINE 2 "./Graphics/UI/Gtk/General/IconFactory.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) IconFactory
--
-- Author : Axel Simon
--
-- Created: 24 May 2001
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
-- Manipulating stock icons
--
module Graphics.UI.Gtk.General.IconFactory (
-- * Detail
--
-- | Browse the available stock icons in the list of stock IDs found here. You
-- can also use the gtk-demo application for this purpose.
--
-- An icon factory manages a collection of 'IconSet'; a 'IconSet' manages a
-- set of variants of a particular icon (i.e. a 'IconSet' contains variants for
-- different sizes and widget states). Icons in an icon factory are named by a
-- stock ID, which is a simple string identifying the icon. Each 'Style' has a
-- list of 'IconFactory' derived from the current theme; those icon factories
-- are consulted first when searching for an icon. If the theme doesn't set a
-- particular icon, Gtk+ looks for the icon in a list of default icon
-- factories, maintained by 'iconFactoryAddDefault' and
-- 'iconFactoryRemoveDefault'. Applications with icons should add a default
-- icon factory with their icons, which will allow themes to override the icons
-- for the application.
--
-- To display an icon, always use
-- 'Graphics.UI.Gtk.General.Style.styleLookupIconSet' on the widget that
-- will display the icon, or the convenience function
-- 'Graphics.UI.Gtk.Abstract.Widget.widgetRenderIcon'. These
-- functions take the theme into account when looking up the icon to use for a
-- given stock ID.

-- * Class Hierarchy
-- |
-- @
-- | 'GObject'
-- | +----IconFactory
-- @

-- * Types
  IconFactory,
  IconFactoryClass,
  castToIconFactory, gTypeIconFactory,
  toIconFactory,

-- * Constructors
  iconFactoryNew,

-- * Methods
  iconFactoryAdd,
  iconFactoryAddDefault,
  iconFactoryLookup,
  iconFactoryLookupDefault,
  iconFactoryRemoveDefault,
  IconSet,
  iconSetNew,
  iconSetNewFromPixbuf,
  iconSetAddSource,
  iconSetRenderIcon,
  iconSetGetSizes,
  IconSource,
  iconSourceNew,
  TextDirection(..),
  iconSourceGetDirection,
  iconSourceSetDirection,
  iconSourceResetDirection,
  iconSourceGetFilename,
  iconSourceSetFilename,
  iconSourceGetPixbuf,
  iconSourceSetPixbuf,
  iconSourceGetSize,
  iconSourceSetSize,
  iconSourceResetSize,
  StateType(..),
  iconSourceGetState,
  iconSourceSetState,
  iconSourceResetState,
  IconSize(..),
  iconSizeCheck,
  iconSizeRegister,
  iconSizeRegisterAlias,
  iconSizeFromName,
  iconSizeGetName
  ) where

import Control.Applicative
import Prelude
import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import Graphics.UI.Gtk.Types
{-# LINE 113 "./Graphics/UI/Gtk/General/IconFactory.chs" #-}
import Graphics.UI.Gtk.General.Enums (TextDirection(..), StateType(..))
import Graphics.UI.Gtk.General.StockItems
import Graphics.UI.Gtk.General.Structs (IconSize(..))


{-# LINE 118 "./Graphics/UI/Gtk/General/IconFactory.chs" #-}

newtype IconSource = IconSource (ForeignPtr (IconSource))
{-# LINE 120 "./Graphics/UI/Gtk/General/IconFactory.chs" #-}

newtype IconSet = IconSet (ForeignPtr (IconSet))
{-# LINE 122 "./Graphics/UI/Gtk/General/IconFactory.chs" #-}

-- The Show instance for IconSize is here since we need c2hs.
instance Show IconSize where
  show i = unsafePerformIO (lookupSizeString (fromEnum i))
    where
    lookupSizeString n = do
      ptr <- gtk_icon_size_get_name (fromIntegral n)
      if ptr==nullPtr then return "" else glibToString <$> peekUTFString ptr

--------------------
-- Constructors

-- | Create a new IconFactory.
--
-- * An application should create a new 'IconFactory' and add all
-- needed icons.
-- By calling 'iconFactoryAddDefault' these icons become
-- available as stock objects and can easily be displayed by
-- 'Image'. Furthermore, a theme can override the icons defined by
-- the application.
--
iconFactoryNew :: IO IconFactory
iconFactoryNew =
  wrapNewGObject mkIconFactory gtk_icon_factory_new
{-# LINE 146 "./Graphics/UI/Gtk/General/IconFactory.chs" #-}

--------------------
-- Methods

-- | Add an IconSet to an IconFactory.
--
-- In order to use the new stock object, the factory as to be added to the
-- default factories by 'iconFactoryAddDefault'.
--
iconFactoryAdd :: IconFactory -> StockId -> IconSet -> IO ()
iconFactoryAdd i stockId iconSet = withUTFString stockId $ \strPtr ->
  (\(IconFactory arg1) arg2 (IconSet arg3) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg3 $ \argPtr3 ->gtk_icon_factory_add argPtr1 arg2 argPtr3) i strPtr iconSet

-- | Add all entries of the IconFactory to the
-- applications stock object database.
--
iconFactoryAddDefault :: IconFactory -> IO ()
iconFactoryAddDefault = (\(IconFactory arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_icon_factory_add_default argPtr1)
{-# LINE 164 "./Graphics/UI/Gtk/General/IconFactory.chs" #-}

-- | Looks up the stock id in the icon factory, returning an icon set if found,
-- otherwise Nothing.
--
-- For display to the user, you should use
-- 'Graphics.UI.Gtk.General.Style.styleLookupIconSet' on the
-- 'Graphics.UI.Gtk.General.Style.Style'
-- for the widget that will display the icon, instead of using this function
-- directly, so that themes are taken into account.
--
iconFactoryLookup :: IconFactory -> StockId -> IO (Maybe IconSet)
iconFactoryLookup i stockId =
  withUTFString stockId $ \strPtr -> do
  iconSetPtr <- (\(IconFactory arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_icon_factory_lookup argPtr1 arg2) i strPtr
  if iconSetPtr == nullPtr then return Nothing else liftM (Just . IconSet) $
    newForeignPtr iconSetPtr icon_set_unref

-- | Looks for an icon in the list of default icon factories.
--
-- For display to the user, you should use
-- 'Graphics.UI.Gtk.General.Style.styleLookupIconSet' on the
-- 'Graphics.UI.Gtk.General.Style.Style'
-- for the widget that will display the icon, instead of using this function
-- directly, so that themes are taken into account.
--
iconFactoryLookupDefault :: StockId -> IO (Maybe IconSet)
iconFactoryLookupDefault stockId =
  withUTFString stockId $ \strPtr -> do
  iconSetPtr <- gtk_icon_factory_lookup_default strPtr
  if iconSetPtr == nullPtr then return Nothing else liftM (Just . IconSet) $
    newForeignPtr iconSetPtr icon_set_unref

-- | Remove an IconFactory from the
-- application's stock database.
--
iconFactoryRemoveDefault :: IconFactory -> IO ()
iconFactoryRemoveDefault = (\(IconFactory arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_icon_factory_remove_default argPtr1)
{-# LINE 201 "./Graphics/UI/Gtk/General/IconFactory.chs" #-}

-- | Add an 'IconSource' (an Icon with
-- attributes) to an 'IconSet'.
--
-- * If an icon is looked up in the IconSet @set@ the best matching
-- IconSource will be taken. It is therefore advisable to add a default
-- (wildcarded) icon, than can be used if no exact match is found.
--
iconSetAddSource :: IconSet -> IconSource -> IO ()
iconSetAddSource set source = (\(IconSet arg1) (IconSource arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_icon_set_add_source argPtr1 argPtr2) set source

iconSetRenderIcon :: WidgetClass widget => IconSet
                  -> TextDirection
                  -> StateType
                  -> IconSize
                  -> widget
                  -> IO Pixbuf
iconSetRenderIcon set dir state size widget = wrapNewGObject mkPixbuf $
  (\(IconSet arg1) (Style arg2) arg3 arg4 arg5 (Widget arg6) arg7 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->withForeignPtr arg6 $ \argPtr6 ->gtk_icon_set_render_icon argPtr1 argPtr2 arg3 arg4 arg5 argPtr6 arg7) set (Style nullForeignPtr)
    ((fromIntegral.fromEnum) dir) ((fromIntegral.fromEnum) state)
    ((fromIntegral.fromEnum) size) (toWidget widget) nullPtr

-- | Create a new IconSet.
--
-- * Each icon in an application is contained in an 'IconSet'. The
-- 'IconSet' contains several variants ('IconSource's) to
-- accomodate for different sizes and states.
--
iconSetNew :: IO IconSet
iconSetNew = do
  isPtr <- gtk_icon_set_new
{-# LINE 232 "./Graphics/UI/Gtk/General/IconFactory.chs" #-}
  liftM IconSet $ newForeignPtr isPtr icon_set_unref

-- | Creates a new 'IconSet' with the given pixbuf as the default\/fallback
-- source image. If you don't add any additional "IconSource" to the icon set,
-- all variants of the icon will be created from the pixbuf, using scaling,
-- pixelation, etc. as required to adjust the icon size or make the icon look
-- insensitive\/prelighted.
--
iconSetNewFromPixbuf :: Pixbuf -> IO IconSet
iconSetNewFromPixbuf pixbuf = do
  isPtr <- (\(Pixbuf arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_icon_set_new_from_pixbuf argPtr1) pixbuf
  liftM IconSet $ newForeignPtr isPtr icon_set_unref

-- | Obtains a list of icon sizes this icon set can render.
--
iconSetGetSizes :: IconSet -> IO [IconSize]
iconSetGetSizes set =
  alloca $ \sizesArrPtr -> alloca $ \lenPtr -> do
  (\(IconSet arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_icon_set_get_sizes argPtr1 arg2 arg3) set sizesArrPtr lenPtr
  len <- peek lenPtr
  sizesArr <- peek sizesArrPtr
  list <- peekArray (fromIntegral len) sizesArr
  g_free (castPtr sizesArr)
  return $ map (toEnum.fromIntegral) list

foreign import ccall unsafe "&gtk_icon_set_unref"
  icon_set_unref :: FinalizerPtr IconSet

-- | Check if a given IconSize is registered.
--
-- * Useful if your application expects a theme to install a set with a
-- specific size. You can test if this actually happend and use another size
-- if not.
--
iconSizeCheck :: IconSize -> IO Bool
iconSizeCheck size = liftM toBool $
  gtk_icon_size_lookup ((fromIntegral . fromEnum) size) nullPtr nullPtr

-- | Register a new IconSize.
--
iconSizeRegister :: GlibString string
  => string -- ^ the new name of the size
  -> Int -- ^ the width of the icon
  -> Int -- ^ the height of the icon
  -> IO IconSize -- ^ the new icon size
iconSizeRegister name width height = liftM (toEnum . fromIntegral) $
  withUTFString name $ \strPtr -> gtk_icon_size_register
{-# LINE 279 "./Graphics/UI/Gtk/General/IconFactory.chs" #-}
  strPtr (fromIntegral width) (fromIntegral height)

-- | Register an additional alias for a name.
--
iconSizeRegisterAlias :: GlibString string => IconSize -> string -> IO ()
iconSizeRegisterAlias target alias = withUTFString alias $ \strPtr ->
  gtk_icon_size_register_alias strPtr ((fromIntegral . fromEnum) target)

-- | Lookup an IconSize by name.
--
-- * This fixed value 'iconSizeInvalid' is returned if the name was
-- not found.
--
iconSizeFromName :: GlibString string => string -> IO IconSize
iconSizeFromName name = liftM (toEnum . fromIntegral) $
  withUTFString name gtk_icon_size_from_name
{-# LINE 295 "./Graphics/UI/Gtk/General/IconFactory.chs" #-}

-- | Lookup the name of an IconSize.
--
-- * Returns @Nothing@ if the name was not found.
--
iconSizeGetName :: GlibString string => IconSize -> IO (Maybe string)
iconSizeGetName size = do
  strPtr <- gtk_icon_size_get_name ((fromIntegral . fromEnum) size)
  if strPtr==nullPtr then return Nothing else liftM Just $ peekUTFString strPtr

-- | Retrieve the 'TextDirection' of
-- this IconSource.
--
-- * @Nothing@ is returned if no explicit direction was set.
--
iconSourceGetDirection :: IconSource -> IO (Maybe TextDirection)
iconSourceGetDirection is = do
  res <- (\(IconSource arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_icon_source_get_direction_wildcarded argPtr1) is
  if (toBool res) then return Nothing else liftM (Just .toEnum.fromIntegral) $
    (\(IconSource arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_icon_source_get_direction argPtr1) is

-- | Retrieve the filename this IconSource was
-- based on.
--
-- * Returns @Nothing@ if the IconSource was generated by a Pixbuf.
--
iconSourceGetFilename :: GlibString string => IconSource -> IO (Maybe string)
iconSourceGetFilename is = do



  strPtr <- (\(IconSource arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_icon_source_get_filename argPtr1) is

  if strPtr==nullPtr then return Nothing else liftM Just $ peekUTFString strPtr

-- | Retrieve the 'IconSize' of this
-- IconSource.
--
-- * @Nothing@ is returned if no explicit size was set (i.e. this
-- 'IconSource' matches all sizes).
--
iconSourceGetSize :: IconSource -> IO (Maybe IconSize)
iconSourceGetSize is = do
  res <- (\(IconSource arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_icon_source_get_size_wildcarded argPtr1) is
  if (toBool res) then return Nothing else liftM (Just . toEnum . fromIntegral) $
    (\(IconSource arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_icon_source_get_size argPtr1) is

-- | Retrieve the 'StateType' of this
-- 'IconSource'.
--
-- * @Nothing@ is returned if the 'IconSource' matches all
-- states.
--
iconSourceGetState :: IconSource -> IO (Maybe StateType)
iconSourceGetState is = do
  res <- (\(IconSource arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_icon_source_get_state_wildcarded argPtr1) is
  if (toBool res) then return Nothing else liftM (Just .toEnum.fromIntegral) $
    (\(IconSource arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_icon_source_get_state argPtr1) is

-- | Create a new IconSource.
--
-- * An IconSource is a single image that is usually added to an IconSet. Next
-- to the image it contains information about which state, text direction
-- and size it should apply.
--
iconSourceNew :: IO IconSource
iconSourceNew = do
  isPtr <- gtk_icon_source_new
{-# LINE 363 "./Graphics/UI/Gtk/General/IconFactory.chs" #-}
  liftM IconSource $ newForeignPtr isPtr icon_source_free

foreign import ccall unsafe "&gtk_icon_source_free"
  icon_source_free :: FinalizerPtr IconSource

-- | Mark this 'IconSource' that it
-- should only apply to the specified 'TextDirection'.
--
iconSourceSetDirection :: IconSource -> TextDirection -> IO ()
iconSourceSetDirection is td = do
  (\(IconSource arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_icon_source_set_direction_wildcarded argPtr1 arg2) is (fromBool False)
  (\(IconSource arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_icon_source_set_direction argPtr1 arg2) is ((fromIntegral.fromEnum) td)

-- | Reset the specific
-- 'TextDirection' set with 'iconSourceSetDirection'.
--
iconSourceResetDirection :: IconSource -> IO ()
iconSourceResetDirection is =
  (\(IconSource arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_icon_source_set_direction_wildcarded argPtr1 arg2) is (fromBool True)

-- | Load an icon picture from this filename.
--
iconSourceSetFilename :: GlibFilePath fp => IconSource -> fp -> IO ()
iconSourceSetFilename is name =



  withUTFFilePath name $ (\(IconSource arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_icon_source_set_filename argPtr1 arg2) is


-- | Retrieves the source pixbuf, or Nothing if none is set.
--
iconSourceGetPixbuf :: IconSource -> IO (Maybe Pixbuf)
iconSourceGetPixbuf is = maybeNull (makeNewGObject mkPixbuf) $
  (\(IconSource arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_icon_source_get_pixbuf argPtr1) is

-- | Sets a pixbuf to use as a base image when creating icon variants for
-- 'IconSet'.
--
iconSourceSetPixbuf :: IconSource -> Pixbuf -> IO ()
iconSourceSetPixbuf is pb = do
  (\(IconSource arg1) (Pixbuf arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_icon_source_set_pixbuf argPtr1 argPtr2) is pb

-- | Set this 'IconSource' to a specific
-- size.
--
iconSourceSetSize :: IconSource -> IconSize -> IO ()
iconSourceSetSize is size = do
  (\(IconSource arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_icon_source_set_size_wildcarded argPtr1 arg2) is (fromBool False)
  (\(IconSource arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_icon_source_set_size argPtr1 arg2) is ((fromIntegral . fromEnum) size)

-- | Reset the 'IconSize' of this
-- 'IconSource' so that is matches anything.
--
iconSourceResetSize :: IconSource -> IO ()
iconSourceResetSize is =
  (\(IconSource arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_icon_source_set_size_wildcarded argPtr1 arg2) is (fromBool True)

-- | Mark this icon to be used only with this
-- specific state.
--
iconSourceSetState :: IconSource -> StateType -> IO ()
iconSourceSetState is state = do
  (\(IconSource arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_icon_source_set_state_wildcarded argPtr1 arg2) is (fromBool False)
  (\(IconSource arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_icon_source_set_state argPtr1 arg2) is ((fromIntegral.fromEnum) state)

-- | Reset the 'StateType' of this
-- 'IconSource' so that is matches anything.
--
iconSourceResetState :: IconSource -> IO ()
iconSourceResetState is =
  (\(IconSource arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_icon_source_set_state_wildcarded argPtr1 arg2) is (fromBool True)

foreign import ccall unsafe "gtk_icon_size_get_name"
  gtk_icon_size_get_name :: (CInt -> (IO (Ptr CChar)))

foreign import ccall unsafe "gtk_icon_factory_new"
  gtk_icon_factory_new :: (IO (Ptr IconFactory))

foreign import ccall unsafe "gtk_icon_factory_add"
  gtk_icon_factory_add :: ((Ptr IconFactory) -> ((Ptr CChar) -> ((Ptr IconSet) -> (IO ()))))

foreign import ccall unsafe "gtk_icon_factory_add_default"
  gtk_icon_factory_add_default :: ((Ptr IconFactory) -> (IO ()))

foreign import ccall unsafe "gtk_icon_factory_lookup"
  gtk_icon_factory_lookup :: ((Ptr IconFactory) -> ((Ptr CChar) -> (IO (Ptr IconSet))))

foreign import ccall unsafe "gtk_icon_factory_lookup_default"
  gtk_icon_factory_lookup_default :: ((Ptr CChar) -> (IO (Ptr IconSet)))

foreign import ccall unsafe "gtk_icon_factory_remove_default"
  gtk_icon_factory_remove_default :: ((Ptr IconFactory) -> (IO ()))

foreign import ccall unsafe "gtk_icon_set_add_source"
  gtk_icon_set_add_source :: ((Ptr IconSet) -> ((Ptr IconSource) -> (IO ())))

foreign import ccall safe "gtk_icon_set_render_icon"
  gtk_icon_set_render_icon :: ((Ptr IconSet) -> ((Ptr Style) -> (CInt -> (CInt -> (CInt -> ((Ptr Widget) -> ((Ptr CChar) -> (IO (Ptr Pixbuf)))))))))

foreign import ccall unsafe "gtk_icon_set_new"
  gtk_icon_set_new :: (IO (Ptr IconSet))

foreign import ccall unsafe "gtk_icon_set_new_from_pixbuf"
  gtk_icon_set_new_from_pixbuf :: ((Ptr Pixbuf) -> (IO (Ptr IconSet)))

foreign import ccall unsafe "gtk_icon_set_get_sizes"
  gtk_icon_set_get_sizes :: ((Ptr IconSet) -> ((Ptr (Ptr CInt)) -> ((Ptr CInt) -> (IO ()))))

foreign import ccall unsafe "g_free"
  g_free :: ((Ptr ()) -> (IO ()))

foreign import ccall safe "gtk_icon_size_lookup"
  gtk_icon_size_lookup :: (CInt -> ((Ptr CInt) -> ((Ptr CInt) -> (IO CInt))))

foreign import ccall unsafe "gtk_icon_size_register"
  gtk_icon_size_register :: ((Ptr CChar) -> (CInt -> (CInt -> (IO CInt))))

foreign import ccall unsafe "gtk_icon_size_register_alias"
  gtk_icon_size_register_alias :: ((Ptr CChar) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_icon_size_from_name"
  gtk_icon_size_from_name :: ((Ptr CChar) -> (IO CInt))

foreign import ccall safe "gtk_icon_source_get_direction_wildcarded"
  gtk_icon_source_get_direction_wildcarded :: ((Ptr IconSource) -> (IO CInt))

foreign import ccall unsafe "gtk_icon_source_get_direction"
  gtk_icon_source_get_direction :: ((Ptr IconSource) -> (IO CInt))

foreign import ccall unsafe "gtk_icon_source_get_filename"
  gtk_icon_source_get_filename :: ((Ptr IconSource) -> (IO (Ptr CChar)))

foreign import ccall unsafe "gtk_icon_source_get_size_wildcarded"
  gtk_icon_source_get_size_wildcarded :: ((Ptr IconSource) -> (IO CInt))

foreign import ccall unsafe "gtk_icon_source_get_size"
  gtk_icon_source_get_size :: ((Ptr IconSource) -> (IO CInt))

foreign import ccall unsafe "gtk_icon_source_get_state_wildcarded"
  gtk_icon_source_get_state_wildcarded :: ((Ptr IconSource) -> (IO CInt))

foreign import ccall unsafe "gtk_icon_source_get_state"
  gtk_icon_source_get_state :: ((Ptr IconSource) -> (IO CInt))

foreign import ccall unsafe "gtk_icon_source_new"
  gtk_icon_source_new :: (IO (Ptr IconSource))

foreign import ccall unsafe "gtk_icon_source_set_direction_wildcarded"
  gtk_icon_source_set_direction_wildcarded :: ((Ptr IconSource) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_icon_source_set_direction"
  gtk_icon_source_set_direction :: ((Ptr IconSource) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_icon_source_set_filename"
  gtk_icon_source_set_filename :: ((Ptr IconSource) -> ((Ptr CChar) -> (IO ())))

foreign import ccall unsafe "gtk_icon_source_get_pixbuf"
  gtk_icon_source_get_pixbuf :: ((Ptr IconSource) -> (IO (Ptr Pixbuf)))

foreign import ccall safe "gtk_icon_source_set_pixbuf"
  gtk_icon_source_set_pixbuf :: ((Ptr IconSource) -> ((Ptr Pixbuf) -> (IO ())))

foreign import ccall unsafe "gtk_icon_source_set_size_wildcarded"
  gtk_icon_source_set_size_wildcarded :: ((Ptr IconSource) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_icon_source_set_size"
  gtk_icon_source_set_size :: ((Ptr IconSource) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_icon_source_set_state_wildcarded"
  gtk_icon_source_set_state_wildcarded :: ((Ptr IconSource) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_icon_source_set_state"
  gtk_icon_source_set_state :: ((Ptr IconSource) -> (CInt -> (IO ())))
