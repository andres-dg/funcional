
{-# LINE 2 "./Graphics/UI/Gtk/General/IconTheme.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget IconTheme
--
-- Author : Andy Stewart
--
-- Created: 28 Mar 2010
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
-- Looking up icons by name
--
-- * Module available since Gtk+ version 2.4
--
module Graphics.UI.Gtk.General.IconTheme (

-- * Detail
--
-- | 'IconTheme' provides a facility for looking up icons by name and size. The main reason for using a
-- name rather than simply providing a filename is to allow different icons to be used depending on
-- what icon theme is selecetd by the user. The operation of icon themes on Linux and Unix follows the
-- Icon Theme Specification. There is a default icon theme, named hicolor where applications should
-- install their icons, but more additional application themes can be installed as operating system
-- vendors and users choose.
--
-- Named icons are similar to the Themeable Stock Images facility, and the distinction between the
-- two may be a bit confusing. A few things to keep in mind:
--
-- * Stock images usually are used in conjunction with Stock Items, such as ''StockOk'' or
-- ''StockOpen''. Named icons are easier to set up and therefore are more useful for new icons
-- that an application wants to add, such as application icons or window icons.
--
-- * Stock images can only be loaded at the symbolic sizes defined by the 'IconSize' enumeration, or
-- by custom sizes defined by 'iconSizeRegister', while named icons are more flexible and any
-- pixel size can be specified.
--
-- * Because stock images are closely tied to stock items, and thus to actions in the user interface,
-- stock images may come in multiple variants for different widget states or writing directions.
--
-- A good rule of thumb is that if there is a stock image for what you want to use, use it, otherwise
-- use a named icon. It turns out that internally stock images are generally defined in terms of one or
-- more named icons. (An example of the more than one case is icons that depend on writing direction;
-- ''StockGoForward'' uses the two themed icons 'gtkStockGoForwardLtr' and
-- 'gtkStockGoForwardRtl'.)
--
-- In many cases, named themes are used indirectly, via 'Image' or stock items, rather than directly,
-- but looking up icons directly is also simple. The 'IconTheme' object acts as a database of all the
-- icons in the current theme. You can create new 'IconTheme' objects, but its much more efficient to
-- use the standard icon theme for the 'Screen' so that the icon information is shared with other
-- people looking up icons. In the case where the default screen is being used, looking up an icon can
-- be as simple as:

-- * Class Hierarchy
-- |
-- @
-- | 'GObject'
-- | +----IconTheme
-- @


-- * Types
  IconTheme,
  IconThemeClass,
  castToIconTheme,
  toIconTheme,

  IconInfo,

-- * Enums
  IconLookupFlags(..),
  IconThemeError(..),

-- * Constructors
  iconThemeNew,


  iconInfoNewForPixbuf,


-- * Methods
  iconThemeGetDefault,
  iconThemeGetForScreen,
  iconThemeSetScreen,
  iconThemeSetSearchPath,
  iconThemeGetSearchPath,
  iconThemeAppendSearchPath,
  iconThemePrependSearchPath,
  iconThemeSetCustomTheme,
  iconThemeHasIcon,
  iconThemeLookupIcon,

  iconThemeChooseIcon,


  iconThemeLookupByGIcon,



  iconThemeLoadIcon,

  iconThemeListContexts,

  iconThemeListIcons,

  iconThemeGetIconSizes,

  iconThemeGetExampleIconName,
  iconThemeRescanIfNeeded,
  iconThemeAddBuiltinIcon,
  iconThemeErrorQuark,

  iconInfoCopy,
  iconInfoGetAttachPoints,
  iconInfoGetBaseSize,
  iconInfoGetBuiltinPixbuf,
  iconInfoGetDisplayName,
  iconInfoGetEmbeddedRect,
  iconInfoGetFilename,
  iconInfoLoadIcon,
  iconInfoSetRawCoordinates,

-- * Signals
  iconThemeChanged,

  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.GList
import System.Glib.GError (propagateGError)
import Graphics.UI.Gtk.General.Structs (Rectangle, Point)
import Graphics.UI.Gtk.Types
{-# LINE 150 "./Graphics/UI/Gtk/General/IconTheme.chs" #-}
import Graphics.UI.Gtk.Signals
{-# LINE 151 "./Graphics/UI/Gtk/General/IconTheme.chs" #-}

import System.GIO.Types
{-# LINE 153 "./Graphics/UI/Gtk/General/IconTheme.chs" #-}



{-# LINE 156 "./Graphics/UI/Gtk/General/IconTheme.chs" #-}


--------------------
-- Enums
data IconLookupFlags = IconLookupNoSvg
                     | IconLookupForceSvg
                     | IconLookupUseBuiltin
                     | IconLookupGenericFallback
                     | IconLookupForceSize
                     deriving (Bounded,Eq,Show)
instance Enum IconLookupFlags where
  fromEnum IconLookupNoSvg = 1
  fromEnum IconLookupForceSvg = 2
  fromEnum IconLookupUseBuiltin = 4
  fromEnum IconLookupGenericFallback = 8
  fromEnum IconLookupForceSize = 16

  toEnum 1 = IconLookupNoSvg
  toEnum 2 = IconLookupForceSvg
  toEnum 4 = IconLookupUseBuiltin
  toEnum 8 = IconLookupGenericFallback
  toEnum 16 = IconLookupForceSize
  toEnum unmatched = error ("IconLookupFlags.toEnum: Cannot match " ++ show unmatched)

  succ IconLookupNoSvg = IconLookupForceSvg
  succ IconLookupForceSvg = IconLookupUseBuiltin
  succ IconLookupUseBuiltin = IconLookupGenericFallback
  succ IconLookupGenericFallback = IconLookupForceSize
  succ _ = undefined

  pred IconLookupForceSvg = IconLookupNoSvg
  pred IconLookupUseBuiltin = IconLookupForceSvg
  pred IconLookupGenericFallback = IconLookupUseBuiltin
  pred IconLookupForceSize = IconLookupGenericFallback
  pred _ = undefined

  enumFromTo x y | fromEnum x == fromEnum y = [ y ]
                 | otherwise = x : enumFromTo (succ x) y
  enumFrom x = enumFromTo x IconLookupForceSize
  enumFromThen _ _ =     error "Enum IconLookupFlags: enumFromThen not implemented"
  enumFromThenTo _ _ _ =     error "Enum IconLookupFlags: enumFromThenTo not implemented"

{-# LINE 161 "./Graphics/UI/Gtk/General/IconTheme.chs" #-}

data IconThemeError = IconThemeNotFound
                    | IconThemeFailed
                    deriving (Enum,Bounded,Eq,Show)

{-# LINE 163 "./Graphics/UI/Gtk/General/IconTheme.chs" #-}

--------------------
-- Constructors

-- | Creates a new icon theme object. Icon theme objects are used to lookup up
-- an icon by name in a particular icon theme. Usually, you'll want to use
-- 'iconThemeGetDefault' or 'iconThemeGetForScreen' rather than creating a new
-- icon theme object for scratch.
--
iconThemeNew :: IO IconTheme
iconThemeNew =
  wrapNewGObject mkIconTheme $
  gtk_icon_theme_new
{-# LINE 176 "./Graphics/UI/Gtk/General/IconTheme.chs" #-}

--------------------
-- Methods

-- | Gets the icon theme for the default screen. See 'iconThemeGetForScreen'.
--
iconThemeGetDefault ::
    IO IconTheme -- ^ returns A unique 'IconTheme' associated with the default
                 -- screen. This icon theme is associated with the screen and
                 -- can be used as long as the screen is open.
iconThemeGetDefault =
  makeNewGObject mkIconTheme $
  gtk_icon_theme_get_default
{-# LINE 189 "./Graphics/UI/Gtk/General/IconTheme.chs" #-}

-- | Gets the icon theme object associated with @screen@; if this function has
-- not previously been called for the given screen, a new icon theme object
-- will be created and associated with the screen. Icon theme objects are
-- fairly expensive to create, so using this function is usually a better
-- choice than calling than 'iconThemeNew' and setting the screen yourself; by
-- using this function a single icon theme object will be shared between users.
--
iconThemeGetForScreen ::
    Screen -- ^ @screen@ - a 'Screen'
 -> IO IconTheme -- ^ returns A unique 'IconTheme' associated with the given
                 -- screen.
iconThemeGetForScreen screen =
  makeNewGObject mkIconTheme $
  (\(Screen arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_icon_theme_get_for_screen argPtr1)
{-# LINE 204 "./Graphics/UI/Gtk/General/IconTheme.chs" #-}
    screen

-- | Sets the screen for an icon theme; the screen is used to track the user's
-- currently configured icon theme, which might be different for different
-- screens.
--
iconThemeSetScreen :: IconThemeClass self => self
 -> Screen -- ^ @screen@ - a 'Screen'
 -> IO ()
iconThemeSetScreen self screen =
  (\(IconTheme arg1) (Screen arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_icon_theme_set_screen argPtr1 argPtr2)
{-# LINE 215 "./Graphics/UI/Gtk/General/IconTheme.chs" #-}
    (toIconTheme self)
    screen

-- | Sets the search path for the icon theme object. When looking for an icon
-- theme, Gtk+ will search for a subdirectory of one or more of the directories
-- in @path@ with the same name as the icon theme. (Themes from multiple of the
-- path elements are combined to allow themes to be extended by adding icons in
-- the user's home directory.)
--
-- In addition if an icon found isn't found either in the current icon theme
-- or the default icon theme, and an image file with the right name is found
-- directly in one of the elements of @path@, then that image will be used for
-- the icon name. (This is legacy feature, and new icons should be put into the
-- default icon theme, which is called DEFAULT_THEME_NAME, rather than directly
-- on the icon path.)
--
iconThemeSetSearchPath :: (IconThemeClass self, GlibFilePath fp) => self
 -> [fp] -- ^ @path@ - list of directories that are searched for icon
           -- themes
 -> Int -- ^ @nElements@ - number of elements in @path@.
 -> IO ()
iconThemeSetSearchPath self path nElements =
  withUTFFilePathArray path $ \pathPtr ->
  (\(IconTheme arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_icon_theme_set_search_path argPtr1 arg2 arg3)
{-# LINE 239 "./Graphics/UI/Gtk/General/IconTheme.chs" #-}
    (toIconTheme self)
    pathPtr
    (fromIntegral nElements)

-- | Gets the current search path. See 'iconThemeSetSearchPath'.
--
iconThemeGetSearchPath :: (IconThemeClass self, GlibFilePath fp) => self
 -> IO ([fp], Int) -- ^ @(path, nElements)@
                                -- @path@ - location to store a list of icon theme path
                                -- directories.
iconThemeGetSearchPath self =
  alloca $ \nElementsPtr ->
  allocaArray 0 $ \pathPtr -> do
  (\(IconTheme arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_icon_theme_get_search_path argPtr1 arg2 arg3)
{-# LINE 253 "./Graphics/UI/Gtk/General/IconTheme.chs" #-}
    (toIconTheme self)
    (castPtr pathPtr)
    nElementsPtr
  pathStr <- readUTFFilePathArray0 pathPtr
  nElements <- peek nElementsPtr
  return (pathStr, fromIntegral nElements)

-- | Appends a directory to the search path. See 'iconThemeSetSearchPath'.
--
iconThemeAppendSearchPath :: (IconThemeClass self, GlibFilePath fp) => self
 -> fp -- ^ @path@ - directory name to append to the icon path
 -> IO ()
iconThemeAppendSearchPath self path =
  withUTFFilePath path $ \pathPtr ->
  (\(IconTheme arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_icon_theme_append_search_path argPtr1 arg2)
{-# LINE 268 "./Graphics/UI/Gtk/General/IconTheme.chs" #-}
    (toIconTheme self)
    pathPtr

-- | Prepends a directory to the search path. See 'iconThemeSetSearchPath'.
--
iconThemePrependSearchPath :: (IconThemeClass self, GlibFilePath fp) => self
 -> fp -- ^ @path@ - directory name to prepend to the icon path
 -> IO ()
iconThemePrependSearchPath self path =
  withUTFFilePath path $ \pathPtr ->
  (\(IconTheme arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_icon_theme_prepend_search_path argPtr1 arg2)
{-# LINE 279 "./Graphics/UI/Gtk/General/IconTheme.chs" #-}
    (toIconTheme self)
    pathPtr

-- | Sets the name of the icon theme that the 'IconTheme' object uses
-- overriding system configuration. This function cannot be called on the icon
-- theme objects returned from 'iconThemeGetDefault' and
-- 'iconThemeGetForScreen'.
--
iconThemeSetCustomTheme :: (IconThemeClass self, GlibString string) => self
 -> (Maybe string) -- ^ @themeName@ name of icon theme to use instead of configured theme, or 'Nothing' to unset a previously set custom theme
 -> IO ()
iconThemeSetCustomTheme self themeName =
  maybeWith withUTFString themeName $ \themeNamePtr ->
  (\(IconTheme arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_icon_theme_set_custom_theme argPtr1 arg2)
{-# LINE 293 "./Graphics/UI/Gtk/General/IconTheme.chs" #-}
    (toIconTheme self)
    themeNamePtr

-- | Checks whether an icon theme includes an icon for a particular name.
--
iconThemeHasIcon :: (IconThemeClass self, GlibString string) => self
 -> string -- ^ @iconName@ - the name of an icon
 -> IO Bool -- ^ returns @True@ if @iconTheme@ includes an icon for
            -- @iconName@.
iconThemeHasIcon self iconName =
  liftM toBool $
  withUTFString iconName $ \iconNamePtr ->
  (\(IconTheme arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_icon_theme_has_icon argPtr1 arg2)
{-# LINE 306 "./Graphics/UI/Gtk/General/IconTheme.chs" #-}
    (toIconTheme self)
    iconNamePtr

-- | Looks up a named icon and returns a structure containing information such
-- as the filename of the icon. The icon can then be rendered into a pixbuf
-- using 'iconInfoLoadIcon'. ('iconThemeLoadIcon' combines these two steps if
-- all you need is the pixbuf.)
--
iconThemeLookupIcon :: (IconThemeClass self, GlibString string) => self
 -> string -- ^ @iconName@ - the name of the icon to lookup
 -> Int -- ^ @size@ - desired icon size
 -> IconLookupFlags -- ^ @flags@ - flags modifying the behavior of the
                        -- icon lookup
 -> IO (Maybe IconInfo) -- ^ returns a 'IconInfo'
                        -- structure containing information about the icon, or
                         -- 'Nothing' if the icon wasn't found.
iconThemeLookupIcon self iconName size flags =
  withUTFString iconName $ \iconNamePtr -> do
  iiPtr <- (\(IconTheme arg1) arg2 arg3 arg4 -> withForeignPtr arg1 $ \argPtr1 ->gtk_icon_theme_lookup_icon argPtr1 arg2 arg3 arg4)
{-# LINE 325 "./Graphics/UI/Gtk/General/IconTheme.chs" #-}
          (toIconTheme self)
          iconNamePtr
          (fromIntegral size)
          ((fromIntegral . fromEnum) flags)
  if iiPtr == nullPtr
     then return Nothing
     else liftM Just (mkIconInfo (castPtr iiPtr))


-- | Looks up a named icon and returns a structure containing information such
-- as the filename of the icon. The icon can then be rendered into a pixbuf
-- using 'iconInfoLoadIcon'. ('iconThemeLoadIcon' combines these two steps if
-- all you need is the pixbuf.)
--
-- If @iconNames@ contains more than one name, this function tries them all
-- in the given order before falling back to inherited icon themes.
--
-- * Available since Gtk+ version 2.12
--
iconThemeChooseIcon :: (IconThemeClass self, GlibString string) => self
 -> [string] -- ^ @iconNames@ terminated list of icon names to lookup
 -> Int -- ^ @size@ - desired icon size
 -> IconLookupFlags -- ^ @flags@ - flags modifying the behavior of the
                        -- icon lookup
 -> IO (Maybe IconInfo) -- ^ returns a 'IconInfo'
                        -- structure containing information about the icon, or
                         -- 'Nothing' if the icon wasn't found.
iconThemeChooseIcon self iconNames size flags =
  withUTFStringArray0 iconNames $ \iconNamesPtr -> do
  iiPtr <- (\(IconTheme arg1) arg2 arg3 arg4 -> withForeignPtr arg1 $ \argPtr1 ->gtk_icon_theme_choose_icon argPtr1 arg2 arg3 arg4)
{-# LINE 355 "./Graphics/UI/Gtk/General/IconTheme.chs" #-}
          (toIconTheme self)
          iconNamesPtr
          (fromIntegral size)
          ((fromIntegral . fromEnum) flags)
  if iiPtr == nullPtr
     then return Nothing
     else liftM Just (mkIconInfo (castPtr iiPtr))



-- | Looks up an icon and returns a structure containing information such as
-- the filename of the icon. The icon can then be rendered into a pixbuf using
-- 'iconInfoLoadIcon'.
--
-- * Available since Gtk+ version 2.14
--
iconThemeLookupByGIcon :: (IconThemeClass self, IconClass icon) => self
 -> icon -- ^ @icon@ - the 'Icon' to look up
 -> Int -- ^ @size@ - desired icon size
 -> IconLookupFlags -- ^ @flags@ - flags modifying the behavior of the
                        -- icon lookup
 -> IO (Maybe IconInfo) -- ^ returns a 'IconInfo'
                        -- structure containing information about the icon, or
                        -- 'Nothing' if the icon wasn't found.
iconThemeLookupByGIcon self icon size flags = do
    iiPtr <- (\(IconTheme arg1) (Icon arg2) arg3 arg4 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_icon_theme_lookup_by_gicon argPtr1 argPtr2 arg3 arg4)
{-# LINE 381 "./Graphics/UI/Gtk/General/IconTheme.chs" #-}
            (toIconTheme self)
            (toIcon icon)
            (fromIntegral size)
            ((fromIntegral . fromEnum) flags)
    if iiPtr == nullPtr
       then return Nothing
       else liftM Just (mkIconInfo (castPtr iiPtr))




-- | Looks up an icon in an icon theme, scales it to the given size and
-- renders it into a pixbuf. This is a convenience function; if more details
-- about the icon are needed, use 'iconThemeLookupIcon' followed by
-- 'iconInfoLoadIcon'.
--
-- Note that you probably want to listen for icon theme changes and update
-- the icon. This is usually done by connecting to the 'Widget'::style-set
-- signal. If for some reason you do not want to update the icon when the icon
-- theme changes, you should consider using 'pixbufCopy' to make a private copy
-- of the pixbuf returned by this function. Otherwise Gtk+ may need to keep the
-- old icon theme loaded, which would be a waste of memory.
--
iconThemeLoadIcon :: (IconThemeClass self, GlibString string) => self
 -> string -- ^ @iconName@ - the name of the icon to lookup
 -> Int -- ^ @size@ - the desired icon size. The resulting icon
                      -- may not be exactly this size; see 'iconInfoLoadIcon'.
 -> IconLookupFlags -- ^ @flags@ - flags modifying the behavior of the icon
                      -- lookup
 -> IO (Maybe Pixbuf) -- ^ returns the rendered icon; this may be a newly
                      -- created icon or a new reference to an internal icon,
                      -- so you must not modify the icon.
                      -- `Nothing` if the icon isn't found.
iconThemeLoadIcon self iconName size flags =
  maybeNull (wrapNewGObject mkPixbuf) $
  propagateGError $ \errorPtr ->
  withUTFString iconName $ \iconNamePtr ->
  (\(IconTheme arg1) arg2 arg3 arg4 arg5 -> withForeignPtr arg1 $ \argPtr1 ->gtk_icon_theme_load_icon argPtr1 arg2 arg3 arg4 arg5)
{-# LINE 419 "./Graphics/UI/Gtk/General/IconTheme.chs" #-}
    (toIconTheme self)
    iconNamePtr
    (fromIntegral size)
    ((fromIntegral . fromEnum) flags)
    errorPtr


-- | Gets the list of contexts available within the current hierarchy of icon
-- themes
--
-- * Available since Gtk+ version 2.12
--
iconThemeListContexts :: (IconThemeClass self, GlibString string) => self
 -> IO [string] -- ^ returns a String list
                            -- holding the names of all the contexts in the
                            -- theme.
iconThemeListContexts self = do
  glistPtr <- (\(IconTheme arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_icon_theme_list_contexts argPtr1) (toIconTheme self)
  list <- fromGList glistPtr
  result <- mapM readUTFString list
  g_list_free (castPtr glistPtr)
  return result


-- | Lists the icons in the current icon theme. Only a subset of the icons can
-- be listed by providing a context string. The set of values for the context
-- string is system dependent, but will typically include such values as
-- \"Applications\" and \"MimeTypes\".
--
iconThemeListIcons :: (IconThemeClass self, GlibString string) => self
 -> (Maybe string) -- ^ @context@ a string identifying a particular type of icon, or 'Nothing' to list all icons.
 -> IO [string] -- ^ returns a String list
               -- holding the names of all the icons in the theme.
iconThemeListIcons self context =
  maybeWith withUTFString context $ \contextPtr -> do
  glistPtr <- (\(IconTheme arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_icon_theme_list_icons argPtr1 arg2)
{-# LINE 455 "./Graphics/UI/Gtk/General/IconTheme.chs" #-}
             (toIconTheme self)
             contextPtr
  list <- fromGList glistPtr
  result <- mapM readUTFString list
  g_list_free (castPtr glistPtr)
  return result


-- | Returns an list of integers describing the sizes at which the icon is
-- available without scaling. A size of -1 means that the icon is available in
-- a scalable format. The list is zero-terminated.
--
-- * Available since Gtk+ version 2.6
--
iconThemeGetIconSizes :: (IconThemeClass self, GlibString string) => self
 -> string -- ^ @iconName@ - the name of an icon
 -> IO [Int] -- ^ returns An newly allocated list describing the sizes at
            -- which the icon is available.
iconThemeGetIconSizes self iconName =
  withUTFString iconName $ \iconNamePtr -> do
  listPtr <- (\(IconTheme arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_icon_theme_get_icon_sizes argPtr1 arg2)
{-# LINE 476 "./Graphics/UI/Gtk/General/IconTheme.chs" #-}
              (toIconTheme self)
              iconNamePtr
  list <- peekArray 0 listPtr
  g_free (castPtr listPtr)
  return (map fromIntegral list)


-- | Gets the name of an icon that is representative of the current theme (for
-- instance, to use when presenting a list of themes to the user.)
--
iconThemeGetExampleIconName :: (IconThemeClass self, GlibString string) => self
 -> IO (Maybe string) -- ^ returns the name of an example icon or `Nothing'
iconThemeGetExampleIconName self = do
  namePtr <- (\(IconTheme arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_icon_theme_get_example_icon_name argPtr1) (toIconTheme self)
  if namePtr == nullPtr
     then return Nothing
     else liftM Just $ readUTFString namePtr

-- | Checks to see if the icon theme has changed; if it has, any currently
-- cached information is discarded and will be reloaded next time @iconTheme@
-- is accessed.
--
iconThemeRescanIfNeeded :: IconThemeClass self => self
 -> IO Bool -- ^ returns @True@ if the icon theme has changed and needed to be
            -- reloaded.
iconThemeRescanIfNeeded self =
  liftM toBool $
  (\(IconTheme arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_icon_theme_rescan_if_needed argPtr1)
{-# LINE 504 "./Graphics/UI/Gtk/General/IconTheme.chs" #-}
    (toIconTheme self)

-- | Registers a built-in icon for icon theme lookups. The idea of built-in
-- icons is to allow an application or library that uses themed icons to
-- function requiring files to be present in the file system. For instance, the
-- default images for all of Gtk+'s stock icons are registered as built-icons.
--
-- In general, if you use 'iconThemeAddBuiltinIcon' you should also install
-- the icon in the icon theme, so that the icon is generally available.
--
-- This function will generally be used with pixbufs loaded via
-- 'pixbufNewFromInline'.
--
iconThemeAddBuiltinIcon :: GlibString string =>
    string -- ^ @iconName@ - the name of the icon to register
 -> Int -- ^ @size@ - the size at which to register the icon (different
           -- images can be registered for the same icon name at different
           -- sizes.)
 -> Pixbuf -- ^ @pixbuf@ - 'Pixbuf' that contains the image to use for
           -- @iconName@.
 -> IO ()
iconThemeAddBuiltinIcon iconName size pixbuf =
  withUTFString iconName $ \iconNamePtr ->
  (\arg1 arg2 (Pixbuf arg3) -> withForeignPtr arg3 $ \argPtr3 ->gtk_icon_theme_add_builtin_icon arg1 arg2 argPtr3)
{-# LINE 528 "./Graphics/UI/Gtk/General/IconTheme.chs" #-}
    iconNamePtr
    (fromIntegral size)
    pixbuf

-- |
--
iconThemeErrorQuark :: IO Quark
iconThemeErrorQuark =
  gtk_icon_theme_error_quark
{-# LINE 537 "./Graphics/UI/Gtk/General/IconTheme.chs" #-}

--------------------
-- Types
newtype IconInfo = IconInfo (ForeignPtr (IconInfo))
{-# LINE 541 "./Graphics/UI/Gtk/General/IconTheme.chs" #-}

foreign import ccall unsafe "&gtk_icon_info_free"
  icon_info_free :: FinalizerPtr IconInfo

-- | Helper function for build 'IconInfo'
mkIconInfo :: Ptr IconInfo -> IO IconInfo
mkIconInfo infoPtr =
  liftM IconInfo $ newForeignPtr infoPtr icon_info_free

--------------------
-- Constructors


-- |
--
iconInfoNewForPixbuf :: IconThemeClass iconTheme => iconTheme -> Pixbuf -> IO IconInfo
iconInfoNewForPixbuf iconTheme pixbuf =
  (\(IconTheme arg1) (Pixbuf arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_icon_info_new_for_pixbuf argPtr1 argPtr2)
{-# LINE 559 "./Graphics/UI/Gtk/General/IconTheme.chs" #-}
          (toIconTheme iconTheme)
          pixbuf
  >>= mkIconInfo


--------------------
-- Methods

-- |
--
iconInfoCopy :: IconInfo -> IO IconInfo
iconInfoCopy self =
  (\(IconInfo arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_icon_info_copy argPtr1) self
  >>= mkIconInfo

-- | Fetches the set of attach points for an icon. An attach point is a location in the icon that can be
-- used as anchor points for attaching emblems or overlays to the icon.
iconInfoGetAttachPoints :: IconInfo -> IO (Maybe [Point])
iconInfoGetAttachPoints self =
  alloca $ \arrPtrPtr ->
  alloca $ \nPointsPtr -> do
  success <- liftM toBool $
            (\(IconInfo arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_icon_info_get_attach_points argPtr1 arg2 arg3)
{-# LINE 582 "./Graphics/UI/Gtk/General/IconTheme.chs" #-}
              self
              (castPtr arrPtrPtr)
              nPointsPtr
  if success
     then do
       arrPtr <- peek arrPtrPtr
       nPoints <- peek nPointsPtr
       pointList <- peekArray (fromIntegral nPoints) arrPtr
       g_free (castPtr arrPtr)
       return $ Just pointList
     else return Nothing

-- | Gets the base size for the icon. The base size is a size for the icon that was specified by the icon
-- theme creator. This may be different than the actual size of image; an example of this is small
-- emblem icons that can be attached to a larger icon. These icons will be given the same base size as
-- the larger icons to which they are attached.
--
iconInfoGetBaseSize :: IconInfo -> IO Int
iconInfoGetBaseSize self =
  liftM fromIntegral $
  (\(IconInfo arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_icon_info_get_base_size argPtr1) self

-- | Gets the built-in image for this icon, if any. To allow GTK+ to use built in icon images, you must
-- pass the ''IconLookupUseBuiltin'' to 'iconThemeLookupIcon'.
iconInfoGetBuiltinPixbuf :: IconInfo
 -> IO (Maybe Pixbuf) -- ^ returns the built-in image pixbuf, or 'Nothing'.
iconInfoGetBuiltinPixbuf self = do
  pixbufPtr <- (\(IconInfo arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_icon_info_get_builtin_pixbuf argPtr1) self
  if pixbufPtr == nullPtr
     then return Nothing
     else liftM Just $ makeNewGObject mkPixbuf (return pixbufPtr)

-- | Gets the display name for an icon. A display name is a string to be used in place of the icon name
-- in a user visible context like a list of icons.
iconInfoGetDisplayName :: GlibString string => IconInfo
 -> IO (Maybe string) -- ^ returns the display name for the icon or 'Nothing', if the icon doesn't have a specified display name.
iconInfoGetDisplayName self = do
  strPtr <- (\(IconInfo arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_icon_info_get_display_name argPtr1) self
  if strPtr == nullPtr
     then return Nothing
     else liftM Just $ peekUTFString strPtr

-- | Gets the coordinates of a rectangle within the icon that can be used for display of information such
-- as a preview of the contents of a text file. See 'iconInfoSetRawCoordinates' for further
-- information about the coordinate system.
iconInfoGetEmbeddedRect :: IconInfo
 -> IO (Maybe Rectangle) -- ^ @rectangle@ 'Rectangle' in which to store embedded
                         -- rectangle coordinates.
iconInfoGetEmbeddedRect self =
  alloca $ \rectPtr -> do
  success <- liftM toBool $
            (\(IconInfo arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_icon_info_get_embedded_rect argPtr1 arg2)
{-# LINE 634 "./Graphics/UI/Gtk/General/IconTheme.chs" #-}
            self
            (castPtr rectPtr)
  if success
     then liftM Just $ peek rectPtr
     else return Nothing

-- | Gets the filename for the icon. If the ''IconLookupUseBuiltin'' flag was passed to
-- 'iconThemeLookupIcon', there may be no filename if a builtin icon is returned; in this case,
-- you should use 'iconInfoGetBuiltinPixbuf'.
iconInfoGetFilename :: GlibString string => IconInfo
 -> IO (Maybe string) -- ^ returns the filename for the icon,
                     -- or 'Nothing' if 'iconInfoGetBuiltinPixbuf' should be used instead.
iconInfoGetFilename self = do
  namePtr <- (\(IconInfo arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_icon_info_get_filename argPtr1) self
  if namePtr == nullPtr
     then return Nothing
     else liftM Just $ peekUTFString namePtr

-- | Looks up an icon in an icon theme, scales it to the given size and renders it into a pixbuf. This is
-- a convenience function; if more details about the icon are needed, use 'iconThemeLookupIcon'
-- followed by 'iconInfoLoadIcon'.
--
-- Note that you probably want to listen for icon theme changes and update the icon. This is usually
-- done by connecting to the 'styleSet' signal. If for some reason you do not want to update
-- the icon when the icon theme changes, you should consider using 'pixbufCopy' to make a private
-- copy of the pixbuf returned by this function. Otherwise GTK+ may need to keep the old icon theme
-- loaded, which would be a waste of memory.
iconInfoLoadIcon :: IconInfo -> IO Pixbuf
iconInfoLoadIcon self =
  wrapNewGObject mkPixbuf $
  propagateGError $ \errorPtr ->
  (\(IconInfo arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_icon_info_load_icon argPtr1 arg2)
{-# LINE 666 "./Graphics/UI/Gtk/General/IconTheme.chs" #-}
    self
    errorPtr

-- | Sets whether the coordinates returned by 'iconInfoGetEmbeddedRect' and
-- 'iconInfoGetAttachPoints' should be returned in their original form as specified in the icon
-- theme, instead of scaled appropriately for the pixbuf returned by 'iconInfoLoadIcon'.
--
-- Raw coordinates are somewhat strange; they are specified to be with respect to the unscaled pixmap
-- for PNG and XPM icons, but for SVG icons, they are in a 1000x1000 coordinate space that is scaled to
-- the final size of the icon. You can determine if the icon is an SVG icon by using
-- 'iconInfoGetFilename', and seeing if it is non-'Nothing' and ends in '.svg'.
--
-- This function is provided primarily to allow compatibility wrappers for older API's, and is not
-- expected to be useful for applications.
iconInfoSetRawCoordinates :: IconInfo
 -> Bool -- ^ @rawCoordinates@ whether the coordinates of
         -- embedded rectangles and attached points should be returned in their original
 -> IO ()
iconInfoSetRawCoordinates self rawCoordinates =
  (\(IconInfo arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_icon_info_set_raw_coordinates argPtr1 arg2)
{-# LINE 686 "./Graphics/UI/Gtk/General/IconTheme.chs" #-}
    self
    (fromBool rawCoordinates)

--------------------
-- Signals

-- | Emitted when the current icon theme is switched or Gtk+ detects that a
-- change has occurred in the contents of the current icon theme.
--
iconThemeChanged :: IconThemeClass self => Signal self (IO ())
iconThemeChanged = Signal (connect_NONE__NONE "changed")

foreign import ccall safe "gtk_icon_theme_new"
  gtk_icon_theme_new :: (IO (Ptr IconTheme))

foreign import ccall safe "gtk_icon_theme_get_default"
  gtk_icon_theme_get_default :: (IO (Ptr IconTheme))

foreign import ccall safe "gtk_icon_theme_get_for_screen"
  gtk_icon_theme_get_for_screen :: ((Ptr Screen) -> (IO (Ptr IconTheme)))

foreign import ccall safe "gtk_icon_theme_set_screen"
  gtk_icon_theme_set_screen :: ((Ptr IconTheme) -> ((Ptr Screen) -> (IO ())))

foreign import ccall safe "gtk_icon_theme_set_search_path"
  gtk_icon_theme_set_search_path :: ((Ptr IconTheme) -> ((Ptr (Ptr CChar)) -> (CInt -> (IO ()))))

foreign import ccall safe "gtk_icon_theme_get_search_path"
  gtk_icon_theme_get_search_path :: ((Ptr IconTheme) -> ((Ptr (Ptr (Ptr CChar))) -> ((Ptr CInt) -> (IO ()))))

foreign import ccall safe "gtk_icon_theme_append_search_path"
  gtk_icon_theme_append_search_path :: ((Ptr IconTheme) -> ((Ptr CChar) -> (IO ())))

foreign import ccall safe "gtk_icon_theme_prepend_search_path"
  gtk_icon_theme_prepend_search_path :: ((Ptr IconTheme) -> ((Ptr CChar) -> (IO ())))

foreign import ccall safe "gtk_icon_theme_set_custom_theme"
  gtk_icon_theme_set_custom_theme :: ((Ptr IconTheme) -> ((Ptr CChar) -> (IO ())))

foreign import ccall safe "gtk_icon_theme_has_icon"
  gtk_icon_theme_has_icon :: ((Ptr IconTheme) -> ((Ptr CChar) -> (IO CInt)))

foreign import ccall safe "gtk_icon_theme_lookup_icon"
  gtk_icon_theme_lookup_icon :: ((Ptr IconTheme) -> ((Ptr CChar) -> (CInt -> (CInt -> (IO (Ptr ()))))))

foreign import ccall safe "gtk_icon_theme_choose_icon"
  gtk_icon_theme_choose_icon :: ((Ptr IconTheme) -> ((Ptr (Ptr CChar)) -> (CInt -> (CInt -> (IO (Ptr ()))))))

foreign import ccall safe "gtk_icon_theme_lookup_by_gicon"
  gtk_icon_theme_lookup_by_gicon :: ((Ptr IconTheme) -> ((Ptr Icon) -> (CInt -> (CInt -> (IO (Ptr ()))))))

foreign import ccall safe "gtk_icon_theme_load_icon"
  gtk_icon_theme_load_icon :: ((Ptr IconTheme) -> ((Ptr CChar) -> (CInt -> (CInt -> ((Ptr (Ptr ())) -> (IO (Ptr Pixbuf)))))))

foreign import ccall safe "gtk_icon_theme_list_contexts"
  gtk_icon_theme_list_contexts :: ((Ptr IconTheme) -> (IO (Ptr ())))

foreign import ccall unsafe "g_list_free"
  g_list_free :: ((Ptr ()) -> (IO ()))

foreign import ccall safe "gtk_icon_theme_list_icons"
  gtk_icon_theme_list_icons :: ((Ptr IconTheme) -> ((Ptr CChar) -> (IO (Ptr ()))))

foreign import ccall safe "gtk_icon_theme_get_icon_sizes"
  gtk_icon_theme_get_icon_sizes :: ((Ptr IconTheme) -> ((Ptr CChar) -> (IO (Ptr CInt))))

foreign import ccall unsafe "g_free"
  g_free :: ((Ptr ()) -> (IO ()))

foreign import ccall safe "gtk_icon_theme_get_example_icon_name"
  gtk_icon_theme_get_example_icon_name :: ((Ptr IconTheme) -> (IO (Ptr CChar)))

foreign import ccall safe "gtk_icon_theme_rescan_if_needed"
  gtk_icon_theme_rescan_if_needed :: ((Ptr IconTheme) -> (IO CInt))

foreign import ccall safe "gtk_icon_theme_add_builtin_icon"
  gtk_icon_theme_add_builtin_icon :: ((Ptr CChar) -> (CInt -> ((Ptr Pixbuf) -> (IO ()))))

foreign import ccall safe "gtk_icon_theme_error_quark"
  gtk_icon_theme_error_quark :: (IO CUInt)

foreign import ccall safe "gtk_icon_info_new_for_pixbuf"
  gtk_icon_info_new_for_pixbuf :: ((Ptr IconTheme) -> ((Ptr Pixbuf) -> (IO (Ptr IconInfo))))

foreign import ccall safe "gtk_icon_info_copy"
  gtk_icon_info_copy :: ((Ptr IconInfo) -> (IO (Ptr IconInfo)))

foreign import ccall safe "gtk_icon_info_get_attach_points"
  gtk_icon_info_get_attach_points :: ((Ptr IconInfo) -> ((Ptr (Ptr ())) -> ((Ptr CInt) -> (IO CInt))))

foreign import ccall safe "gtk_icon_info_get_base_size"
  gtk_icon_info_get_base_size :: ((Ptr IconInfo) -> (IO CInt))

foreign import ccall safe "gtk_icon_info_get_builtin_pixbuf"
  gtk_icon_info_get_builtin_pixbuf :: ((Ptr IconInfo) -> (IO (Ptr Pixbuf)))

foreign import ccall safe "gtk_icon_info_get_display_name"
  gtk_icon_info_get_display_name :: ((Ptr IconInfo) -> (IO (Ptr CChar)))

foreign import ccall safe "gtk_icon_info_get_embedded_rect"
  gtk_icon_info_get_embedded_rect :: ((Ptr IconInfo) -> ((Ptr ()) -> (IO CInt)))

foreign import ccall safe "gtk_icon_info_get_filename"
  gtk_icon_info_get_filename :: ((Ptr IconInfo) -> (IO (Ptr CChar)))

foreign import ccall safe "gtk_icon_info_load_icon"
  gtk_icon_info_load_icon :: ((Ptr IconInfo) -> ((Ptr (Ptr ())) -> (IO (Ptr Pixbuf))))

foreign import ccall safe "gtk_icon_info_set_raw_coordinates"
  gtk_icon_info_set_raw_coordinates :: ((Ptr IconInfo) -> (CInt -> (IO ())))
