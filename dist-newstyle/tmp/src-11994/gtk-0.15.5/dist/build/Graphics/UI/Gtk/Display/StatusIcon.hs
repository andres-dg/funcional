
{-# LINE 2 "./Graphics/UI/Gtk/Display/StatusIcon.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget StatusIcon
--
-- Author : Andrea Vezzosi, Andy Stewart
--
-- Created: 19 July 2007
--
-- Copyright (C) 2007 Axel Simon
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
-- Display an icon in the system tray
--
-- * Module available since Gtk+ version 2.10
--
module Graphics.UI.Gtk.Display.StatusIcon (

-- * Detail
--
-- | The \"system tray\" or notification area is normally used for transient
-- icons that indicate some special state. For example, a system tray icon
-- might appear to tell the user that they have new mail, or have an incoming
-- instant message, or something along those lines. The basic idea is that
-- creating an icon in the notification area is less annoying than popping up a
-- dialog.
--
-- A 'StatusIcon' object can be used to display an icon in a \"system
-- tray\". The icon can have a tooltip, and the user can interact with it by
-- activating it or popping up a context menu. Critical information should not
-- solely be displayed in a 'StatusIcon', since it may not be visible (e.g.
-- when the user doesn't have a notification area on his panel). This can be
-- checked with 'statusIconIsEmbedded'.
--
-- On X11, the implementation follows the freedesktop.org \"System Tray\"
-- specification. Implementations of the \"tray\" side of this specification
-- can be found e.g. in the GNOME and KDE panel applications.
--
-- Note that a 'StatusIcon' is /not/ a widget, but just a 'GObject'. Making
-- it a widget would be impractical, since the system tray on Win32 doesn't
-- allow to embed arbitrary widgets.

-- * Class Hierarchy
--
-- |
-- @
-- | 'GObject'
-- | +----StatusIcon
-- @


-- * Types
  StatusIcon,
  StatusIconClass,
  castToStatusIcon, gTypeStatusIcon,
  toStatusIcon,

-- * Constructors
  statusIconNew,
  statusIconNewFromPixbuf,
  statusIconNewFromFile,
  statusIconNewFromStock,
  statusIconNewFromIconName,

-- * Methods
  statusIconSetFromPixbuf,
  statusIconSetFromFile,
  statusIconSetFromStock,
  statusIconSetFromIconName,
  statusIconGetStorageType,
  statusIconGetPixbuf,
  statusIconGetStock,
  statusIconGetIconName,
  statusIconGetSize,

  statusIconSetTooltip,

  statusIconSetVisible,
  statusIconGetVisible,

  statusIconSetBlinking,
  statusIconGetBlinking,

  statusIconIsEmbedded,
  statusIconPositionMenu,
  statusIconGetGeometry,

  statusIconSetScreen,
  statusIconGetScreen,


  statusIconSetTooltipText,
  statusIconGetTooltipText,
  statusIconSetTooltipMarkup,
  statusIconGetTooltipMarkup,
  statusIconSetHasTooltip,
  statusIconGetHasTooltip,


  statusIconSetTitle,
  statusIconGetTitle,


  statusIconSetName,


-- * Attributes
  statusIconPixbuf,
  statusIconFile,
  statusIconStock,
  statusIconIconName,
  statusIconStorageType,
  statusIconSize,

  statusIconBlinking,

  statusIconVisible,

  statusIconScreen,


  statusIconTooltipText,
  statusIconTooltipMarkup,
  statusIconHasTooltip,


  statusIconTitle,


-- * Signals
  statusIconSizeChanged,
  statusIconActivated,
  statusIconActivate,
  statusIconPopupMenu,

-- * Deprecated

  onActivate,
  afterActivate,
  onPopupMenu,
  afterPopupMenu,
  onSizeChanged,
  afterSizeChanged,


  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Types
{-# LINE 169 "./Graphics/UI/Gtk/Display/StatusIcon.chs" #-}
import Graphics.UI.Gtk.General.Enums
{-# LINE 170 "./Graphics/UI/Gtk/Display/StatusIcon.chs" #-}
import Graphics.UI.Gtk.General.Structs
import Graphics.UI.Gtk.Display.Image (ImageType)
import Graphics.UI.Gtk.Signals
{-# LINE 173 "./Graphics/UI/Gtk/Display/StatusIcon.chs" #-}
import Graphics.UI.Gtk.General.StockItems
import Graphics.UI.Gtk.Gdk.Events



{-# LINE 178 "./Graphics/UI/Gtk/Display/StatusIcon.chs" #-}


--------------------
-- Constructors

-- %hash c:2fb1 d:9bd6
-- | Creates an empty status icon object.
--
statusIconNew :: IO StatusIcon
statusIconNew =
  wrapNewGObject mkStatusIcon $
  gtk_status_icon_new
{-# LINE 190 "./Graphics/UI/Gtk/Display/StatusIcon.chs" #-}

-- %hash c:3318 d:cd70
-- | Creates a status icon displaying @pixbuf@.
--
-- The image will be scaled down to fit in the available space in the
-- notification area, if necessary.
--
statusIconNewFromPixbuf ::
    Pixbuf -- ^ @pixbuf@ - a 'Pixbuf'
 -> IO StatusIcon
statusIconNewFromPixbuf pixbuf =
  wrapNewGObject mkStatusIcon $
  (\(Pixbuf arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_status_icon_new_from_pixbuf argPtr1)
{-# LINE 203 "./Graphics/UI/Gtk/Display/StatusIcon.chs" #-}
    pixbuf

-- %hash c:325a d:6c24
-- | Creates a status icon displaying the file @filename@.
--
-- The image will be scaled down to fit in the available space in the
-- notification area, if necessary.
--
statusIconNewFromFile :: GlibString string
 => string -- ^ @filename@ - a filename
 -> IO StatusIcon
statusIconNewFromFile filename =
  wrapNewGObject mkStatusIcon $
  withUTFString filename $ \filenamePtr ->
  gtk_status_icon_new_from_file
{-# LINE 218 "./Graphics/UI/Gtk/Display/StatusIcon.chs" #-}
    filenamePtr

-- %hash c:784f d:88a3 | Creates a status icon displaying a stock
-- icon. Sample stock icon names are 'stockOpen', 'stockQuit'. You can
-- register your own stock icon names, see
-- 'Graphics.UI.Gtk.General.IconFactory.iconFactoryAddDefault' and
-- 'Graphics.UI.Gtk.General.IconFactory.iconFactoryAdd'.
--
statusIconNewFromStock ::
    StockId -- ^ @stockId@ - a stock icon id
 -> IO StatusIcon
statusIconNewFromStock stockId =
  wrapNewGObject mkStatusIcon $
  withUTFString stockId $ \stockIdPtr ->
  gtk_status_icon_new_from_stock
{-# LINE 233 "./Graphics/UI/Gtk/Display/StatusIcon.chs" #-}
    stockIdPtr

-- %hash c:6e1b d:8731
-- | Creates a status icon displaying an icon from the current icon theme. If
-- the current icon theme is changed, the icon will be updated appropriately.
--
statusIconNewFromIconName :: GlibString string
 => string -- ^ @iconName@ - an icon name
 -> IO StatusIcon
statusIconNewFromIconName iconName =
  wrapNewGObject mkStatusIcon $
  withUTFString iconName $ \iconNamePtr ->
  gtk_status_icon_new_from_icon_name
{-# LINE 246 "./Graphics/UI/Gtk/Display/StatusIcon.chs" #-}
    iconNamePtr

--------------------
-- Methods

-- %hash c:2256 d:12b2
-- | Makes @statusIcon@ display @pixbuf@. See 'statusIconNewFromPixbuf' for
-- details.
--
statusIconSetFromPixbuf :: StatusIconClass self => self
 -> Pixbuf -- ^ @pixbuf@ - a 'Pixbuf'
 -> IO ()
statusIconSetFromPixbuf self pixbuf =
  (\(StatusIcon arg1) (Pixbuf arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_status_icon_set_from_pixbuf argPtr1 argPtr2)
{-# LINE 260 "./Graphics/UI/Gtk/Display/StatusIcon.chs" #-}
    (toStatusIcon self)
    pixbuf

-- %hash c:c2c0 d:c0f8
-- | Makes @statusIcon@ display the file @filename@. See
-- 'statusIconNewFromFile' for details.
--
statusIconSetFromFile :: (StatusIconClass self, GlibString string) => self
 -> string -- ^ @filename@ - a filename
 -> IO ()
statusIconSetFromFile self filename =
  withUTFString filename $ \filenamePtr ->
  (\(StatusIcon arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_status_icon_set_from_file argPtr1 arg2)
{-# LINE 273 "./Graphics/UI/Gtk/Display/StatusIcon.chs" #-}
    (toStatusIcon self)
    filenamePtr

-- %hash c:d755 d:8fa3
-- | Makes @statusIcon@ display the stock icon with the id @stockId@. See
-- 'statusIconNewFromStock' for details.
--
statusIconSetFromStock :: StatusIconClass self => self
 -> StockId -- ^ @stockId@ - a stock icon id
 -> IO ()
statusIconSetFromStock self stockId =
  withUTFString stockId $ \stockIdPtr ->
  (\(StatusIcon arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_status_icon_set_from_stock argPtr1 arg2)
{-# LINE 286 "./Graphics/UI/Gtk/Display/StatusIcon.chs" #-}
    (toStatusIcon self)
    stockIdPtr

-- %hash c:b501 d:3ded
-- | Makes @statusIcon@ display the icon named @iconName@ from the current
-- icon theme. See 'statusIconNewFromIconName' for details.
--
statusIconSetFromIconName :: (StatusIconClass self, GlibString string) => self
 -> string -- ^ @iconName@ - an icon name
 -> IO ()
statusIconSetFromIconName self iconName =
  withUTFString iconName $ \iconNamePtr ->
  (\(StatusIcon arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_status_icon_set_from_icon_name argPtr1 arg2)
{-# LINE 299 "./Graphics/UI/Gtk/Display/StatusIcon.chs" #-}
    (toStatusIcon self)
    iconNamePtr

-- %hash c:6317 d:d3c5
-- | Gets the type of representation being used by the 'StatusIcon' to store
-- image data. If the 'StatusIcon' has no image data, the return value will be
-- 'Graphics.UI.Gtk.Display.Image.ImageEmpty'.
--
statusIconGetStorageType :: StatusIconClass self => self
 -> IO ImageType -- ^ returns the image representation being used
statusIconGetStorageType self =
  liftM (toEnum . fromIntegral) $
  (\(StatusIcon arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_status_icon_get_storage_type argPtr1)
{-# LINE 312 "./Graphics/UI/Gtk/Display/StatusIcon.chs" #-}
    (toStatusIcon self)

-- %hash c:cd8a d:9fed | Gets the 'Pixbuf' being displayed by the
-- 'StatusIcon'. The storage type of the status icon must be
-- 'Graphics.UI.Gtk.Display.Image.ImageEmpty' or
-- 'Graphics.UI.Gtk.Display.Image.ImagePixbuf' (see
-- 'statusIconGetStorageType'). The caller of this function does not
-- own a reference to the returned pixbuf.
--
statusIconGetPixbuf :: StatusIconClass self => self
 -> IO (Maybe Pixbuf) -- ^ returns the displayed pixbuf, or @Nothing@ if the
                      -- image is empty.
statusIconGetPixbuf self = do
  ptr <- (\(StatusIcon arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_status_icon_get_pixbuf argPtr1)
{-# LINE 326 "./Graphics/UI/Gtk/Display/StatusIcon.chs" #-}
       (toStatusIcon self)
  maybePeek (makeNewGObject mkPixbuf . return) ptr


-- %hash c:ecce d:448 | Gets the id of the stock icon being displayed
-- by the 'StatusIcon'. The storage type of the status icon must be
-- 'Graphics.UI.Gtk.Display.Image.ImageEmpty' or
-- 'Graphics.UI.Gtk.Display.Image.ImageStock' (see
-- 'statusIconGetStorageType'). The returned string is owned by the
-- 'StatusIcon' and should not be freed or modified.
--
statusIconGetStock :: StatusIconClass self => self
 -> IO (Maybe StockId) -- ^ returns stock id of the displayed stock icon, or @Nothing@
                      -- if the image is empty.
statusIconGetStock self =
  (\(StatusIcon arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_status_icon_get_stock argPtr1)
{-# LINE 342 "./Graphics/UI/Gtk/Display/StatusIcon.chs" #-}
    (toStatusIcon self)
  >>= maybePeek peekUTFString

-- %hash c:6e6b d:273e | Gets the name of the icon being displayed by
-- the 'StatusIcon'. The storage type of the status icon must be
-- 'Graphics.UI.Gtk.Display.Image.ImageEmpty' or
-- 'Graphics.UI.Gtk.Display.Image.ImageIconName' (see
-- 'statusIconGetStorageType'). The returned string is owned by the
-- 'StatusIcon' and should not be freed or modified.
--
statusIconGetIconName :: (StatusIconClass self, GlibString string) => self
 -> IO (Maybe string) -- ^ returns name of the displayed icon, or @Nothing@
                      -- if the image is empty.
statusIconGetIconName self =
  (\(StatusIcon arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_status_icon_get_icon_name argPtr1)
{-# LINE 357 "./Graphics/UI/Gtk/Display/StatusIcon.chs" #-}
    (toStatusIcon self)
  >>= maybePeek peekUTFString

-- %hash c:b1db d:b874
-- | Gets the size in pixels that is available for the image. Stock icons and
-- named icons adapt their size automatically if the size of the notification
-- area changes. For other storage types, the 'sizeChanged' signal can be used
-- to react to size changes.
--
statusIconGetSize :: StatusIconClass self => self
 -> IO Int -- ^ returns the size that is available for the image
statusIconGetSize self =
  liftM fromIntegral $
  (\(StatusIcon arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_status_icon_get_size argPtr1)
{-# LINE 371 "./Graphics/UI/Gtk/Display/StatusIcon.chs" #-}
    (toStatusIcon self)


-- %hash c:7644 d:d87b
-- | Sets the tooltip of the status icon.
--
-- Removed in Gtk3.
statusIconSetTooltip :: (StatusIconClass self, GlibString string) => self
 -> string -- ^ @tooltipText@ - the tooltip text
 -> IO ()
statusIconSetTooltip self tooltipText =
  withUTFString tooltipText $ \tooltipTextPtr ->
  (\(StatusIcon arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_status_icon_set_tooltip argPtr1 arg2)
{-# LINE 384 "./Graphics/UI/Gtk/Display/StatusIcon.chs" #-}
    (toStatusIcon self)
    tooltipTextPtr


-- %hash c:7bd8 d:74fd
-- | Shows or hides a status icon.
--
statusIconSetVisible :: StatusIconClass self => self
 -> Bool -- ^ @visible@ - @True@ to show the status icon, @False@ to hide it
 -> IO ()
statusIconSetVisible self visible =
  (\(StatusIcon arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_status_icon_set_visible argPtr1 arg2)
{-# LINE 396 "./Graphics/UI/Gtk/Display/StatusIcon.chs" #-}
    (toStatusIcon self)
    (fromBool visible)

-- %hash c:e90c d:6c0b
-- | Returns whether the status icon is visible or not. Note that being
-- visible does not guarantee that the user can actually see the icon, see also
-- 'statusIconIsEmbedded'.
--
statusIconGetVisible :: StatusIconClass self => self
 -> IO Bool -- ^ returns @True@ if the status icon is visible
statusIconGetVisible self =
  liftM toBool $
  (\(StatusIcon arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_status_icon_get_visible argPtr1)
{-# LINE 409 "./Graphics/UI/Gtk/Display/StatusIcon.chs" #-}
    (toStatusIcon self)


-- %hash c:aa47 d:3980
-- | Makes the status icon start or stop blinking. Note that blinking user
-- interface elements may be problematic for some users, and thus may be turned
-- off, in which case this setting has no effect.
--
-- Removed in Gtk3.
statusIconSetBlinking :: StatusIconClass self => self
 -> Bool -- ^ @blinking@ - @True@ to turn blinking on, @False@ to turn it off
 -> IO ()
statusIconSetBlinking self blinking =
  (\(StatusIcon arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_status_icon_set_blinking argPtr1 arg2)
{-# LINE 423 "./Graphics/UI/Gtk/Display/StatusIcon.chs" #-}
    (toStatusIcon self)
    (fromBool blinking)

-- %hash c:2168 d:3189
-- | Returns whether the icon is blinking, see 'statusIconSetBlinking'.
--
-- Removed in Gtk3.
statusIconGetBlinking :: StatusIconClass self => self
 -> IO Bool -- ^ returns @True@ if the icon is blinking
statusIconGetBlinking self =
  liftM toBool $
  (\(StatusIcon arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_status_icon_get_blinking argPtr1)
{-# LINE 435 "./Graphics/UI/Gtk/Display/StatusIcon.chs" #-}
    (toStatusIcon self)


-- %hash c:ffa d:8c83
-- | Returns whether the status icon is embedded in a notification area.
--
statusIconIsEmbedded :: StatusIconClass self => self
 -> IO Bool -- ^ returns @True@ if the status icon is embedded in a
            -- notification area.
statusIconIsEmbedded self =
  liftM toBool $
  (\(StatusIcon arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_status_icon_is_embedded argPtr1)
{-# LINE 447 "./Graphics/UI/Gtk/Display/StatusIcon.chs" #-}
    (toStatusIcon self)

-- %hash c:6a16 d:99ad
-- | Menu positioning function to use with 'menuPopup' to position @menu@
-- aligned to the status icon @userData@.
--
statusIconPositionMenu :: (MenuClass menu, StatusIconClass self) =>
    menu -- ^ @menu@ - the 'Menu'
 -> self -- ^ @userData@ - the status icon to position the
                           -- menu on
 -> IO (Int,Int,Bool) -- ^ @(x,y,pushIn)@ -
                           -- @(x,y)@ - coordinates.
                           -- @pushIn@ - whether the menu should be
                           -- pushed in to be completely inside the screen
                           -- instead of just clamped to the size to the
                           -- screen.
statusIconPositionMenu menu userData =
  alloca $ \xPtr ->
  alloca $ \yPtr ->
  alloca $ \pushInPtr -> do
  (\(Menu arg1) arg2 arg3 arg4 arg5 -> withForeignPtr arg1 $ \argPtr1 ->gtk_status_icon_position_menu argPtr1 arg2 arg3 arg4 arg5)
{-# LINE 468 "./Graphics/UI/Gtk/Display/StatusIcon.chs" #-}
    (toMenu menu)
    xPtr
    yPtr
    pushInPtr
    (castPtr . unsafeForeignPtrToPtr . unStatusIcon $ toStatusIcon userData)
  x <- peek xPtr
  y <- peek yPtr
  pushIn <- peek pushInPtr
  return (fromIntegral x, fromIntegral y, toBool pushIn)

-- FIXME: it's a partial binding, the potentially returned Screen is ignored
-- %hash c:7939 d:5ea
-- | Obtains information about the location of the status icon on screen. This
-- information can be used to e.g. position popups like notification bubbles.
--
-- Note that some platforms do not allow Gtk+ to provide this information,
-- and even on platforms that do allow it, the information is not reliable
-- unless the status icon is embedded in a notification area, see
-- 'statusIconIsEmbedded'.
--
statusIconGetGeometry :: StatusIconClass self => self -> IO (Maybe (Rectangle,Orientation))
statusIconGetGeometry self =
  alloca $ \recPtr ->
  alloca $ \orPtr ->
        (liftM toBool $ (\(StatusIcon arg1) arg2 arg3 arg4 -> withForeignPtr arg1 $ \argPtr1 ->gtk_status_icon_get_geometry argPtr1 arg2 arg3 arg4)
{-# LINE 493 "./Graphics/UI/Gtk/Display/StatusIcon.chs" #-}
         (toStatusIcon self) nullPtr (castPtr recPtr) orPtr) >>= \b ->
        if b
          then do
            rec_ <- peek recPtr
            or <- peek orPtr
            return $ Just (rec_,toEnum $ fromIntegral or)
        else return Nothing


-- | Sets the 'Screen' where status icon is displayed; if the icon is already
-- mapped, it will be unmapped, and then remapped on the new screen.
--
statusIconSetScreen :: (StatusIconClass self, ScreenClass screen) => self
 -> Maybe screen
 -> IO ()
statusIconSetScreen self screen =
  (\(StatusIcon arg1) (Screen arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_status_icon_set_screen argPtr1 argPtr2)
{-# LINE 510 "./Graphics/UI/Gtk/Display/StatusIcon.chs" #-}
    (toStatusIcon self)
    (maybe (Screen nullForeignPtr) toScreen screen)

-- | Returns the 'Screen' associated with the status icon.
--
statusIconGetScreen :: StatusIconClass self => self
 -> IO (Maybe Screen)
statusIconGetScreen self =
  maybeNull (makeNewGObject mkScreen) $
  (\(StatusIcon arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_status_icon_get_screen argPtr1)
{-# LINE 520 "./Graphics/UI/Gtk/Display/StatusIcon.chs" #-}
    (toStatusIcon self)



-- | Sets text as the contents of the tooltip.
--
-- This function will take care of setting "has-tooltip" to 'True' and of the default
-- handler for the "query-tooltip" signal.
--
-- See also the "tooltip-text" property and 'tooltipSetText'.
--
statusIconSetTooltipText :: (StatusIconClass self, GlibString string) => self
 -> Maybe string
 -> IO ()
statusIconSetTooltipText self text =
  maybeWith withUTFString text $ \textPtr ->
  (\(StatusIcon arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_status_icon_set_tooltip_text argPtr1 arg2)
{-# LINE 537 "./Graphics/UI/Gtk/Display/StatusIcon.chs" #-}
    (toStatusIcon self)
    textPtr

-- | Gets the contents of the tooltip for status icon.
--
statusIconGetTooltipText :: (StatusIconClass self, GlibString string) => self
 -> IO (Maybe string)
statusIconGetTooltipText self =
  (\(StatusIcon arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_status_icon_get_tooltip_text argPtr1)
{-# LINE 546 "./Graphics/UI/Gtk/Display/StatusIcon.chs" #-}
    (toStatusIcon self)
  >>= maybePeek peekUTFString

-- | Sets markup as the contents of the tooltip, which is marked up with the
-- Pango text markup language.
--
-- This function will take care of setting 'statusIconHasTooltip' to 'True' and of the default
-- handler for the 'queryTooltip' signal.
--
-- See also the 'tooltipMarkup' property and 'tooltipSetMarkup'.
--
statusIconSetTooltipMarkup :: (StatusIconClass self, GlibString string) => self
 -> Maybe string
 -> IO ()
statusIconSetTooltipMarkup self markup =
  maybeWith withUTFString markup $ \markupPtr ->
  (\(StatusIcon arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_status_icon_set_tooltip_markup argPtr1 arg2)
{-# LINE 563 "./Graphics/UI/Gtk/Display/StatusIcon.chs" #-}
    (toStatusIcon self)
    markupPtr

-- | Gets the contents of the tooltip for status icon.
--
statusIconGetTooltipMarkup :: (StatusIconClass self, GlibString string) => self
 -> IO (Maybe string)
statusIconGetTooltipMarkup self =
  (\(StatusIcon arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_status_icon_get_tooltip_markup argPtr1)
{-# LINE 572 "./Graphics/UI/Gtk/Display/StatusIcon.chs" #-}
    (toStatusIcon self)
  >>= maybePeek peekUTFString

-- | Sets the has-tooltip property on the status icon to @hasTooltip@.
-- See 'statusIconHasTooltip' for more information.
--
statusIconSetHasTooltip :: StatusIconClass self => self
 -> Bool
 -> IO ()
statusIconSetHasTooltip self hasTooltip =
  (\(StatusIcon arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_status_icon_set_has_tooltip argPtr1 arg2)
{-# LINE 583 "./Graphics/UI/Gtk/Display/StatusIcon.chs" #-}
    (toStatusIcon self)
    (fromBool hasTooltip)

-- | Returns the current value of the has-tooltip property. See 'statusIconHasTooltip' for more information.
--
statusIconGetHasTooltip :: StatusIconClass self => self
 -> IO Bool
statusIconGetHasTooltip self =
  liftM toBool $
  (\(StatusIcon arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_status_icon_get_has_tooltip argPtr1)
{-# LINE 593 "./Graphics/UI/Gtk/Display/StatusIcon.chs" #-}
    (toStatusIcon self)



-- | Sets the title of this tray icon. This should be a short, human-readable, localized
-- string describing the tray icon. It may be used by tools like screen readers to
-- render the tray icon.
--
statusIconSetTitle :: (StatusIconClass self, GlibString string) => self
 -> Maybe string
 -> IO ()
statusIconSetTitle self title =
  maybeWith withUTFString title $ \titlePtr ->
  (\(StatusIcon arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_status_icon_set_title argPtr1 arg2)
{-# LINE 607 "./Graphics/UI/Gtk/Display/StatusIcon.chs" #-}
    (toStatusIcon self)
    titlePtr

-- | Gets the title of this tray icon. See 'statusIconSetTitle'.
--
statusIconGetTitle :: (StatusIconClass self, GlibString string) => self
 -> IO (Maybe string)
statusIconGetTitle self =
  (\(StatusIcon arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_status_icon_get_title argPtr1)
{-# LINE 616 "./Graphics/UI/Gtk/Display/StatusIcon.chs" #-}
    (toStatusIcon self)
  >>= maybePeek peekUTFString



-- | Sets the name of this tray icon. This should be a string identifying this icon. It is may be used
-- for sorting the icons in the tray and will not be shown to the user.
statusIconSetName :: (StatusIconClass self, GlibString string) => self -> string -> IO ()
statusIconSetName self name =
  withUTFString name $ \ namePtr ->
  (\(StatusIcon arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_status_icon_set_name argPtr1 arg2)
{-# LINE 627 "./Graphics/UI/Gtk/Display/StatusIcon.chs" #-}
    (toStatusIcon self)
    namePtr


--------------------
-- Attributes

-- %hash c:575d d:54e3
-- | A 'Pixbuf' to display.
--
statusIconPixbuf :: StatusIconClass self => Attr self Pixbuf
statusIconPixbuf = newAttrFromObjectProperty "pixbuf"
                     gdk_pixbuf_get_type
{-# LINE 640 "./Graphics/UI/Gtk/Display/StatusIcon.chs" #-}

-- %hash c:6783 d:d235
-- | Filename to load and display.
--
-- Default value: @Nothing@
--
statusIconFile :: (StatusIconClass self, GlibString string) => WriteAttr self (Maybe string)
statusIconFile = writeAttrFromMaybeStringProperty "file"

-- %hash c:3fc3 d:7ec1
-- | Stock ID for a stock image to display.
--
-- Default value: @Nothing@
--
statusIconStock :: (StatusIconClass self, GlibString string) => Attr self (Maybe string)
statusIconStock = newAttrFromMaybeStringProperty "stock"

-- %hash c:3502 d:9b7a
-- | The name of the icon from the icon theme.
--
-- Default value: @Nothing@
--
statusIconIconName :: (StatusIconClass self, GlibString string) => Attr self (Maybe string)
statusIconIconName = newAttrFromMaybeStringProperty "icon-name"

-- %hash c:570e d:983f
-- | The representation being used for image data.
--
-- Default value: 'Graphics.UI.Gtk.Display.Image.ImageEmpty'
--
statusIconStorageType :: StatusIconClass self => ReadAttr self ImageType
statusIconStorageType = readAttrFromEnumProperty "storage-type"
                          gtk_image_type_get_type
{-# LINE 673 "./Graphics/UI/Gtk/Display/StatusIcon.chs" #-}

-- %hash c:10be d:4621
-- | The size of the icon.
--
-- Allowed values: >= 0
--
-- Default value: 0
--
statusIconSize :: StatusIconClass self => ReadAttr self Int
statusIconSize = readAttrFromIntProperty "size"


-- %hash c:eb d:655d
-- | Whether or not the status icon is blinking.
--
-- Default value: @False@
--
-- Removed in Gtk3.
statusIconBlinking :: StatusIconClass self => Attr self Bool
statusIconBlinking = newAttrFromBoolProperty "blinking"


-- %hash c:4e2b d:7712
-- | Whether or not the status icon is visible.
--
-- Default value: @True@
--
statusIconVisible :: StatusIconClass self => Attr self Bool
statusIconVisible = newAttrFromBoolProperty "visible"


-- | The screen where this status icon will be displayed.
statusIconScreen :: StatusIconClass self => Attr self Screen
statusIconScreen = newAttrFromObjectProperty "screen"
                          gdk_screen_get_type
{-# LINE 708 "./Graphics/UI/Gtk/Display/StatusIcon.chs" #-}



-- | Sets the text of tooltip to be the given string.
--
-- Also see 'tooltipSetText'.
--
-- This is a convenience property which will take care of getting the tooltip
-- shown if the given value is not 'Nothing'. "has-tooltip" will automatically
-- be set to 'True' and the default handler for the "query-tooltip" signal will
-- take care of displaying the tooltip.
--
-- Note that some platforms have limitations on the length of tooltips that
-- they allow on status icons, e.g. Windows only shows the first 64 characters.
--
-- Default value: 'Nothing'
statusIconTooltipText :: (StatusIconClass self, GlibString string) => Attr self (Maybe string)
statusIconTooltipText = newAttrFromMaybeStringProperty "tooltip-text"

-- | Sets the text of tooltip to be the given string, which is marked up with the
-- Pango text markup language. Also see 'tooltipSetMarkup'.
--
-- This is a convenience property which will take care of getting the tooltip
-- shown if the given value is not 'Nothing'. "has-tooltip" will automatically
-- be set to 'True' and the default handler for the "query-tooltip" signal will
-- take care of displaying the tooltip.
--
-- On some platforms, embedded markup will be ignored.
--
-- Default value: 'Nothing'
statusIconTooltipMarkup :: (StatusIconClass self, GlibString string) => Attr self (Maybe string)
statusIconTooltipMarkup = newAttrFromMaybeStringProperty "tooltip-markup"

-- | Enables or disables the emission of "query-tooltip" on status_icon. A value
-- of 'True' indicates that status_icon can have a tooltip, in this case the status
-- icon will be queried using "query-tooltip" to determine whether it will provide
-- a tooltip or not.
--
-- Note that setting this property to 'True' for the first time will change the
-- event masks of the windows of this status icon to include leave-notify and
-- motion-notify events. This will not be undone when the property is set to
-- 'False' again.
--
-- Whether this property is respected is platform dependent. For plain text
-- tooltips, use "tooltip-text" in preference.
--
-- Default value: 'False'
statusIconHasTooltip :: StatusIconClass self => Attr self Bool
statusIconHasTooltip = newAttrFromBoolProperty "has-tooltip"



-- | The title of this tray icon. This should be a short, human-readable,
-- localized string describing the tray icon. It may be used by tools
-- like screen readers to render the tray icon.
--
-- Default value: 'Nothing'
statusIconTitle :: (StatusIconClass self, GlibString string) => Attr self (Maybe string)
statusIconTitle = newAttrFromMaybeStringProperty "title"


--------------------
-- Signals

-- %hash c:969a d:71d0
-- | Gets emitted when the size available for the image changes, e.g. because
-- the notification area got resized.
--
statusIconSizeChanged :: StatusIconClass self => Signal self (Int -> IO Bool)
statusIconSizeChanged = Signal (connect_INT__BOOL "size-changed")

-- | Gets emitted when the user activates the status icon.
-- If and how status icons can activated is platform-dependent.
statusIconActivated :: StatusIconClass self => Signal self (IO ())
statusIconActivated = Signal (connect_NONE__NONE "activate")

-- | Deprecated. See 'statusIconActivated'.
statusIconActivate :: StatusIconClass self => Signal self (IO ())
statusIconActivate = statusIconActivated

-- | Gets emitted when the user brings up the context menu
-- of the status icon. Whether status icons can have context
-- menus and how these are activated is platform-dependent.
--
-- The 'MouseButton' and 'TimeStamp' parameters should be
-- passed as the last to arguments to 'Graphics.UI.Gtk.menuPopup'.
statusIconPopupMenu :: StatusIconClass self => Signal self (Maybe MouseButton -> TimeStamp -> IO ())
statusIconPopupMenu = Signal wrap

wrap flag self f = connect_WORD_WORD__NONE "popup_menu" flag self (\m t -> f (toMB m) (fromIntegral t))
    where toMB 0 = Nothing
          toMB n = Just . toEnum . fromIntegral $ n

--------------------
-- Deprecated Signals


-- %hash c:bd32
onActivate :: StatusIconClass self => self
 -> IO ()
 -> IO (ConnectId self)
onActivate = connect_NONE__NONE "activate" False
{-# DEPRECATED onActivate "instead of 'onActivate obj' use 'on obj activate'" #-}

-- %hash c:a571
afterActivate :: StatusIconClass self => self
 -> IO ()
 -> IO (ConnectId self)
afterActivate = connect_NONE__NONE "activate" True
{-# DEPRECATED afterActivate "instead of 'afterActivate obj' use 'after obj activate'" #-}

-- %hash c:44a5
onPopupMenu :: StatusIconClass self => self
 -> (Maybe MouseButton -> TimeStamp -> IO ())
 -> IO (ConnectId self)
onPopupMenu = wrap False
{-# DEPRECATED onPopupMenu "instead of 'onPopupMenu obj' use 'on obj popupMenu'" #-}

-- %hash c:1904
afterPopupMenu :: StatusIconClass self => self
 -> (Maybe MouseButton -> TimeStamp -> IO ())
 -> IO (ConnectId self)
afterPopupMenu = wrap True
{-# DEPRECATED afterPopupMenu "instead of 'afterPopupMenu obj' use 'after obj popupMenu'" #-}

-- %hash c:e226
onSizeChanged :: StatusIconClass self => self
 -> (Int -> IO Bool)
 -> IO (ConnectId self)
onSizeChanged = connect_INT__BOOL "size_changed" False
{-# DEPRECATED onSizeChanged "instead of 'onSizeChanged obj' use 'on obj sizeChanged'" #-}

-- %hash c:ec65
afterSizeChanged :: StatusIconClass self => self
 -> (Int -> IO Bool)
 -> IO (ConnectId self)
afterSizeChanged = connect_INT__BOOL "size_changed" True
{-# DEPRECATED afterSizeChanged "instead of 'afterSizeChanged obj' use 'after obj sizeChanged'" #-}

foreign import ccall safe "gtk_status_icon_new"
  gtk_status_icon_new :: (IO (Ptr StatusIcon))

foreign import ccall safe "gtk_status_icon_new_from_pixbuf"
  gtk_status_icon_new_from_pixbuf :: ((Ptr Pixbuf) -> (IO (Ptr StatusIcon)))

foreign import ccall safe "gtk_status_icon_new_from_file"
  gtk_status_icon_new_from_file :: ((Ptr CChar) -> (IO (Ptr StatusIcon)))

foreign import ccall safe "gtk_status_icon_new_from_stock"
  gtk_status_icon_new_from_stock :: ((Ptr CChar) -> (IO (Ptr StatusIcon)))

foreign import ccall safe "gtk_status_icon_new_from_icon_name"
  gtk_status_icon_new_from_icon_name :: ((Ptr CChar) -> (IO (Ptr StatusIcon)))

foreign import ccall safe "gtk_status_icon_set_from_pixbuf"
  gtk_status_icon_set_from_pixbuf :: ((Ptr StatusIcon) -> ((Ptr Pixbuf) -> (IO ())))

foreign import ccall safe "gtk_status_icon_set_from_file"
  gtk_status_icon_set_from_file :: ((Ptr StatusIcon) -> ((Ptr CChar) -> (IO ())))

foreign import ccall safe "gtk_status_icon_set_from_stock"
  gtk_status_icon_set_from_stock :: ((Ptr StatusIcon) -> ((Ptr CChar) -> (IO ())))

foreign import ccall safe "gtk_status_icon_set_from_icon_name"
  gtk_status_icon_set_from_icon_name :: ((Ptr StatusIcon) -> ((Ptr CChar) -> (IO ())))

foreign import ccall safe "gtk_status_icon_get_storage_type"
  gtk_status_icon_get_storage_type :: ((Ptr StatusIcon) -> (IO CInt))

foreign import ccall safe "gtk_status_icon_get_pixbuf"
  gtk_status_icon_get_pixbuf :: ((Ptr StatusIcon) -> (IO (Ptr Pixbuf)))

foreign import ccall safe "gtk_status_icon_get_stock"
  gtk_status_icon_get_stock :: ((Ptr StatusIcon) -> (IO (Ptr CChar)))

foreign import ccall safe "gtk_status_icon_get_icon_name"
  gtk_status_icon_get_icon_name :: ((Ptr StatusIcon) -> (IO (Ptr CChar)))

foreign import ccall safe "gtk_status_icon_get_size"
  gtk_status_icon_get_size :: ((Ptr StatusIcon) -> (IO CInt))

foreign import ccall safe "gtk_status_icon_set_tooltip"
  gtk_status_icon_set_tooltip :: ((Ptr StatusIcon) -> ((Ptr CChar) -> (IO ())))

foreign import ccall safe "gtk_status_icon_set_visible"
  gtk_status_icon_set_visible :: ((Ptr StatusIcon) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_status_icon_get_visible"
  gtk_status_icon_get_visible :: ((Ptr StatusIcon) -> (IO CInt))

foreign import ccall safe "gtk_status_icon_set_blinking"
  gtk_status_icon_set_blinking :: ((Ptr StatusIcon) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_status_icon_get_blinking"
  gtk_status_icon_get_blinking :: ((Ptr StatusIcon) -> (IO CInt))

foreign import ccall safe "gtk_status_icon_is_embedded"
  gtk_status_icon_is_embedded :: ((Ptr StatusIcon) -> (IO CInt))

foreign import ccall safe "gtk_status_icon_position_menu"
  gtk_status_icon_position_menu :: ((Ptr Menu) -> ((Ptr CInt) -> ((Ptr CInt) -> ((Ptr CInt) -> ((Ptr ()) -> (IO ()))))))

foreign import ccall safe "gtk_status_icon_get_geometry"
  gtk_status_icon_get_geometry :: ((Ptr StatusIcon) -> ((Ptr Screen) -> ((Ptr ()) -> ((Ptr CInt) -> (IO CInt)))))

foreign import ccall safe "gtk_status_icon_set_screen"
  gtk_status_icon_set_screen :: ((Ptr StatusIcon) -> ((Ptr Screen) -> (IO ())))

foreign import ccall safe "gtk_status_icon_get_screen"
  gtk_status_icon_get_screen :: ((Ptr StatusIcon) -> (IO (Ptr Screen)))

foreign import ccall safe "gtk_status_icon_set_tooltip_text"
  gtk_status_icon_set_tooltip_text :: ((Ptr StatusIcon) -> ((Ptr CChar) -> (IO ())))

foreign import ccall safe "gtk_status_icon_get_tooltip_text"
  gtk_status_icon_get_tooltip_text :: ((Ptr StatusIcon) -> (IO (Ptr CChar)))

foreign import ccall safe "gtk_status_icon_set_tooltip_markup"
  gtk_status_icon_set_tooltip_markup :: ((Ptr StatusIcon) -> ((Ptr CChar) -> (IO ())))

foreign import ccall safe "gtk_status_icon_get_tooltip_markup"
  gtk_status_icon_get_tooltip_markup :: ((Ptr StatusIcon) -> (IO (Ptr CChar)))

foreign import ccall safe "gtk_status_icon_set_has_tooltip"
  gtk_status_icon_set_has_tooltip :: ((Ptr StatusIcon) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_status_icon_get_has_tooltip"
  gtk_status_icon_get_has_tooltip :: ((Ptr StatusIcon) -> (IO CInt))

foreign import ccall safe "gtk_status_icon_set_title"
  gtk_status_icon_set_title :: ((Ptr StatusIcon) -> ((Ptr CChar) -> (IO ())))

foreign import ccall safe "gtk_status_icon_get_title"
  gtk_status_icon_get_title :: ((Ptr StatusIcon) -> (IO (Ptr CChar)))

foreign import ccall safe "gtk_status_icon_set_name"
  gtk_status_icon_set_name :: ((Ptr StatusIcon) -> ((Ptr CChar) -> (IO ())))

foreign import ccall unsafe "gdk_pixbuf_get_type"
  gdk_pixbuf_get_type :: CULong

foreign import ccall unsafe "gtk_image_type_get_type"
  gtk_image_type_get_type :: CULong

foreign import ccall unsafe "gdk_screen_get_type"
  gdk_screen_get_type :: CULong
