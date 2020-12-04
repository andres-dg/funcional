
{-# LINE 2 "./Graphics/UI/Gtk/Windows/AboutDialog.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget AboutDialog
--
-- Author : Duncan Coutts
--
-- Created: 1 March 2005
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
-- Display information about an application
--
-- * Module available since Gtk+ version 2.6
--
module Graphics.UI.Gtk.Windows.AboutDialog (
-- * Detail
--
-- | The 'AboutDialog' offers a simple way to display information about a
-- program like its logo, name, copyright, website and license. It is also
-- possible to give credits to the authors, documenters, translators and
-- artists who have worked on the program. An about dialog is typically opened
-- when the user selects the @About@ option from the @Help@ menu. All parts of
-- the dialog are optional.
--
-- About dialog often contain links and email addresses. 'AboutDialog'
-- supports this by offering global hooks, which are called when the user
-- clicks on a link or email address, see 'aboutDialogSetEmailHook' and
-- 'aboutDialogSetUrlHook'. Email addresses in the authors, documenters and
-- artists properties are recognized by looking for @\<user\@host>@, URLs are
-- recognized by looking for @http:\/\/url@, with @url@ extending to the next
-- space, tab or line break.
-- Since 2.18 'AboutDialog' provides default website and email hooks that
-- use 'showURI'.
--
-- Note that Gtk+ sets a default title of @_(\"About %s\")@ on the dialog
-- window (where %s is replaced by the name of the application, but in order to
-- ensure proper translation of the title, applications should set the title
-- property explicitly when constructing a 'AboutDialog', as shown in the
-- following example:
--
-- Note that prior to Gtk+ 2.12, the 'aboutDialogProgramName' property was called
-- 'aboutDialogName'. Both names may be used in Gtk2Hs.

-- * Class Hierarchy
-- |
-- @
-- | 'GObject'
-- | +----'Object'
-- | +----'Widget'
-- | +----'Container'
-- | +----'Bin'
-- | +----'Window'
-- | +----'Dialog'
-- | +----AboutDialog
-- @


-- * Types
  AboutDialog,
  AboutDialogClass,
  castToAboutDialog, gTypeAboutDialog,
  toAboutDialog,

-- * Constructors
  aboutDialogNew,

-- * Methods

  aboutDialogSetEmailHook,
  aboutDialogSetUrlHook,


-- * Attributes
  aboutDialogProgramName,
  aboutDialogName,
  aboutDialogVersion,
  aboutDialogCopyright,
  aboutDialogComments,
  aboutDialogLicense,
  aboutDialogWebsite,
  aboutDialogWebsiteLabel,
  aboutDialogAuthors,
  aboutDialogDocumenters,
  aboutDialogArtists,
  aboutDialogTranslatorCredits,
  aboutDialogLogo,
  aboutDialogLogoIconName,

  aboutDialogWrapLicense,


-- * Deprecated

  aboutDialogGetName,
  aboutDialogSetName,
  aboutDialogGetVersion,
  aboutDialogSetVersion,
  aboutDialogGetCopyright,
  aboutDialogSetCopyright,
  aboutDialogGetComments,
  aboutDialogSetComments,
  aboutDialogGetLicense,
  aboutDialogSetLicense,
  aboutDialogGetWebsite,
  aboutDialogSetWebsite,
  aboutDialogGetWebsiteLabel,
  aboutDialogSetWebsiteLabel,
  aboutDialogSetAuthors,
  aboutDialogGetAuthors,
  aboutDialogSetArtists,
  aboutDialogGetArtists,
  aboutDialogSetDocumenters,
  aboutDialogGetDocumenters,
  aboutDialogGetTranslatorCredits,
  aboutDialogSetTranslatorCredits,
  aboutDialogGetLogo,
  aboutDialogSetLogo,
  aboutDialogGetLogoIconName,
  aboutDialogSetLogoIconName,

  aboutDialogGetWrapLicense,
  aboutDialogSetWrapLicense,



  ) where

import Control.Monad (liftM)
import Data.Maybe (fromMaybe)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.UI.Gtk.Types
{-# LINE 152 "./Graphics/UI/Gtk/Windows/AboutDialog.chs" #-}


{-# LINE 154 "./Graphics/UI/Gtk/Windows/AboutDialog.chs" #-}


--------------------
-- Constructors

-- | Creates a new 'AboutDialog'.
--
aboutDialogNew :: IO AboutDialog
aboutDialogNew =
  makeNewObject mkAboutDialog $
  liftM (castPtr :: Ptr Widget -> Ptr AboutDialog) $
  gtk_about_dialog_new
{-# LINE 166 "./Graphics/UI/Gtk/Windows/AboutDialog.chs" #-}

--------------------
-- Methods


-- | Returns the program name displayed in the about dialog.
--
aboutDialogGetName :: (AboutDialogClass self, GlibString string) => self
 -> IO string -- ^ returns The program name.
aboutDialogGetName self =

  (\(AboutDialog arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_about_dialog_get_program_name argPtr1)
{-# LINE 178 "./Graphics/UI/Gtk/Windows/AboutDialog.chs" #-}



    (toAboutDialog self)
  >>= peekUTFString

-- | Sets the name to display in the about dialog. If this is not set, it
-- defaults to the program executable name.
--
aboutDialogSetName :: (AboutDialogClass self, GlibString string) => self
 -> string -- ^ @name@ - the program name
 -> IO ()
aboutDialogSetName self name =
  withUTFString name $ \namePtr ->

  (\(AboutDialog arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_about_dialog_set_program_name argPtr1 arg2)
{-# LINE 194 "./Graphics/UI/Gtk/Windows/AboutDialog.chs" #-}



    (toAboutDialog self)
    namePtr

-- | Returns the version string.
--
aboutDialogGetVersion :: (AboutDialogClass self, GlibString string) => self -> IO string
aboutDialogGetVersion self =
  (\(AboutDialog arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_about_dialog_get_version argPtr1)
{-# LINE 205 "./Graphics/UI/Gtk/Windows/AboutDialog.chs" #-}
    (toAboutDialog self)
  >>= peekUTFString

-- | Sets the version string to display in the about dialog.
--
aboutDialogSetVersion :: (AboutDialogClass self, GlibString string) => self -> string -> IO ()
aboutDialogSetVersion self version =
  withUTFString version $ \versionPtr ->
  (\(AboutDialog arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_about_dialog_set_version argPtr1 arg2)
{-# LINE 214 "./Graphics/UI/Gtk/Windows/AboutDialog.chs" #-}
    (toAboutDialog self)
    versionPtr

-- | Returns the copyright string.
--
aboutDialogGetCopyright :: (AboutDialogClass self, GlibString string) => self -> IO string
aboutDialogGetCopyright self =
  (\(AboutDialog arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_about_dialog_get_copyright argPtr1)
{-# LINE 222 "./Graphics/UI/Gtk/Windows/AboutDialog.chs" #-}
    (toAboutDialog self)
  >>= peekUTFString

-- | Sets the copyright string to display in the about dialog. This should be
-- a short string of one or two lines.
--
aboutDialogSetCopyright :: (AboutDialogClass self, GlibString string) => self -> string -> IO ()
aboutDialogSetCopyright self copyright =
  withUTFString copyright $ \copyrightPtr ->
  (\(AboutDialog arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_about_dialog_set_copyright argPtr1 arg2)
{-# LINE 232 "./Graphics/UI/Gtk/Windows/AboutDialog.chs" #-}
    (toAboutDialog self)
    copyrightPtr

-- | Returns the comments string.
--
aboutDialogGetComments :: (AboutDialogClass self, GlibString string) => self -> IO string
aboutDialogGetComments self =
  (\(AboutDialog arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_about_dialog_get_comments argPtr1)
{-# LINE 240 "./Graphics/UI/Gtk/Windows/AboutDialog.chs" #-}
    (toAboutDialog self)
  >>= peekUTFString

-- | Sets the comments string to display in the about dialog. This should be a
-- short string of one or two lines.
--
aboutDialogSetComments :: (AboutDialogClass self, GlibString string) => self -> string -> IO ()
aboutDialogSetComments self comments =
  withUTFString comments $ \commentsPtr ->
  (\(AboutDialog arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_about_dialog_set_comments argPtr1 arg2)
{-# LINE 250 "./Graphics/UI/Gtk/Windows/AboutDialog.chs" #-}
    (toAboutDialog self)
    commentsPtr

-- | Returns the license information.
--
aboutDialogGetLicense :: (AboutDialogClass self, GlibString string) => self -> IO (Maybe string)
aboutDialogGetLicense self =
  (\(AboutDialog arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_about_dialog_get_license argPtr1)
{-# LINE 258 "./Graphics/UI/Gtk/Windows/AboutDialog.chs" #-}
    (toAboutDialog self)
  >>= maybePeek peekUTFString

-- | Sets the license information to be displayed in the secondary license
-- dialog. If @license@ is @Nothing@, the license button is hidden.
--
aboutDialogSetLicense :: (AboutDialogClass self, GlibString string) => self
 -> Maybe string -- ^ @license@ - the license information or @Nothing@
 -> IO ()
aboutDialogSetLicense self license =
  maybeWith withUTFString license $ \licensePtr ->
  (\(AboutDialog arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_about_dialog_set_license argPtr1 arg2)
{-# LINE 270 "./Graphics/UI/Gtk/Windows/AboutDialog.chs" #-}
    (toAboutDialog self)
    licensePtr

-- | Returns the website URL.
--
aboutDialogGetWebsite :: (AboutDialogClass self, GlibString string) => self -> IO string
aboutDialogGetWebsite self =
  (\(AboutDialog arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_about_dialog_get_website argPtr1)
{-# LINE 278 "./Graphics/UI/Gtk/Windows/AboutDialog.chs" #-}
    (toAboutDialog self)
  >>= peekUTFString

-- | Sets the URL to use for the website link.
--
aboutDialogSetWebsite :: (AboutDialogClass self, GlibString string) => self
 -> string -- ^ @website@ - a URL string starting with \"http:\/\/\"
 -> IO ()
aboutDialogSetWebsite self website =
  withUTFString website $ \websitePtr ->
  (\(AboutDialog arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_about_dialog_set_website argPtr1 arg2)
{-# LINE 289 "./Graphics/UI/Gtk/Windows/AboutDialog.chs" #-}
    (toAboutDialog self)
    websitePtr

-- | Returns the label used for the website link.
--
aboutDialogGetWebsiteLabel :: (AboutDialogClass self, GlibString string) => self -> IO string
aboutDialogGetWebsiteLabel self =
  (\(AboutDialog arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_about_dialog_get_website_label argPtr1)
{-# LINE 297 "./Graphics/UI/Gtk/Windows/AboutDialog.chs" #-}
    (toAboutDialog self)
  >>= peekUTFString

-- | Sets the label to be used for the website link. It defaults to the
-- website URL.
--
aboutDialogSetWebsiteLabel :: (AboutDialogClass self, GlibString string) => self -> string -> IO ()
aboutDialogSetWebsiteLabel self websiteLabel =
  withUTFString websiteLabel $ \websiteLabelPtr ->
  (\(AboutDialog arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_about_dialog_set_website_label argPtr1 arg2)
{-# LINE 307 "./Graphics/UI/Gtk/Windows/AboutDialog.chs" #-}
    (toAboutDialog self)
    websiteLabelPtr


-- | Sets the strings which are displayed in the authors tab of the secondary
-- credits dialog.
--
aboutDialogSetAuthors :: (AboutDialogClass self, GlibString string) => self
 -> [string] -- ^ @authors@ - a list of author names
 -> IO ()
aboutDialogSetAuthors self authors =
  withUTFStringArray0 authors $ \authorsPtr ->
  (\(AboutDialog arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_about_dialog_set_authors argPtr1 arg2)
{-# LINE 320 "./Graphics/UI/Gtk/Windows/AboutDialog.chs" #-}
    (toAboutDialog self)
    authorsPtr

-- | Returns the string which are displayed in the authors tab of the
-- secondary credits dialog.
--
aboutDialogGetAuthors :: (AboutDialogClass self, GlibString string) => self -> IO [string]
aboutDialogGetAuthors self =
  (\(AboutDialog arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_about_dialog_get_authors argPtr1)
{-# LINE 329 "./Graphics/UI/Gtk/Windows/AboutDialog.chs" #-}
    (toAboutDialog self)
  >>= peekUTFStringArray0

-- | Sets the strings which are displayed in the artists tab of the secondary
-- credits dialog.
--
aboutDialogSetArtists :: (AboutDialogClass self, GlibString string) => self
 -> [string] -- ^ @artists@ - a list of artist names
 -> IO ()
aboutDialogSetArtists self artists =
  withUTFStringArray0 artists $ \artistsPtr ->
  (\(AboutDialog arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_about_dialog_set_artists argPtr1 arg2)
{-# LINE 341 "./Graphics/UI/Gtk/Windows/AboutDialog.chs" #-}
    (toAboutDialog self)
    artistsPtr

-- | Returns the string which are displayed in the artists tab of the
-- secondary credits dialog.
--
aboutDialogGetArtists :: (AboutDialogClass self, GlibString string) => self -> IO [string]
aboutDialogGetArtists self =
  (\(AboutDialog arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_about_dialog_get_artists argPtr1)
{-# LINE 350 "./Graphics/UI/Gtk/Windows/AboutDialog.chs" #-}
    (toAboutDialog self)
  >>= peekUTFStringArray0

-- | Sets the strings which are displayed in the documenters tab of the
-- secondary credits dialog.
--
aboutDialogSetDocumenters :: (AboutDialogClass self, GlibString string) => self
 -> [string] -- ^ @artists@ - a list of documenter names
 -> IO ()
aboutDialogSetDocumenters self documenters =
  withUTFStringArray0 documenters $ \documentersPtr ->
  (\(AboutDialog arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_about_dialog_set_documenters argPtr1 arg2)
{-# LINE 362 "./Graphics/UI/Gtk/Windows/AboutDialog.chs" #-}
    (toAboutDialog self)
    documentersPtr

-- | Returns the string which are displayed in the documenters tab of the
-- secondary credits dialog.
--
aboutDialogGetDocumenters :: (AboutDialogClass self, GlibString string) => self -> IO [string]
aboutDialogGetDocumenters self =
  (\(AboutDialog arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_about_dialog_get_documenters argPtr1)
{-# LINE 371 "./Graphics/UI/Gtk/Windows/AboutDialog.chs" #-}
    (toAboutDialog self)
  >>= peekUTFStringArray0


-- | Returns the translator credits string which is displayed in the
-- translators tab of the secondary credits dialog.
--
aboutDialogGetTranslatorCredits :: (AboutDialogClass self, GlibString string) => self -> IO string
aboutDialogGetTranslatorCredits self =
  (\(AboutDialog arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_about_dialog_get_translator_credits argPtr1)
{-# LINE 381 "./Graphics/UI/Gtk/Windows/AboutDialog.chs" #-}
    (toAboutDialog self)
  >>= peekUTFString

-- | Sets the translator credits string which is displayed in the translators
-- tab of the secondary credits dialog.
--
-- The intended use for this string is to display the translator of the
-- language which is currently used in the user interface.
--
aboutDialogSetTranslatorCredits :: (AboutDialogClass self, GlibString string) => self -> string -> IO ()
aboutDialogSetTranslatorCredits self translatorCredits =
  withUTFString translatorCredits $ \translatorCreditsPtr ->
  (\(AboutDialog arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_about_dialog_set_translator_credits argPtr1 arg2)
{-# LINE 394 "./Graphics/UI/Gtk/Windows/AboutDialog.chs" #-}
    (toAboutDialog self)
    translatorCreditsPtr


-- | Returns the pixbuf displayed as logo in the about dialog.
--
aboutDialogGetLogo :: AboutDialogClass self => self -> IO Pixbuf
aboutDialogGetLogo self =
  makeNewGObject mkPixbuf $
  (\(AboutDialog arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_about_dialog_get_logo argPtr1)
{-# LINE 404 "./Graphics/UI/Gtk/Windows/AboutDialog.chs" #-}
    (toAboutDialog self)

-- | Sets the pixbuf to be displayed as logo in the about dialog. If it is
-- @Nothing@, the default window icon set with 'windowSetDefaultIcon' will be
-- used.
--
aboutDialogSetLogo :: AboutDialogClass self => self
 -> Maybe Pixbuf -- ^ @logo@ - a 'Pixbuf', or @Nothing@
 -> IO ()
aboutDialogSetLogo self logo =
  (\(AboutDialog arg1) (Pixbuf arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_about_dialog_set_logo argPtr1 argPtr2)
{-# LINE 415 "./Graphics/UI/Gtk/Windows/AboutDialog.chs" #-}
    (toAboutDialog self)
    (fromMaybe (Pixbuf nullForeignPtr) logo)

-- | Returns the icon name displayed as logo in the about dialog.
--
aboutDialogGetLogoIconName :: (AboutDialogClass self, GlibString string) => self -> IO string
aboutDialogGetLogoIconName self =
  (\(AboutDialog arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_about_dialog_get_logo_icon_name argPtr1)
{-# LINE 423 "./Graphics/UI/Gtk/Windows/AboutDialog.chs" #-}
    (toAboutDialog self)
  >>= peekUTFString

-- | Sets the pixbuf to be displayed as logo in the about dialog. If it is
-- @Nothing@, the default window icon set with 'windowSetDefaultIcon' will be
-- used.
--
aboutDialogSetLogoIconName :: (AboutDialogClass self, GlibString string) => self
 -> Maybe string -- ^ @iconName@ - an icon name, or @Nothing@
 -> IO ()
aboutDialogSetLogoIconName self iconName =
  maybeWith withUTFString iconName $ \iconNamePtr ->
  (\(AboutDialog arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_about_dialog_set_logo_icon_name argPtr1 arg2)
{-# LINE 436 "./Graphics/UI/Gtk/Windows/AboutDialog.chs" #-}
    (toAboutDialog self)
    iconNamePtr


-- | Installs a global function to be called whenever the user activates an
-- email link in an about dialog.
--
-- Removed in Gtk3.
aboutDialogSetEmailHook :: GlibString string
 => (string -> IO ()) -- ^ @(\url -> ...)@ - a function to call when an email
                      -- link is activated.
 -> IO ()
aboutDialogSetEmailHook func = do
  funcPtr <- mkAboutDialogActivateLinkFunc (\_ linkPtr _ -> do
    link <- peekUTFString linkPtr
    func link
    )
  gtk_about_dialog_set_email_hook
{-# LINE 454 "./Graphics/UI/Gtk/Windows/AboutDialog.chs" #-}
    funcPtr
    (castFunPtrToPtr funcPtr)
    destroyFunPtr
  return ()

-- | Installs a global function to be called whenever the user activates a URL
-- link in an about dialog.
--
-- Removed in Gtk3.
aboutDialogSetUrlHook ::GlibString string
 => (string -> IO ()) -- ^ @(\url -> ...)@ - a function to call when a URL link
                      -- is activated.
 -> IO ()
aboutDialogSetUrlHook func = do
  funcPtr <- mkAboutDialogActivateLinkFunc (\_ linkPtr _ -> do
    link <- peekUTFString linkPtr
    func link
    )
  gtk_about_dialog_set_url_hook
{-# LINE 473 "./Graphics/UI/Gtk/Windows/AboutDialog.chs" #-}
    funcPtr
    (castFunPtrToPtr funcPtr)
    destroyFunPtr
  return ()

type AboutDialogActivateLinkFunc = FunPtr (((Ptr AboutDialog) -> ((Ptr CChar) -> ((Ptr ()) -> (IO ())))))
{-# LINE 479 "./Graphics/UI/Gtk/Windows/AboutDialog.chs" #-}

foreign import ccall "wrapper" mkAboutDialogActivateLinkFunc ::
  (Ptr AboutDialog -> CString -> Ptr () -> IO ()) -> IO AboutDialogActivateLinkFunc




-- | Returns whether the license text in @about@ is automatically wrapped.
--
-- * Available since Gtk+ version 2.8
--
aboutDialogGetWrapLicense :: AboutDialogClass self => self
 -> IO Bool -- ^ returns @True@ if the license text is wrapped
aboutDialogGetWrapLicense self =
  liftM toBool $
  (\(AboutDialog arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_about_dialog_get_wrap_license argPtr1)
{-# LINE 495 "./Graphics/UI/Gtk/Windows/AboutDialog.chs" #-}
    (toAboutDialog self)

-- | Sets whether the license text in @about@ is automatically wrapped.
--
-- * Available since Gtk+ version 2.8
--
aboutDialogSetWrapLicense :: AboutDialogClass self => self
 -> Bool -- ^ @wrapLicense@ - whether to wrap the license
 -> IO ()
aboutDialogSetWrapLicense self wrapLicense =
  (\(AboutDialog arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_about_dialog_set_wrap_license argPtr1 arg2)
{-# LINE 506 "./Graphics/UI/Gtk/Windows/AboutDialog.chs" #-}
    (toAboutDialog self)
    (fromBool wrapLicense)



--------------------
-- Attributes

-- | The name of the program. If this is not set, it defaults to
-- 'gGetApplicationName'.
--
aboutDialogName :: (AboutDialogClass self, GlibString string) => Attr self string
aboutDialogName = newAttrFromStringProperty "name"

-- | The name of the program. If this is not set, it defaults to
-- 'gGetApplicationName'.
--

aboutDialogProgramName :: (AboutDialogClass self, GlibString string) => Attr self string
aboutDialogProgramName = newAttrFromStringProperty "program-name"





-- | The version of the program.
--
aboutDialogVersion :: (AboutDialogClass self, GlibString string) => Attr self string
aboutDialogVersion = newAttrFromStringProperty "version"

-- | Copyright information for the program.
--
aboutDialogCopyright :: (AboutDialogClass self, GlibString string) => Attr self string
aboutDialogCopyright = newAttrFromStringProperty "copyright"

-- | Comments about the program. This string is displayed in a label in the
-- main dialog, thus it should be a short explanation of the main purpose of
-- the program, not a detailed list of features.
--
aboutDialogComments :: (AboutDialogClass self, GlibString string) => Attr self string
aboutDialogComments = newAttrFromStringProperty "comments"

-- | The license of the program. This string is displayed in a text view in a
-- secondary dialog, therefore it is fine to use a long multi-paragraph text.
-- Note that the text is only wrapped in the text view if the 'aboutDialogWrapLicense'
-- property is set to @True@; otherwise the text itself must contain the
-- intended linebreaks.
--
-- Default value: @Nothing@
--
aboutDialogLicense :: (AboutDialogClass self, GlibString string) => Attr self (Maybe string)
aboutDialogLicense = newAttrFromMaybeStringProperty "license"

-- | The URL for the link to the website of the program. This should be a
-- string starting with \"http:\/\/.
--
aboutDialogWebsite :: (AboutDialogClass self, GlibString string) => Attr self string
aboutDialogWebsite = newAttrFromStringProperty "website"

-- | The label for the link to the website of the program. If this is not set,
-- it defaults to the URL specified in the website property.
--
aboutDialogWebsiteLabel :: (AboutDialogClass self, GlibString string) => Attr self string
aboutDialogWebsiteLabel = newAttrFromStringProperty "website-label"

-- | The authors of the program. Each string may
-- contain email addresses and URLs, which will be displayed as links, see the
-- introduction for more details.
--
aboutDialogAuthors :: (AboutDialogClass self, GlibString string) => Attr self [string]
aboutDialogAuthors = newAttr
  aboutDialogGetAuthors
  aboutDialogSetAuthors

-- | The people documenting the program.
-- Each string may contain email addresses and URLs, which will be displayed as
-- links, see the introduction for more details.
--
aboutDialogDocumenters :: (AboutDialogClass self, GlibString string) => Attr self [string]
aboutDialogDocumenters = newAttr
  aboutDialogGetDocumenters
  aboutDialogSetDocumenters

-- | The people who contributed artwork to the program.
-- Each string may contain email addresses and URLs, which will be
-- displayed as links, see the introduction for more details.
--
aboutDialogArtists :: (AboutDialogClass self, GlibString string) => Attr self [string]
aboutDialogArtists = newAttr
  aboutDialogGetArtists
  aboutDialogSetArtists

-- | Credits to the translators. This string should be marked as translatable.
-- The string may contain email addresses and URLs, which will be displayed as
-- links, see the introduction for more details.
--
aboutDialogTranslatorCredits :: (AboutDialogClass self, GlibString string) => Attr self string
aboutDialogTranslatorCredits = newAttrFromStringProperty "translator-credits"

-- | A logo for the about box. If this is not set, it defaults to
-- 'windowGetDefaultIconList'.
--
aboutDialogLogo :: AboutDialogClass self => ReadWriteAttr self Pixbuf (Maybe Pixbuf)
aboutDialogLogo = newAttr
  aboutDialogGetLogo
  aboutDialogSetLogo

-- | A named icon to use as the logo for the about box. This property
-- overrides the logo property.
--
-- Default value: @Nothing@
--
aboutDialogLogoIconName :: (AboutDialogClass self, GlibString string) => ReadWriteAttr self string (Maybe string)
aboutDialogLogoIconName = newAttr
  aboutDialogGetLogoIconName
  aboutDialogSetLogoIconName



-- | Whether to wrap the text in the license dialog.
--
-- Default value: @False@
--
aboutDialogWrapLicense :: AboutDialogClass self => Attr self Bool
aboutDialogWrapLicense = newAttrFromBoolProperty "wrap-license"

foreign import ccall safe "gtk_about_dialog_new"
  gtk_about_dialog_new :: (IO (Ptr Widget))

foreign import ccall safe "gtk_about_dialog_get_program_name"
  gtk_about_dialog_get_program_name :: ((Ptr AboutDialog) -> (IO (Ptr CChar)))

foreign import ccall safe "gtk_about_dialog_set_program_name"
  gtk_about_dialog_set_program_name :: ((Ptr AboutDialog) -> ((Ptr CChar) -> (IO ())))

foreign import ccall safe "gtk_about_dialog_get_version"
  gtk_about_dialog_get_version :: ((Ptr AboutDialog) -> (IO (Ptr CChar)))

foreign import ccall safe "gtk_about_dialog_set_version"
  gtk_about_dialog_set_version :: ((Ptr AboutDialog) -> ((Ptr CChar) -> (IO ())))

foreign import ccall safe "gtk_about_dialog_get_copyright"
  gtk_about_dialog_get_copyright :: ((Ptr AboutDialog) -> (IO (Ptr CChar)))

foreign import ccall safe "gtk_about_dialog_set_copyright"
  gtk_about_dialog_set_copyright :: ((Ptr AboutDialog) -> ((Ptr CChar) -> (IO ())))

foreign import ccall safe "gtk_about_dialog_get_comments"
  gtk_about_dialog_get_comments :: ((Ptr AboutDialog) -> (IO (Ptr CChar)))

foreign import ccall safe "gtk_about_dialog_set_comments"
  gtk_about_dialog_set_comments :: ((Ptr AboutDialog) -> ((Ptr CChar) -> (IO ())))

foreign import ccall safe "gtk_about_dialog_get_license"
  gtk_about_dialog_get_license :: ((Ptr AboutDialog) -> (IO (Ptr CChar)))

foreign import ccall safe "gtk_about_dialog_set_license"
  gtk_about_dialog_set_license :: ((Ptr AboutDialog) -> ((Ptr CChar) -> (IO ())))

foreign import ccall safe "gtk_about_dialog_get_website"
  gtk_about_dialog_get_website :: ((Ptr AboutDialog) -> (IO (Ptr CChar)))

foreign import ccall safe "gtk_about_dialog_set_website"
  gtk_about_dialog_set_website :: ((Ptr AboutDialog) -> ((Ptr CChar) -> (IO ())))

foreign import ccall safe "gtk_about_dialog_get_website_label"
  gtk_about_dialog_get_website_label :: ((Ptr AboutDialog) -> (IO (Ptr CChar)))

foreign import ccall safe "gtk_about_dialog_set_website_label"
  gtk_about_dialog_set_website_label :: ((Ptr AboutDialog) -> ((Ptr CChar) -> (IO ())))

foreign import ccall safe "gtk_about_dialog_set_authors"
  gtk_about_dialog_set_authors :: ((Ptr AboutDialog) -> ((Ptr (Ptr CChar)) -> (IO ())))

foreign import ccall safe "gtk_about_dialog_get_authors"
  gtk_about_dialog_get_authors :: ((Ptr AboutDialog) -> (IO (Ptr (Ptr CChar))))

foreign import ccall safe "gtk_about_dialog_set_artists"
  gtk_about_dialog_set_artists :: ((Ptr AboutDialog) -> ((Ptr (Ptr CChar)) -> (IO ())))

foreign import ccall safe "gtk_about_dialog_get_artists"
  gtk_about_dialog_get_artists :: ((Ptr AboutDialog) -> (IO (Ptr (Ptr CChar))))

foreign import ccall safe "gtk_about_dialog_set_documenters"
  gtk_about_dialog_set_documenters :: ((Ptr AboutDialog) -> ((Ptr (Ptr CChar)) -> (IO ())))

foreign import ccall safe "gtk_about_dialog_get_documenters"
  gtk_about_dialog_get_documenters :: ((Ptr AboutDialog) -> (IO (Ptr (Ptr CChar))))

foreign import ccall safe "gtk_about_dialog_get_translator_credits"
  gtk_about_dialog_get_translator_credits :: ((Ptr AboutDialog) -> (IO (Ptr CChar)))

foreign import ccall safe "gtk_about_dialog_set_translator_credits"
  gtk_about_dialog_set_translator_credits :: ((Ptr AboutDialog) -> ((Ptr CChar) -> (IO ())))

foreign import ccall safe "gtk_about_dialog_get_logo"
  gtk_about_dialog_get_logo :: ((Ptr AboutDialog) -> (IO (Ptr Pixbuf)))

foreign import ccall safe "gtk_about_dialog_set_logo"
  gtk_about_dialog_set_logo :: ((Ptr AboutDialog) -> ((Ptr Pixbuf) -> (IO ())))

foreign import ccall safe "gtk_about_dialog_get_logo_icon_name"
  gtk_about_dialog_get_logo_icon_name :: ((Ptr AboutDialog) -> (IO (Ptr CChar)))

foreign import ccall safe "gtk_about_dialog_set_logo_icon_name"
  gtk_about_dialog_set_logo_icon_name :: ((Ptr AboutDialog) -> ((Ptr CChar) -> (IO ())))

foreign import ccall safe "gtk_about_dialog_set_email_hook"
  gtk_about_dialog_set_email_hook :: ((FunPtr ((Ptr AboutDialog) -> ((Ptr CChar) -> ((Ptr ()) -> (IO ()))))) -> ((Ptr ()) -> ((FunPtr ((Ptr ()) -> (IO ()))) -> (IO (FunPtr ((Ptr AboutDialog) -> ((Ptr CChar) -> ((Ptr ()) -> (IO ())))))))))

foreign import ccall safe "gtk_about_dialog_set_url_hook"
  gtk_about_dialog_set_url_hook :: ((FunPtr ((Ptr AboutDialog) -> ((Ptr CChar) -> ((Ptr ()) -> (IO ()))))) -> ((Ptr ()) -> ((FunPtr ((Ptr ()) -> (IO ()))) -> (IO (FunPtr ((Ptr AboutDialog) -> ((Ptr CChar) -> ((Ptr ()) -> (IO ())))))))))

foreign import ccall safe "gtk_about_dialog_get_wrap_license"
  gtk_about_dialog_get_wrap_license :: ((Ptr AboutDialog) -> (IO CInt))

foreign import ccall safe "gtk_about_dialog_set_wrap_license"
  gtk_about_dialog_set_wrap_license :: ((Ptr AboutDialog) -> (CInt -> (IO ())))
