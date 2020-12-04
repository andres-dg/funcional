{-# LINE 1 "Graphics/UI/Gtk/General/StockItems.hsc" #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
-- -*-haskell-*-




--  GIMP Toolkit (GTK) StockItems
--
--  Author : Axel Simon
--
--  Created: 24 May 2001
--
--  Copyright (C) 1999-2005 Axel Simon
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public
--  License as published by the Free Software Foundation; either
--  version 2.1 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- A StockItem is a resource that is know throughout Gtk.
--
-- * Defining you own 'Graphics.UI.Gtk.General.IconFactory.IconSet's
--   as 'StockItem's will make it possible for Gtk to choose the most
--   appropriate sizes and enables themes to override your built in
--   icons. A couple of constants are defined here as well. They are
--   useful in accessing Gtk's predefined items.
--

module Graphics.UI.Gtk.General.StockItems (
  StockItem(StockItem),
  StockId,
  siStockId,
  siLabel,
  siModifier,
  siKeyval,
  siTransDom,
  stockAddItem,
  stockLookupItem,
  stockListIds,
  stockAbout,
  stockAdd,
  stockApply,
  stockBold,
  stockCancel,

{-# LINE 57 "Graphics/UI/Gtk/General/StockItems.hsc" #-}
  stockCapsLockWarning,

{-# LINE 59 "Graphics/UI/Gtk/General/StockItems.hsc" #-}
  stockCDROM,
  stockClear,
  stockClose,
  stockColorPicker,
  stockConvert,
  stockConnect,
  stockCopy,
  stockCut,
  stockDelete,
  stockDialogAuthentication,
  stockDialogError,
  stockDialogInfo,
  stockDialogQuestion,
  stockDialogWarning,
  stockDirectory,

{-# LINE 75 "Graphics/UI/Gtk/General/StockItems.hsc" #-}
  stockDiscard,

{-# LINE 77 "Graphics/UI/Gtk/General/StockItems.hsc" #-}
  stockDisconnect,
  stockDnd,
  stockDndMultiple,
  stockEdit,
  stockExecute,
  stockFile,
  stockFind,
  stockFindAndRelpace,
  stockFloppy,
  stockFullscreen,
  stockGotoBottom,
  stockGotoFirst,
  stockGotoLast,
  stockGotoTop,
  stockGoBack,
  stockGoDown,
  stockGoForward,
  stockGoUp,
  stockHarddisk,
  stockHelp,
  stockHome,
  stockIndent,
  stockIndex,
  stockInfo,
  stockItalic,
  stockJumpTo,
  stockJustifyCenter,
  stockJustifyFill,
  stockJustifyLeft,
  stockJustifyRight,
  stockLeaveFullscreen,
  stockMediaForward,
  stockMediaNext,
  stockMediaPause,
  stockMediaPlay,
  stockMediaPrevious,
  stockMediaRecord,
  stockMediaRewind,
  stockMediaStop,
  stockMissingImage,
  stockNetwork,
  stockNew,
  stockNo,
  stockOk,
  stockOpen,

{-# LINE 123 "Graphics/UI/Gtk/General/StockItems.hsc" #-}
  stockOrientationLandscape,
  stockOrientationReverseLandscape,
  stockOrientationPortrait,
  stockOrientationReversePortrait,

{-# LINE 128 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

{-# LINE 129 "Graphics/UI/Gtk/General/StockItems.hsc" #-}
  stockPageSetup,

{-# LINE 131 "Graphics/UI/Gtk/General/StockItems.hsc" #-}
  stockPaste,
  stockPreferences,
  stockPrint,

{-# LINE 135 "Graphics/UI/Gtk/General/StockItems.hsc" #-}
  stockPrintError,
  stockPrintPaused,
  stockPrintReport,
  stockPrintWarning,

{-# LINE 140 "Graphics/UI/Gtk/General/StockItems.hsc" #-}
  stockPrintPreview,
  stockProperties,
  stockQuit,
  stockRedo,
  stockRefresh,
  stockRemove,
  stockRevertToSaved,
  stockSave,
  stockSaveAs,

{-# LINE 150 "Graphics/UI/Gtk/General/StockItems.hsc" #-}
  stockSelectAll,

{-# LINE 152 "Graphics/UI/Gtk/General/StockItems.hsc" #-}
  stockSelectColor,
  stockSelectFont,
  stockSortAscending,
  stockSortDescending,
  stockSpellCheck,
  stockStop,
  stockStrikethrough,
  stockUndelete,
  stockUnderline,
  stockUndo,
  stockUnindent,
  stockYes,
  stockZoom100,
  stockZoomFit,
  stockZoomIn,
  stockZoomOut
  ) where

-- The StockItem structure is completely marshaled to Haskell. It is
-- possible to marshal all strings lazily because the string pointers are
-- valid throughout the lifetime of the application. The only drawback it
-- that a stock item that is replaced by the another item with the same
-- name will never be freed. This deficiency is built into Gtk however.
--

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Flags
import System.Glib.GList        (GSList, fromGSListRev)
import Graphics.UI.Gtk.Gdk.Events       (Modifier)
import Graphics.UI.Gtk.Gdk.Keys         (KeyVal)

-- |  A synonym for a standard button or icon.
--
type StockId = DefaultGlibString


-- Although the structure itself is allocated dynamically, its contents
-- are not. All string pointers are constant throughout the lifetime of
-- the application. We do not need to marshal these Strings to Haskell if
-- they are not needed.
--

-- | The description of a stock item.
--
data StockItem = StockItem {
  siStockId :: StockId,
  siLabel   :: DefaultGlibString,
  siModifier:: [Modifier],
  siKeyval  :: KeyVal,
  siTransDom:: DefaultGlibString }

instance Storable StockItem where
  sizeOf _      = 32
{-# LINE 208 "Graphics/UI/Gtk/General/StockItems.hsc" #-}
  alignment _   = alignment (undefined::CString)
  peek siPtr    = do
    (stockId    :: CString) <- (\hsc_ptr -> peekByteOff hsc_ptr 0) siPtr
{-# LINE 211 "Graphics/UI/Gtk/General/StockItems.hsc" #-}
    (label      :: CString) <- (\hsc_ptr -> peekByteOff hsc_ptr 8) siPtr
{-# LINE 212 "Graphics/UI/Gtk/General/StockItems.hsc" #-}
    (modifier   :: Word32)
{-# LINE 213 "Graphics/UI/Gtk/General/StockItems.hsc" #-}
                            <- (\hsc_ptr -> peekByteOff hsc_ptr 16) siPtr
{-# LINE 214 "Graphics/UI/Gtk/General/StockItems.hsc" #-}
    (keyval     :: Word32)
{-# LINE 215 "Graphics/UI/Gtk/General/StockItems.hsc" #-}
                            <- (\hsc_ptr -> peekByteOff hsc_ptr 20) siPtr
{-# LINE 216 "Graphics/UI/Gtk/General/StockItems.hsc" #-}
    (transDom   :: CString) <- (\hsc_ptr -> peekByteOff hsc_ptr 24) siPtr
{-# LINE 217 "Graphics/UI/Gtk/General/StockItems.hsc" #-}
    return $ StockItem {
      siStockId  = unsafePerformIO $ peekUTFString' stockId,
      siLabel    = unsafePerformIO $ peekUTFString' label,
      -- &%!?$ c2hs and hsc should agree on types
      siModifier = toFlags (fromIntegral modifier),
      siKeyval   = keyval,
      siTransDom = unsafePerformIO $ peekUTFString' transDom }
    where
      peekUTFString' :: CString -> IO DefaultGlibString
      peekUTFString' strPtr | strPtr==nullPtr = return ""
                            | otherwise       = peekUTFString strPtr

  poke siPtr (StockItem {
    siStockId = stockId,
    siLabel   = label,
    siModifier= modifier,
    siKeyval  = keyval,
    siTransDom= transDom }) = do
    stockIdPtr <- newUTFString stockId
    (\hsc_ptr -> pokeByteOff hsc_ptr 0) siPtr stockIdPtr
{-# LINE 237 "Graphics/UI/Gtk/General/StockItems.hsc" #-}
    labelPtr   <- newUTFString label
    (\hsc_ptr -> pokeByteOff hsc_ptr 8)    siPtr labelPtr
{-# LINE 239 "Graphics/UI/Gtk/General/StockItems.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 16) siPtr
{-# LINE 240 "Graphics/UI/Gtk/General/StockItems.hsc" #-}
      ((fromIntegral (fromFlags modifier))::Word32)
{-# LINE 241 "Graphics/UI/Gtk/General/StockItems.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 20)   siPtr ((fromIntegral keyval)::Word32)
{-# LINE 242 "Graphics/UI/Gtk/General/StockItems.hsc" #-}
    transDomPtr<- newUTFString transDom
    (\hsc_ptr -> pokeByteOff hsc_ptr 24) siPtr transDomPtr
{-# LINE 244 "Graphics/UI/Gtk/General/StockItems.hsc" #-}


-- | Add new stock items to Gtk.
--

-- Using stock_add_static would be possible if we used g_malloc to reserve
-- space since the allocated space might actually be freed when another
-- stock item with the same name is added.
stockAddItem :: [StockItem] -> IO ()
stockAddItem [] = return ()
stockAddItem sis = let items = length sis in do
  allocaArray items $ \aPtr -> do
  pokeArray aPtr sis
  stock_add aPtr (fromIntegral items)

-- | Lookup an item in stock.
--
stockLookupItem :: StockId -> IO (Maybe StockItem)
stockLookupItem stockId =
  alloca $ \siPtr ->
  withUTFString stockId $ \strPtr -> do
  res <- stock_lookup strPtr siPtr
  if (toBool res) then liftM Just $ peek siPtr else return Nothing

-- | Produce a list of all known stock identifiers.
--
-- * Retrieve a list of all known stock identifiers. These can either be
--   added by 'stockAddItem' or by adding items to a
--   'Graphics.UI.Gtk.General.IconFactory.IconFactory'.
--
-- * The list is sorted alphabetically (sorting is not Unicode aware).
--
stockListIds :: IO [StockId]
stockListIds = do
  lPtr <- stock_list_ids
  sPtrs <- fromGSListRev lPtr
  res <- mapM readUTFString sPtrs
  return res

foreign import ccall unsafe "gtk_stock_add"
  stock_add :: Ptr StockItem -> Word32 -> IO ()
{-# LINE 285 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

foreign import ccall unsafe "gtk_stock_lookup"
  stock_lookup :: CString -> Ptr StockItem -> IO Int32
{-# LINE 288 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

foreign import ccall unsafe "gtk_stock_list_ids"
  stock_list_ids :: IO GSList


{-# LINE 293 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-about.png>>
stockAbout              :: StockId
stockAbout              = "gtk-about"
{-# LINE 297 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

{-# LINE 300 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-add.png>>
stockAdd                :: StockId
stockAdd                = "gtk-add"
{-# LINE 304 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-apply.png>>
stockApply              :: StockId
stockApply              = "gtk-apply"
{-# LINE 308 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-bold.png>>
stockBold               :: StockId
stockBold               = "gtk-bold"
{-# LINE 312 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-cancel.png>>
stockCancel             :: StockId
stockCancel             = "gtk-cancel"
{-# LINE 316 "Graphics/UI/Gtk/General/StockItems.hsc" #-}


{-# LINE 318 "Graphics/UI/Gtk/General/StockItems.hsc" #-}
-- | <<http://library.gnome.org/devel/gtk/stable/gtk-caps-lock-warning.png>>
stockCapsLockWarning    :: StockId
stockCapsLockWarning    = "gtk-caps-lock-warning"
{-# LINE 321 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

{-# LINE 322 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-cdrom.png>>
stockCDROM              :: StockId
stockCDROM              = "gtk-cdrom"
{-# LINE 326 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-clear.png>>
stockClear              :: StockId
stockClear              = "gtk-clear"
{-# LINE 330 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-close.png>>
stockClose              :: StockId
stockClose              = "gtk-close"
{-# LINE 334 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

{-# LINE 335 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-color-picker.png>>
stockColorPicker        :: StockId
stockColorPicker        = "gtk-color-picker"
{-# LINE 339 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

{-# LINE 342 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-convert.png>>
stockConvert            :: StockId
stockConvert            = "gtk-convert"
{-# LINE 346 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

{-# LINE 347 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-connect.png>>
stockConnect            :: StockId
stockConnect            = "gtk-connect"
{-# LINE 351 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

{-# LINE 354 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-copy.png>>
stockCopy               :: StockId
stockCopy               = "gtk-copy"
{-# LINE 358 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-cut.png>>
stockCut                :: StockId
stockCut                = "gtk-cut"
{-# LINE 362 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-delete.png>>
stockDelete             :: StockId
stockDelete             = "gtk-delete"
{-# LINE 366 "Graphics/UI/Gtk/General/StockItems.hsc" #-}


{-# LINE 368 "Graphics/UI/Gtk/General/StockItems.hsc" #-}
-- | <<http://library.gnome.org/devel/gtk/stable/gtk-dialog-authentication.png>>
stockDialogAuthentication :: StockId
stockDialogAuthentication = "gtk-dialog-authentication"
{-# LINE 371 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

{-# LINE 374 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-dialog-error.png>>
stockDialogError        :: StockId
stockDialogError        = "gtk-dialog-error"
{-# LINE 378 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-dialog-info.png>>
stockDialogInfo         :: StockId
stockDialogInfo         = "gtk-dialog-info"
{-# LINE 382 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-dialog-question.png>>
stockDialogQuestion     :: StockId
stockDialogQuestion     = "gtk-dialog-question"
{-# LINE 386 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-dialog-warning.png>>
stockDialogWarning      :: StockId
stockDialogWarning      = "gtk-dialog-warning"
{-# LINE 390 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

{-# LINE 391 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-directory.png>>
stockDirectory          :: StockId
stockDirectory          = "gtk-directory"
{-# LINE 395 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

{-# LINE 398 "Graphics/UI/Gtk/General/StockItems.hsc" #-}


{-# LINE 400 "Graphics/UI/Gtk/General/StockItems.hsc" #-}
-- |
stockDiscard            :: StockId
stockDiscard            = "gtk-discard"
{-# LINE 403 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

{-# LINE 404 "Graphics/UI/Gtk/General/StockItems.hsc" #-}


{-# LINE 406 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-disconnect.png>>
stockDisconnect         :: StockId
stockDisconnect         = "gtk-disconnect"
{-# LINE 410 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

{-# LINE 413 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-dnd.png>>
stockDnd                :: StockId
stockDnd                = "gtk-dnd"
{-# LINE 417 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-dnd-multiple.png>>
stockDndMultiple        :: StockId
stockDndMultiple        = "gtk-dnd-multiple"
{-# LINE 421 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

{-# LINE 422 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-edit.png>>
stockEdit               :: StockId
stockEdit               = "gtk-edit"
{-# LINE 426 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

{-# LINE 429 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-execute.png>>
stockExecute            :: StockId
stockExecute            = "gtk-execute"
{-# LINE 433 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

{-# LINE 434 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-file.png>>
stockFile               :: StockId
stockFile               = "gtk-file"
{-# LINE 438 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

{-# LINE 441 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-find.png>>
stockFind               :: StockId
stockFind               = "gtk-find"
{-# LINE 445 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-find-and-replace.png>>
stockFindAndRelpace     :: StockId
stockFindAndRelpace     = "gtk-find-and-replace"
{-# LINE 449 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-floppy.png>>
stockFloppy             :: StockId
stockFloppy             = "gtk-floppy"
{-# LINE 453 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

{-# LINE 454 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-fullscreen.png>>
stockFullscreen         :: StockId
stockFullscreen         = "gtk-fullscreen"
{-# LINE 458 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

{-# LINE 461 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-goto-bottom.png>>
stockGotoBottom         :: StockId
stockGotoBottom         = "gtk-goto-bottom"
{-# LINE 465 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-goto-first-ltr.png>>
-- <<http://library.gnome.org/devel/gtk/stable/gtk-goto-first-rtl.png>>
stockGotoFirst          :: StockId
stockGotoFirst          = "gtk-goto-first"
{-# LINE 470 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-goto-last-ltr.png>>
-- <<http://library.gnome.org/devel/gtk/stable/gtk-goto-last-rtl.png>>
stockGotoLast           :: StockId
stockGotoLast           = "gtk-goto-last"
{-# LINE 475 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-goto-top.png>>
stockGotoTop            :: StockId
stockGotoTop            = "gtk-goto-top"
{-# LINE 479 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-go-back-ltr.png>>
-- <<http://library.gnome.org/devel/gtk/stable/gtk-go-back-rtl.png>>
stockGoBack             :: StockId
stockGoBack             = "gtk-go-back"
{-# LINE 484 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-go-down.png>>
stockGoDown             :: StockId
stockGoDown             = "gtk-go-down"
{-# LINE 488 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-go-forward-ltr.png>>
-- <<http://library.gnome.org/devel/gtk/stable/gtk-go-forward-rtl.png>>
stockGoForward          :: StockId
stockGoForward          = "gtk-go-forward"
{-# LINE 493 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-go-up.png>>
stockGoUp               :: StockId
stockGoUp               = "gtk-go-up"
{-# LINE 497 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

{-# LINE 498 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-harddisk.png>>
stockHarddisk           :: StockId
stockHarddisk           = "gtk-harddisk"
{-# LINE 502 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

{-# LINE 505 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-help.png>>
stockHelp               :: StockId
stockHelp               = "gtk-help"
{-# LINE 509 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-home.png>>
stockHome               :: StockId
stockHome               = "gtk-home"
{-# LINE 513 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

{-# LINE 514 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-indent-ltr.png>>
-- <<http://library.gnome.org/devel/gtk/stable/gtk-indent-rtl.png>>
stockIndent             :: StockId
stockIndent             = "gtk-indent"
{-# LINE 519 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

{-# LINE 522 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-index.png>>
stockIndex              :: StockId
stockIndex              = "gtk-index"
{-# LINE 526 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

{-# LINE 527 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-info.png>>
stockInfo               :: StockId
stockInfo               = "gtk-info"
{-# LINE 531 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

{-# LINE 534 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-italic.png>>
stockItalic             :: StockId
stockItalic             = "gtk-italic"
{-# LINE 538 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-jump-to-ltr.png>>
-- <<http://library.gnome.org/devel/gtk/stable/gtk-jump-to-rtl.png>>
stockJumpTo             :: StockId
stockJumpTo             = "gtk-jump-to"
{-# LINE 543 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-justify-center.png>>
stockJustifyCenter      :: StockId
stockJustifyCenter      = "gtk-justify-center"
{-# LINE 547 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-justify-fill.png>>
stockJustifyFill        :: StockId
stockJustifyFill        = "gtk-justify-fill"
{-# LINE 551 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-justify-left.png>>
stockJustifyLeft        :: StockId
stockJustifyLeft        = "gtk-justify-left"
{-# LINE 555 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-justify-right.png>>
stockJustifyRight       :: StockId
stockJustifyRight       = "gtk-justify-right"
{-# LINE 559 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-leave-fullscreen.png>>
stockLeaveFullscreen    :: StockId
stockLeaveFullscreen    = "gtk-leave-fullscreen"
{-# LINE 563 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-missing-image.png>>
stockMissingImage       :: StockId
stockMissingImage       = "gtk-missing-image"
{-# LINE 567 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

{-# LINE 568 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-media-forward-ltr.png>>
-- <<http://library.gnome.org/devel/gtk/stable/gtk-media-forward-rtl.png>>
stockMediaForward       :: StockId
stockMediaForward       = "gtk-media-forward"
{-# LINE 573 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-media-next-ltr.png>>
-- <<http://library.gnome.org/devel/gtk/stable/gtk-media-next-rtl.png>>
stockMediaNext          :: StockId
stockMediaNext          = "gtk-media-next"
{-# LINE 578 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-media-pause.png>>
stockMediaPause         :: StockId
stockMediaPause         = "gtk-media-pause"
{-# LINE 582 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-media-play-ltr.png>>
-- <<http://library.gnome.org/devel/gtk/stable/gtk-media-play-rtl.png>>
stockMediaPlay          :: StockId
stockMediaPlay          = "gtk-media-play"
{-# LINE 587 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-media-previous-ltr.png>>
-- <<http://library.gnome.org/devel/gtk/stable/gtk-media-previous-rtl.png>>
stockMediaPrevious      :: StockId
stockMediaPrevious      = "gtk-media-previous"
{-# LINE 592 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-media-record.png>>
stockMediaRecord        :: StockId
stockMediaRecord        = "gtk-media-record"
{-# LINE 596 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-media-rewind-ltr.png>>
-- <<http://library.gnome.org/devel/gtk/stable/gtk-media-rewind-rtl.png>>
stockMediaRewind        :: StockId
stockMediaRewind        = "gtk-media-rewind"
{-# LINE 601 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-media-stop.png>>
stockMediaStop          :: StockId
stockMediaStop          = "gtk-media-stop"
{-# LINE 605 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

{-# LINE 615 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

{-# LINE 616 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-network.png>>
stockNetwork            :: StockId
stockNetwork            = "gtk-network"
{-# LINE 620 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

{-# LINE 623 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-new.png>>
stockNew                :: StockId
stockNew                = "gtk-new"
{-# LINE 627 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-no.png>>
stockNo                 :: StockId
stockNo                 = "gtk-no"
{-# LINE 631 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-ok.png>>
stockOk                 :: StockId
stockOk                 = "gtk-ok"
{-# LINE 635 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-open.png>>
stockOpen               :: StockId
stockOpen               = "gtk-open"
{-# LINE 639 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

{-# LINE 640 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-orientation-landscape.png>>
stockOrientationLandscape :: StockId
stockOrientationLandscape = "gtk-orientation-landscape"
{-# LINE 644 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-orientation-reverse-landscape.png>>
stockOrientationReverseLandscape :: StockId
stockOrientationReverseLandscape = "gtk-orientation-reverse-landscape"
{-# LINE 648 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-orientation-portrait.png>>
stockOrientationPortrait  :: StockId
stockOrientationPortrait  = "gtk-orientation-portrait"
{-# LINE 652 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-orientation-reverse-portrait.png>>
stockOrientationReversePortrait  :: StockId
stockOrientationReversePortrait  = "gtk-orientation-reverse-portrait"
{-# LINE 656 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

{-# LINE 662 "Graphics/UI/Gtk/General/StockItems.hsc" #-}


{-# LINE 664 "Graphics/UI/Gtk/General/StockItems.hsc" #-}
-- | <<http://library.gnome.org/devel/gtkmm/stable/gtk-page-setup.png>>
stockPageSetup          :: StockId
stockPageSetup          = "gtk-page-setup"
{-# LINE 667 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

{-# LINE 668 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-paste.png>>
stockPaste              :: StockId
stockPaste              = "gtk-paste"
{-# LINE 672 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-preferences.png>>
stockPreferences        :: StockId
stockPreferences        = "gtk-preferences"
{-# LINE 676 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-print.png>>
stockPrint              :: StockId
stockPrint              = "gtk-print"
{-# LINE 680 "Graphics/UI/Gtk/General/StockItems.hsc" #-}


{-# LINE 682 "Graphics/UI/Gtk/General/StockItems.hsc" #-}
-- | <<http://library.gnome.org/devel/gtk/stable/gtk-print-error.png>>
stockPrintError         :: StockId
stockPrintError         = "gtk-print-error"
{-# LINE 685 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-print-paused.png>>
stockPrintPaused        :: StockId
stockPrintPaused        = "gtk-print-paused"
{-# LINE 689 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-print-report.png>>
stockPrintReport        :: StockId
stockPrintReport        = "gtk-print-report"
{-# LINE 693 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-print-warning.png>>
stockPrintWarning       :: StockId
stockPrintWarning       = "gtk-print-warning"
{-# LINE 697 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

{-# LINE 698 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-print-preview.png>>
stockPrintPreview       :: StockId
stockPrintPreview       = "gtk-print-preview"
{-# LINE 702 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-properties.png>>
stockProperties         :: StockId
stockProperties         = "gtk-properties"
{-# LINE 706 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-quit.png>>
stockQuit               :: StockId
stockQuit               = "gtk-quit"
{-# LINE 710 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-redo-ltr.png>>
-- <<http://library.gnome.org/devel/gtk/stable/gtk-redo-rtl.png>>
stockRedo               :: StockId
stockRedo               = "gtk-redo"
{-# LINE 715 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-refresh.png>>
stockRefresh            :: StockId
stockRefresh            = "gtk-refresh"
{-# LINE 719 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-remove.png>>
stockRemove             :: StockId
stockRemove             = "gtk-remove"
{-# LINE 723 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-revert-to-saved-ltr.png>>
-- <<http://library.gnome.org/devel/gtk/stable/gtk-revert-to-saved-rtl.png>>
stockRevertToSaved      :: StockId
stockRevertToSaved      = "gtk-revert-to-saved"
{-# LINE 728 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-save.png>>
stockSave               :: StockId
stockSave               = "gtk-save"
{-# LINE 732 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-save-as.png>>
stockSaveAs             :: StockId
stockSaveAs             = "gtk-save-as"
{-# LINE 736 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

{-# LINE 737 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-select-all.png>>
stockSelectAll          :: StockId
stockSelectAll          = "gtk-select-all"
{-# LINE 741 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

{-# LINE 744 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-select-color.png>>
stockSelectColor        :: StockId
stockSelectColor        = "gtk-select-color"
{-# LINE 748 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-font.png>>
stockSelectFont         :: StockId
stockSelectFont         = "gtk-select-font"
{-# LINE 752 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-sort-ascending.png>>
stockSortAscending      :: StockId
stockSortAscending      = "gtk-sort-ascending"
{-# LINE 756 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-sort-descending.png>>
stockSortDescending     :: StockId
stockSortDescending     = "gtk-sort-descending"
{-# LINE 760 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-spell-check.png>>
stockSpellCheck         :: StockId
stockSpellCheck         = "gtk-spell-check"
{-# LINE 764 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-stop.png>>
stockStop               :: StockId
stockStop               = "gtk-stop"
{-# LINE 768 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-strikethrough.png>>
stockStrikethrough      :: StockId
stockStrikethrough      = "gtk-strikethrough"
{-# LINE 772 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-undelete-ltr.png>>
-- <<http://library.gnome.org/devel/gtk/stable/gtk-undelete-rtl.png>>
stockUndelete           :: StockId
stockUndelete           = "gtk-undelete"
{-# LINE 777 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-underline.png>>
stockUnderline          :: StockId
stockUnderline          = "gtk-underline"
{-# LINE 781 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-undo-ltr.png>>
-- <<http://library.gnome.org/devel/gtk/stable/gtk-undo-rtl.png>>
stockUndo               :: StockId
stockUndo               = "gtk-undo"
{-# LINE 786 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

{-# LINE 787 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-unindent-ltr.png>>
-- <<http://library.gnome.org/devel/gtk/stable/gtk-unindent-rtl.png>>
stockUnindent           :: StockId
stockUnindent           = "gtk-unindent"
{-# LINE 792 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

{-# LINE 795 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-yes.png>>
stockYes                :: StockId
stockYes                = "gtk-yes"
{-# LINE 799 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-zoom-100.png>>
stockZoom100            :: StockId
stockZoom100            = "gtk-zoom-100"
{-# LINE 803 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-zoom-fit.png>>
stockZoomFit            :: StockId
stockZoomFit            = "gtk-zoom-fit"
{-# LINE 807 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-zoom-in.png>>
stockZoomIn             :: StockId
stockZoomIn             = "gtk-zoom-in"
{-# LINE 811 "Graphics/UI/Gtk/General/StockItems.hsc" #-}

-- | <<http://library.gnome.org/devel/gtk/stable/gtk-zoom-out.png>>
stockZoomOut            :: StockId
stockZoomOut            = "gtk-zoom-out"
{-# LINE 815 "Graphics/UI/Gtk/General/StockItems.hsc" #-}




