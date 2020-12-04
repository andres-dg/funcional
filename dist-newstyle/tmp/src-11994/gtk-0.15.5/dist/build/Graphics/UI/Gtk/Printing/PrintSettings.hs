
{-# LINE 2 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LINE 3 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget PrintSettings
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
-- Stores print settings
--
-- * Module available since Gtk+ version 2.10
--
module Graphics.UI.Gtk.Printing.PrintSettings (

-- * Detail
--
-- | A 'PrintSettings' object represents the settings of a print dialog in a
-- system-independent way. The main use for this object is that once you\'ve
-- printed you can get a settings object that represents the settings the user
-- chose, and the next time you print you can pass that object in so that the
-- user doesn't have to re-set all his settings.
--
-- Its also possible to enumerate the settings so that you can easily save
-- the settings for the next time your app runs, or even store them in a
-- document. The predefined keys try to use shared values as much as possible
-- so that moving such a document between systems still works.
--
-- Printing support was added in Gtk+ 2.10.
--

-- * Class Hierarchy
--
-- |
-- @
-- | 'GObject'
-- | +----PrintSettings
-- @


-- * Types
  PrintSettings,
  PrintSettingsClass,
  castToPrintSettings,
  toPrintSettings,
  -- PageRange,

-- * Enums
  PageOrientation(..),

  NumberUpLayout(..),

  PrintQuality(..),
  PrintDuplex(..),
  PrintPages(..),
  PageSet(..),

-- * Constructors
  printSettingsNew,

  printSettingsNewFromFile,


-- * Methods
  printSettingsCopy,
  printSettingsHasKey,
  printSettingsGet,
  printSettingsSet,
  printSettingsUnset,
  printSettingsForeach,
  printSettingsGetBool,
  printSettingsSetBool,
  printSettingsGetDouble,
  printSettingsGetDoubleWithDefault,
  printSettingsSetDouble,
  printSettingsGetLength,
  printSettingsSetLength,
  printSettingsGetInt,
  printSettingsGetIntWithDefault,
  printSettingsSetInt,
  printSettingsGetPaperWidth,
  printSettingsSetPaperWidth,
  printSettingsGetPaperHeight,
  printSettingsSetPaperHeight,

  printSettingsSetResolutionXy,
  printSettingsGetResolutionX,
  printSettingsGetResolutionY,

  -- printSettingsGetPageRanges,
  -- printSettingsSetPageRanges,

  printSettingsLoadFile,


  printSettingsToFile,


-- * Attributes
  printSettingsPrinter,
  printSettingsOrientation,
  printSettingsPaperSize,
  printSettingsUseColor,
  printSettingsCollate,
  printSettingsReverse,
  printSettingsDuplex,
  printSettingsQuality,
  printSettingsNCopies,
  printSettingsNumberUp,
  printSettingsResolution,
  printSettingsScale,
  printSettingsPrintPages,
  printSettingsPageSet,
  printSettingsDefaultSource,
  printSettingsMediaType,
  printSettingsDither,
  printSettingsFinishings,
  printSettingsOutputBin,

  printSettingsNumberUpLayout,


  printSettingsPrinterLpi,


  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.Attributes
import System.Glib.UTFString
import System.Glib.GError
import Graphics.UI.Gtk.Types
{-# LINE 152 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}

import Graphics.UI.Gtk.Printing.PaperSize (PaperSize(PaperSize), mkPaperSize, Unit(..))



{-# LINE 157 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}


--------------------
-- Enums
data PageOrientation = PageOrientationPortrait
                     | PageOrientationLandscape
                     | PageOrientationReversePortrait
                     | PageOrientationReverseLandscape
                     deriving (Enum,Bounded,Eq,Show)

{-# LINE 162 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}

data PrintQuality = PrintQualityLow
                  | PrintQualityNormal
                  | PrintQualityHigh
                  | PrintQualityDraft
                  deriving (Enum,Bounded,Eq,Show)

{-# LINE 164 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}

data PrintDuplex = PrintDuplexSimplex
                 | PrintDuplexHorizontal
                 | PrintDuplexVertical
                 deriving (Enum,Bounded,Eq,Show)

{-# LINE 166 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}

data PrintPages = PrintPagesAll
                | PrintPagesCurrent
                | PrintPagesRanges
                | PrintPagesSelection
                deriving (Enum,Bounded,Eq,Show)

{-# LINE 168 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}

data PageSet = PageSetAll
             | PageSetEven
             | PageSetOdd
             deriving (Enum,Bounded,Eq,Show)

{-# LINE 170 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}


-- | Used to determine the layout of pages on a sheet when printing multiple pages per sheet.
data NumberUpLayout = NumberUpLayoutLeftToRightTopToBottom
                    | NumberUpLayoutLeftToRightBottomToTop
                    | NumberUpLayoutRightToLeftTopToBottom
                    | NumberUpLayoutRightToLeftBottomToTop
                    | NumberUpLayoutTopToBottomLeftToRight
                    | NumberUpLayoutTopToBottomRightToLeft
                    | NumberUpLayoutBottomToTopLeftToRight
                    | NumberUpLayoutBottomToTopRightToLeft
                    deriving (Enum,Bounded,Eq,Show)

{-# LINE 174 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}


--------------------
-- Constructors

-- | Creates a new 'PrintSettings' object.
--
printSettingsNew :: IO PrintSettings
printSettingsNew =
  wrapNewGObject mkPrintSettings $
  gtk_print_settings_new
{-# LINE 185 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}


-- | Reads the print settings from @fileName@. Returns a new 'PrintSettings'
-- object with the restored settings.
--
-- * Available since Gtk+ version 2.12
--
printSettingsNewFromFile :: GlibFilePath fp
 => fp -- ^ @fileName@ - the filename to read the settings from
 -> IO PrintSettings
printSettingsNewFromFile fileName =
  wrapNewGObject mkPrintSettings $
  propagateGError $ \errorPtr ->
  withUTFFilePath fileName $ \fileNamePtr ->
  gtk_print_settings_new_from_file
{-# LINE 200 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
        fileNamePtr
        errorPtr



--------------------
-- Methods

-- | Copies a 'PrintSettings' object.
--
printSettingsCopy :: PrintSettingsClass self => self
 -> IO PrintSettings -- ^ returns a newly allocated copy of @other@
printSettingsCopy self =
  wrapNewGObject mkPrintSettings $
  (\(PrintSettings arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_copy argPtr1)
{-# LINE 215 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)

-- | Returns @True@, if a value is associated with @key@.
--
printSettingsHasKey :: (PrintSettingsClass self, GlibString string) => self
 -> string -- ^ @key@ - a key
 -> IO Bool -- ^ returns @True@, if @key@ has a value
printSettingsHasKey self key =
  liftM toBool $
  withUTFString key $ \keyPtr ->
  (\(PrintSettings arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_has_key argPtr1 arg2)
{-# LINE 226 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)
    keyPtr

-- | Looks up the string value associated with @key@.
--
printSettingsGet :: (PrintSettingsClass self, GlibString string) => self
 -> string -- ^ @key@ - a key
 -> IO string -- ^ returns the string value for @key@
printSettingsGet self key =
  withUTFString key $ \keyPtr ->
  (\(PrintSettings arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_get argPtr1 arg2)
{-# LINE 237 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)
    keyPtr
  >>= peekUTFString

-- | Associates @value@ with @key@.
--
printSettingsSet :: (PrintSettingsClass self, GlibString string) => self
 -> string -- ^ @key@ - a key
 -> string -- ^ @value@ - a string value
 -> IO ()
printSettingsSet self key value =
  withUTFString value $ \valuePtr ->
  withUTFString key $ \keyPtr ->
  (\(PrintSettings arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_set argPtr1 arg2 arg3)
{-# LINE 251 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)
    keyPtr
    valuePtr

-- | Removes any value associated with @key@
--
printSettingsUnset :: (PrintSettingsClass self, GlibString string) => self
 -> string -- ^ @key@ - a key
 -> IO ()
printSettingsUnset self key =
  withUTFString key $ \keyPtr ->
  (\(PrintSettings arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_unset argPtr1 arg2)
{-# LINE 263 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)
    keyPtr

-- | Calls @func@ for each key-value pair of @settings@.
--
printSettingsForeach :: PrintSettingsClass self => self
 -> (String -> IO ()) -- ^ @func@ - the function to call
 -> IO ()
printSettingsForeach self func = do
  funcPtr <- mkPrintSettingsFunc $ \_ strPtr _ -> do
    str <- peekCString strPtr
    func str
  (\(PrintSettings arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_foreach argPtr1 arg2 arg3)
{-# LINE 276 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)
    funcPtr
    (castFunPtrToPtr funcPtr)

type PrintSettingsFunc = FunPtr (((Ptr CChar) -> ((Ptr CChar) -> ((Ptr ()) -> (IO ())))))
{-# LINE 281 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}

foreign import ccall "wrapper" mkPrintSettingsFunc ::
  (CString -> CString -> Ptr () -> IO ())
  -> IO PrintSettingsFunc

-- | Returns the boolean represented by the value that is associated with
-- @key@.
--
-- The string \"true\" represents @True@, any other string @False@.
--
printSettingsGetBool :: (PrintSettingsClass self, GlibString string) => self
 -> string -- ^ @key@ - a key
 -> IO Bool -- ^ returns @True@, if @key@ maps to a true value.
printSettingsGetBool self key =
  liftM toBool $
  withUTFString key $ \keyPtr ->
  (\(PrintSettings arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_get_bool argPtr1 arg2)
{-# LINE 298 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)
    keyPtr

-- | Sets @key@ to a boolean value.
--
printSettingsSetBool :: (PrintSettingsClass self, GlibString string) => self
 -> string -- ^ @key@ - a key
 -> Bool -- ^ @value@ - a boolean
 -> IO ()
printSettingsSetBool self key value =
  withUTFString key $ \keyPtr ->
  (\(PrintSettings arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_set_bool argPtr1 arg2 arg3)
{-# LINE 310 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)
    keyPtr
    (fromBool value)

-- | Returns the double value associated with @key@, or 0.
--
printSettingsGetDouble :: (PrintSettingsClass self, GlibString string) => self
 -> string -- ^ @key@ - a key
 -> IO Double -- ^ returns the double value of @key@
printSettingsGetDouble self key =
  liftM realToFrac $
  withUTFString key $ \keyPtr ->
  (\(PrintSettings arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_get_double argPtr1 arg2)
{-# LINE 323 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)
    keyPtr

-- | Returns the floating point number represented by the value that is
-- associated with @key@, or @defaultVal@ if the value does not represent a
-- floating point number.
--
-- Floating point numbers are parsed with 'gAsciiStrtod'.
--
printSettingsGetDoubleWithDefault :: (PrintSettingsClass self, GlibString string) => self
 -> string -- ^ @key@ - a key
 -> Double -- ^ @def@ - the default value
 -> IO Double -- ^ returns the floating point number associated with @key@
printSettingsGetDoubleWithDefault self key def =
  liftM realToFrac $
  withUTFString key $ \keyPtr ->
  (\(PrintSettings arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_get_double_with_default argPtr1 arg2 arg3)
{-# LINE 340 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)
    keyPtr
    (realToFrac def)

-- | Sets @key@ to a double value.
--
printSettingsSetDouble :: (PrintSettingsClass self, GlibString string) => self
 -> string -- ^ @key@ - a key
 -> Double -- ^ @value@ - a double value
 -> IO ()
printSettingsSetDouble self key value =
  withUTFString key $ \keyPtr ->
  (\(PrintSettings arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_set_double argPtr1 arg2 arg3)
{-# LINE 353 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)
    keyPtr
    (realToFrac value)

-- | Returns the value associated with @key@, interpreted as a length. The
-- returned value is converted to @units@.
--
printSettingsGetLength :: (PrintSettingsClass self, GlibString string) => self
 -> string -- ^ @key@ - a key
 -> Unit -- ^ @unit@ - the unit of the return value
 -> IO Double -- ^ returns the length value of @key@, converted to @unit@
printSettingsGetLength self key unit =
  liftM realToFrac $
  withUTFString key $ \keyPtr ->
  (\(PrintSettings arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_get_length argPtr1 arg2 arg3)
{-# LINE 368 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)
    keyPtr
    ((fromIntegral . fromEnum) unit)

-- | Associates a length in units of @unit@ with @key@.
--
printSettingsSetLength :: (PrintSettingsClass self, GlibString string) => self
 -> string -- ^ @key@ - a key
 -> Double -- ^ @value@ - a length
 -> Unit -- ^ @unit@ - the unit of @length@
 -> IO ()
printSettingsSetLength self key value unit =
  withUTFString key $ \keyPtr ->
  (\(PrintSettings arg1) arg2 arg3 arg4 -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_set_length argPtr1 arg2 arg3 arg4)
{-# LINE 382 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)
    keyPtr
    (realToFrac value)
    ((fromIntegral . fromEnum) unit)

-- | Returns the integer value of @key@, or 0.
--
printSettingsGetInt :: (PrintSettingsClass self, GlibString string) => self
 -> string -- ^ @key@ - a key
 -> IO Int -- ^ returns the integer value of @key@
printSettingsGetInt self key =
  liftM fromIntegral $
  withUTFString key $ \keyPtr ->
  (\(PrintSettings arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_get_int argPtr1 arg2)
{-# LINE 396 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)
    keyPtr

-- | Returns the value of @key@, interpreted as an integer, or the default
-- value.
--
printSettingsGetIntWithDefault :: (PrintSettingsClass self, GlibString string) => self
 -> string -- ^ @key@ - a key
 -> Int -- ^ @def@ - the default value
 -> IO Int -- ^ returns the integer value of @key@
printSettingsGetIntWithDefault self key def =
  liftM fromIntegral $
  withUTFString key $ \keyPtr ->
  (\(PrintSettings arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_get_int_with_default argPtr1 arg2 arg3)
{-# LINE 410 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)
    keyPtr
    (fromIntegral def)

-- | Sets @key@ to an integer value.
--
printSettingsSetInt :: (PrintSettingsClass self, GlibString string) => self
 -> string -- ^ @key@ - a key
 -> Int -- ^ @value@ - an integer
 -> IO ()
printSettingsSetInt self key value =
  withUTFString key $ \keyPtr ->
  (\(PrintSettings arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_set_int argPtr1 arg2 arg3)
{-# LINE 423 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)
    keyPtr
    (fromIntegral value)

-- | Convenience function to obtain the value of ''PrintSettingsPrinter''.
printSettingsGetPrinter :: (PrintSettingsClass self, GlibString string) => self
 -> IO string -- ^ returns the printer name
printSettingsGetPrinter self =
  (\(PrintSettings arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_get_printer argPtr1)
{-# LINE 432 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)
  >>= peekUTFString

-- | Convenience function to obtain the value of ''PrintSettingsPrinter''.
printSettingsSetPrinter :: (PrintSettingsClass self, GlibString string) => self
 -> string -- ^ @printer@ - the printer name
 -> IO ()
printSettingsSetPrinter self printer =
  withUTFString printer $ \printerPtr ->
  (\(PrintSettings arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_set_printer argPtr1 arg2)
{-# LINE 442 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)
    printerPtr

-- | Get the value of ''PrintSettingsOrientation'', converted to a 'PageOrientation'.
printSettingsGetOrientation :: PrintSettingsClass self => self
 -> IO PageOrientation -- ^ returns the orientation
printSettingsGetOrientation self =
  liftM (toEnum . fromIntegral) $
  (\(PrintSettings arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_get_orientation argPtr1)
{-# LINE 451 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)

-- | Sets the value of ''PrintSettingsOrientation''.
printSettingsSetOrientation :: PrintSettingsClass self => self
 -> PageOrientation -- ^ @orientation@ - a page orientation
 -> IO ()
printSettingsSetOrientation self orientation =
  (\(PrintSettings arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_set_orientation argPtr1 arg2)
{-# LINE 459 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)
    ((fromIntegral . fromEnum) orientation)

-- | Gets the value of 'PrintSettingsPaperFormat', converted to a 'PaperSize'.
printSettingsGetPaperSize :: PrintSettingsClass self => self
 -> IO PaperSize -- ^ returns the paper size
printSettingsGetPaperSize self =
  (\(PrintSettings arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_get_paper_size argPtr1)
{-# LINE 467 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
            (toPrintSettings self)
  >>= mkPaperSize . castPtr

-- | Sets the value of 'PrintSettingsPaperFormat', 'PrintSettingsPaperWidth' and
-- 'PrintSettingsPaperHeight'.
printSettingsSetPaperSize :: PrintSettingsClass self => self
 -> PaperSize -- ^ @paperSize@ - a paper size
 -> IO ()
printSettingsSetPaperSize self (PaperSize paperSize) =
  (\(PrintSettings arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_set_paper_size argPtr1 arg2)
{-# LINE 477 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)
    (castPtr $ unsafeForeignPtrToPtr $ paperSize)

-- | Gets the value of 'PrintSettingsPaperWidth', converted to unit.
--
printSettingsGetPaperWidth :: PrintSettingsClass self => self
 -> Unit -- ^ @unit@ - the unit for the return value
 -> IO Double -- ^ returns the paper width, in units of @unit@
printSettingsGetPaperWidth self unit =
  liftM realToFrac $
  (\(PrintSettings arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_get_paper_width argPtr1 arg2)
{-# LINE 488 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)
    ((fromIntegral . fromEnum) unit)

-- | Sets the value of 'PrintSettingsPaperWidth'.
--
printSettingsSetPaperWidth :: PrintSettingsClass self => self
 -> Double -- ^ @width@ - the paper width
 -> Unit -- ^ @unit@ - the units of @width@
 -> IO ()
printSettingsSetPaperWidth self width unit =
  (\(PrintSettings arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_set_paper_width argPtr1 arg2 arg3)
{-# LINE 499 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)
    (realToFrac width)
    ((fromIntegral . fromEnum) unit)

-- | Gets the value of 'PrintSettingsPaperHeight', converted to unit.
--
printSettingsGetPaperHeight :: PrintSettingsClass self => self
 -> Unit -- ^ @unit@ - the unit for the return value
 -> IO Double -- ^ returns the paper height, in units of @unit@
printSettingsGetPaperHeight self unit =
  liftM realToFrac $
  (\(PrintSettings arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_get_paper_height argPtr1 arg2)
{-# LINE 511 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)
    ((fromIntegral . fromEnum) unit)

-- | Sets the value of 'PrintSettingsPaperHeight'.
--
printSettingsSetPaperHeight :: PrintSettingsClass self => self
 -> Double -- ^ @height@ - the paper height
 -> Unit -- ^ @unit@ - the units of @height@
 -> IO ()
printSettingsSetPaperHeight self height unit =
  (\(PrintSettings arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_set_paper_height argPtr1 arg2 arg3)
{-# LINE 522 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)
    (realToFrac height)
    ((fromIntegral . fromEnum) unit)

-- | Gets the value of ''PrintSettingsUseColor''.
printSettingsGetUseColor :: PrintSettingsClass self => self
 -> IO Bool -- ^ returns whether to use color
printSettingsGetUseColor self =
  liftM toBool $
  (\(PrintSettings arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_get_use_color argPtr1)
{-# LINE 532 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)

-- | Sets the value of ''PrintSettingsUseColor''.
printSettingsSetUseColor :: PrintSettingsClass self => self
 -> Bool -- ^ @useColor@ - whether to use color
 -> IO ()
printSettingsSetUseColor self useColor =
  (\(PrintSettings arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_set_use_color argPtr1 arg2)
{-# LINE 540 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)
    (fromBool useColor)

-- | Gets the value of ''PrintSettingsCollate''.
printSettingsGetCollate :: PrintSettingsClass self => self
 -> IO Bool -- ^ returns whether to collate the printed pages
printSettingsGetCollate self =
  liftM toBool $
  (\(PrintSettings arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_get_collate argPtr1)
{-# LINE 549 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)

-- | Sets the value of ''PrintSettingsCollate''.
printSettingsSetCollate :: PrintSettingsClass self => self
 -> Bool -- ^ @collate@ - whether to collate the output
 -> IO ()
printSettingsSetCollate self collate =
  (\(PrintSettings arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_set_collate argPtr1 arg2)
{-# LINE 557 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)
    (fromBool collate)

-- | Gets the value of ''PrintSettingsReverse''.
printSettingsGetReverse :: PrintSettingsClass self => self
 -> IO Bool -- ^ returns whether to reverse the order of the printed pages
printSettingsGetReverse self =
  liftM toBool $
  (\(PrintSettings arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_get_reverse argPtr1)
{-# LINE 566 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)

-- | Sets the value of ''PrintSettingsReverse''.
printSettingsSetReverse :: PrintSettingsClass self => self
 -> Bool -- ^ @reverse@ - whether to reverse the output
 -> IO ()
printSettingsSetReverse self reverse =
  (\(PrintSettings arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_set_reverse argPtr1 arg2)
{-# LINE 574 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)
    (fromBool reverse)

-- | Gets the value of ''PrintSettingsDuplex''.
printSettingsGetDuplex :: PrintSettingsClass self => self
 -> IO PrintDuplex -- ^ returns whether to print the output in duplex.
printSettingsGetDuplex self =
  liftM (toEnum . fromIntegral) $
  (\(PrintSettings arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_get_duplex argPtr1)
{-# LINE 583 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)

-- | Sets the value of 'PrintSettingsDuplex'.
printSettingsSetDuplex :: PrintSettingsClass self => self
 -> PrintDuplex -- ^ @duplex@ - a 'PrintDuplex' value
 -> IO ()
printSettingsSetDuplex self duplex =
  (\(PrintSettings arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_set_duplex argPtr1 arg2)
{-# LINE 591 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)
    ((fromIntegral . fromEnum) duplex)

-- | Gets the value of 'PrintSettingsQuality'.
printSettingsGetQuality :: PrintSettingsClass self => self
 -> IO PrintQuality -- ^ returns the print quality
printSettingsGetQuality self =
  liftM (toEnum . fromIntegral) $
  (\(PrintSettings arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_get_quality argPtr1)
{-# LINE 600 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)

-- | Sets the value of 'PrintSettingsQuality'.
printSettingsSetQuality :: PrintSettingsClass self => self
 -> PrintQuality -- ^ @quality@ - a 'PrintQuality' value
 -> IO ()
printSettingsSetQuality self quality =
  (\(PrintSettings arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_set_quality argPtr1 arg2)
{-# LINE 608 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)
    ((fromIntegral . fromEnum) quality)

-- | Gets the value of 'PrintSettingsNCopies'.
printSettingsGetNCopies :: PrintSettingsClass self => self
 -> IO Int -- ^ returns the number of copies to print
printSettingsGetNCopies self =
  liftM fromIntegral $
  (\(PrintSettings arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_get_n_copies argPtr1)
{-# LINE 617 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)

-- | Sets the value of 'PrintSettingsNCopies'.
printSettingsSetNCopies :: PrintSettingsClass self => self
 -> Int -- ^ @numCopies@ - the number of copies
 -> IO ()
printSettingsSetNCopies self numCopies =
  (\(PrintSettings arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_set_n_copies argPtr1 arg2)
{-# LINE 625 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)
    (fromIntegral numCopies)

-- | Gets the value of 'PrintSettingsNumberUp'.
printSettingsGetNumberUp :: PrintSettingsClass self => self
 -> IO Int -- ^ returns the number of pages per sheet
printSettingsGetNumberUp self =
  liftM fromIntegral $
  (\(PrintSettings arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_get_number_up argPtr1)
{-# LINE 634 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)

-- | Sets the value of 'PrintSettingsNumberUp'.
printSettingsSetNumberUp :: PrintSettingsClass self => self
 -> Int -- ^ @numberUp@ - the number of pages per sheet
 -> IO ()
printSettingsSetNumberUp self numberUp =
  (\(PrintSettings arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_set_number_up argPtr1 arg2)
{-# LINE 642 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)
    (fromIntegral numberUp)


-- | Gets the value of 'PrintSettingsNumberUpLayout'.
printSettingsGetNumberUpLayout :: PrintSettingsClass self => self
 -> IO NumberUpLayout -- ^ returns layout of page in number-up mode
printSettingsGetNumberUpLayout self =
  liftM (toEnum . fromIntegral) $
  (\(PrintSettings arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_get_number_up_layout argPtr1)
{-# LINE 652 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)

-- | Sets the value of 'PrintSettingsNumberUpLayout'.
printSettingsSetNumberUpLayout :: PrintSettingsClass self => self
 -> NumberUpLayout -- ^ @numberUpLayout@ - a 'NumberUpLayout' value
 -> IO ()
printSettingsSetNumberUpLayout self numberUpLayout =
  (\(PrintSettings arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_set_number_up_layout argPtr1 arg2)
{-# LINE 660 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)
    ((fromIntegral . fromEnum) numberUpLayout)


-- | Gets the value of 'PrintSettingsResolution'.
printSettingsGetResolution :: PrintSettingsClass self => self
 -> IO Int -- ^ returns the resolution in dpi
printSettingsGetResolution self =
  liftM fromIntegral $
  (\(PrintSettings arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_get_resolution argPtr1)
{-# LINE 670 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)

-- | Sets the values of 'PrintSettingsResolution', 'PrintSettingsResolutionX' and
-- 'PrintSettingsResolutionY'.
printSettingsSetResolution :: PrintSettingsClass self => self
 -> Int -- ^ @resolution@ - the resolution in dpi
 -> IO ()
printSettingsSetResolution self resolution =
  (\(PrintSettings arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_set_resolution argPtr1 arg2)
{-# LINE 679 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)
    (fromIntegral resolution)


-- | Sets the values of 'PrintSettingsResolution', 'PrintSettingsResolutionX' and
-- 'PrintSettingsResolutionY'.
--
-- * Available since Gtk+ version 2.16
--
printSettingsSetResolutionXy :: PrintSettingsClass self => self
 -> Int -- ^ @resolutionX@ - the horizontal resolution in dpi
 -> Int -- ^ @resolutionY@ - the vertical resolution in dpi
 -> IO ()
printSettingsSetResolutionXy self resolutionX resolutionY =
  (\(PrintSettings arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_set_resolution_xy argPtr1 arg2 arg3)
{-# LINE 694 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)
    (fromIntegral resolutionX)
    (fromIntegral resolutionY)

-- | Gets the value of @"resolution-x"@.
--
-- * Available since Gtk+ version 2.16
--
printSettingsGetResolutionX :: PrintSettingsClass self => self
 -> IO Int -- ^ returns the horizontal resolution in dpi
printSettingsGetResolutionX self =
  liftM fromIntegral $
  (\(PrintSettings arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_get_resolution_x argPtr1)
{-# LINE 707 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)

-- | Gets the value of @"resolution-y"@.
--
-- * Available since Gtk+ version 2.16
--
printSettingsGetResolutionY :: PrintSettingsClass self => self
 -> IO Int -- ^ returns the vertical resolution in dpi
printSettingsGetResolutionY self =
  liftM fromIntegral $
  (\(PrintSettings arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_get_resolution_y argPtr1)
{-# LINE 718 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)

-- | Gets the value of 'PrintSettingsPrinterLpi'.
printSettingsGetPrinterLpi :: PrintSettingsClass self => self
 -> IO Double -- ^ returns the resolution in lpi (lines per inch)
printSettingsGetPrinterLpi self =
  liftM realToFrac $
  (\(PrintSettings arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_get_printer_lpi argPtr1)
{-# LINE 726 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)

-- | Sets the value of 'PrintSettingsPrinterLpi'.
printSettingsSetPrinterLpi :: PrintSettingsClass self => self
 -> Double -- ^ @lpi@ - the resolution in lpi (lines per inch)
 -> IO ()
printSettingsSetPrinterLpi self lpi =
  (\(PrintSettings arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_set_printer_lpi argPtr1 arg2)
{-# LINE 734 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)
    (realToFrac lpi)


-- | Gets the value of 'PrintSettingsScale'.
printSettingsGetScale :: PrintSettingsClass self => self
 -> IO Double -- ^ returns the scale in percent
printSettingsGetScale self =
  liftM realToFrac $
  (\(PrintSettings arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_get_scale argPtr1)
{-# LINE 744 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)

-- | Sets the value of 'PrintSettingsScale'.
printSettingsSetScale :: PrintSettingsClass self => self
 -> Double -- ^ @scale@ - the scale in percent
 -> IO ()
printSettingsSetScale self scale =
  (\(PrintSettings arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_set_scale argPtr1 arg2)
{-# LINE 752 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)
    (realToFrac scale)

-- | Gets the value of 'PrintSettingsPrintPages'.
printSettingsGetPrintPages :: PrintSettingsClass self => self
 -> IO PrintPages -- ^ returns which pages to print
printSettingsGetPrintPages self =
  liftM (toEnum . fromIntegral) $
  (\(PrintSettings arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_get_print_pages argPtr1)
{-# LINE 761 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)

-- | Sets the value of 'PrintSettingsPrintPages'.
printSettingsSetPrintPages :: PrintSettingsClass self => self
 -> PrintPages -- ^ @pages@ - a 'PrintPages' value
 -> IO ()
printSettingsSetPrintPages self pages =
  (\(PrintSettings arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_set_print_pages argPtr1 arg2)
{-# LINE 769 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)
    ((fromIntegral . fromEnum) pages)

-- | Gets the value of 'PrintSettingsPageRanges'.
--
-- printSettingsGetPageRanges :: PrintSettingsClass self => self
-- -> IO [PageRange] -- ^ returns an array of 'PageRange'.
-- printSettingsGetPageRanges self =
-- alloca $ \numRangesPtr -> do
-- rangeListPtr <- {# call gtk_print_settings_get_page_ranges #}
-- (toPrintSettings self)
-- numRangesPtr
-- rangeLen <- peek numRangesPtr
-- ptrList <- peekArray (fromIntegral rangeLen) (castPtr rangeListPtr)
-- rangeList <- mapM peek ptrList
-- {#call unsafe g_free#} (castPtr rangeListPtr)
-- return rangeList

-- | Sets the value of @"page-ranges"@.
--
-- printSettingsSetPageRanges :: PrintSettingsClass self => self
-- -> [PageRange] -- ^ @pageRanges@ - an array of 'PageRange'
-- -> IO ()
-- printSettingsSetPageRanges self rangeList =
-- withArrayLen (concatMap (\(PageRange x y) -> [fromIntegral x, fromIntegral y]) rangeList)
-- $ \rangeLen rangeListPtr ->
-- {# call gtk_print_settings_set_page_ranges #}
-- (toPrintSettings self)
-- (castPtr rangeListPtr)
-- (fromIntegral rangeLen)

-- | Gets the value of 'PrintSettingsPageSet'.
printSettingsGetPageSet :: PrintSettingsClass self => self
 -> IO PageSet -- ^ returns the set of pages to print
printSettingsGetPageSet self =
  liftM (toEnum . fromIntegral) $
  (\(PrintSettings arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_get_page_set argPtr1)
{-# LINE 806 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)

-- | Sets the value of 'PrintSettingsPageSet'.
printSettingsSetPageSet :: PrintSettingsClass self => self
 -> PageSet -- ^ @pageSet@ - a 'PageSet' value
 -> IO ()
printSettingsSetPageSet self pageSet =
  (\(PrintSettings arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_set_page_set argPtr1 arg2)
{-# LINE 814 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)
    ((fromIntegral . fromEnum) pageSet)

-- | Gets the value of 'PrintSettingsDefaultSource'.
printSettingsGetDefaultSource :: (PrintSettingsClass self, GlibString string) => self
 -> IO string -- ^ returns the default source
printSettingsGetDefaultSource self =
  (\(PrintSettings arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_get_default_source argPtr1)
{-# LINE 822 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)
  >>= peekUTFString

-- | Sets the value of 'PrintSettingsDefaultSource'.
printSettingsSetDefaultSource :: (PrintSettingsClass self, GlibString string) => self
 -> string -- ^ @defaultSource@ - the default source
 -> IO ()
printSettingsSetDefaultSource self defaultSource =
  withUTFString defaultSource $ \defaultSourcePtr ->
  (\(PrintSettings arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_set_default_source argPtr1 arg2)
{-# LINE 832 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)
    defaultSourcePtr

-- | Gets the value of 'PrintSettingsMediaType'.
printSettingsGetMediaType :: (PrintSettingsClass self, GlibString string) => self
 -> IO string -- ^ returns the media type
printSettingsGetMediaType self =
  (\(PrintSettings arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_get_media_type argPtr1)
{-# LINE 840 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)
  >>= peekUTFString

-- | Sets the value of 'PrintSettingsMediaType'.
printSettingsSetMediaType :: (PrintSettingsClass self, GlibString string) => self
 -> string -- ^ @mediaType@ - the media type
 -> IO ()
printSettingsSetMediaType self mediaType =
  withUTFString mediaType $ \mediaTypePtr ->
  (\(PrintSettings arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_set_media_type argPtr1 arg2)
{-# LINE 850 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)
    mediaTypePtr

-- | Gets the value of 'PrintSettingsDither'.
printSettingsGetDither :: (PrintSettingsClass self, GlibString string) => self
 -> IO string -- ^ returns the dithering that is used
printSettingsGetDither self =
  (\(PrintSettings arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_get_dither argPtr1)
{-# LINE 858 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)
  >>= peekUTFString

-- | Sets the value of 'PrintSettingsDither'.
printSettingsSetDither :: (PrintSettingsClass self, GlibString string) => self
 -> string -- ^ @dither@ - the dithering that is used
 -> IO ()
printSettingsSetDither self dither =
  withUTFString dither $ \ditherPtr ->
  (\(PrintSettings arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_set_dither argPtr1 arg2)
{-# LINE 868 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)
    ditherPtr

-- | Gets the value of 'PrintSettingsFinishings'.
printSettingsGetFinishings :: (PrintSettingsClass self, GlibString string) => self
 -> IO string -- ^ returns the finishings
printSettingsGetFinishings self =
  (\(PrintSettings arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_get_finishings argPtr1)
{-# LINE 876 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)
  >>= peekUTFString

-- | Sets the value of 'PrintSettingsFinishings'.
printSettingsSetFinishings :: (PrintSettingsClass self, GlibString string) => self
 -> string -- ^ @finishings@ - the finishings
 -> IO ()
printSettingsSetFinishings self finishings =
  withUTFString finishings $ \finishingsPtr ->
  (\(PrintSettings arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_set_finishings argPtr1 arg2)
{-# LINE 886 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)
    finishingsPtr

-- | Gets the value of 'PrintSettingsOutputBin'.
printSettingsGetOutputBin :: (PrintSettingsClass self, GlibString string) => self
 -> IO string -- ^ returns the output bin
printSettingsGetOutputBin self =
  (\(PrintSettings arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_get_output_bin argPtr1)
{-# LINE 894 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)
  >>= peekUTFString

-- | Sets the value of 'PrintSettingsOutputBin'.
printSettingsSetOutputBin :: (PrintSettingsClass self, GlibString string) => self
 -> string -- ^ @outputBin@ - the output bin
 -> IO ()
printSettingsSetOutputBin self outputBin =
  withUTFString outputBin $ \outputBinPtr ->
  (\(PrintSettings arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_set_output_bin argPtr1 arg2)
{-# LINE 904 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)
    outputBinPtr



-- | Reads the print settings from @fileName@. See 'printSettingsToFile'.
--
-- * Available since Gtk+ version 2.14
--
printSettingsLoadFile :: (PrintSettingsClass self, GlibString string) => self
 -> string -- ^ @fileName@ - the filename to read the settings from
 -> IO Bool -- ^ returns @True@ on success
printSettingsLoadFile self fileName =
  liftM toBool $
  propagateGError $ \errorPtr ->
  withUTFString fileName $ \fileNamePtr ->
  (\(PrintSettings arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_load_file argPtr1 arg2 arg3)
{-# LINE 921 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)
    fileNamePtr
    errorPtr




-- | This function saves the print settings from @settings@ to @fileName@.
--
-- * Available since Gtk+ version 2.12
--
printSettingsToFile :: (PrintSettingsClass self, GlibString string) => self
 -> string -- ^ @fileName@ - the file to save to
 -> IO Bool -- ^ returns @True@ on success
printSettingsToFile self fileName =
  liftM toBool $
  propagateGError $ \errorPtr ->
  withUTFString fileName $ \fileNamePtr ->
  (\(PrintSettings arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_print_settings_to_file argPtr1 arg2 arg3)
{-# LINE 940 "./Graphics/UI/Gtk/Printing/PrintSettings.chs" #-}
    (toPrintSettings self)
    fileNamePtr
    errorPtr


-- | Obtain the value of 'PrintSettingsPrinter'.
printSettingsPrinter :: (PrintSettingsClass self, GlibString string) => Attr self string
printSettingsPrinter = newAttr
  printSettingsGetPrinter
  printSettingsSetPrinter

-- | The value of ''PrintSettingsOrientation'', converted to a 'PageOrientation'.
printSettingsOrientation :: PrintSettingsClass self => Attr self PageOrientation
printSettingsOrientation = newAttr
  printSettingsGetOrientation
  printSettingsSetOrientation

-- | The value of 'PrintSettingsPaperFormat', converted to a 'PaperSize'.
printSettingsPaperSize :: PrintSettingsClass self => Attr self PaperSize
printSettingsPaperSize = newAttr
  printSettingsGetPaperSize
  printSettingsSetPaperSize

-- | The value of ''PrintSettingsUseColor''.
printSettingsUseColor :: PrintSettingsClass self => Attr self Bool
printSettingsUseColor = newAttr
  printSettingsGetUseColor
  printSettingsSetUseColor

-- | The value of ''PrintSettingsCollate''.
printSettingsCollate :: PrintSettingsClass self => Attr self Bool
printSettingsCollate = newAttr
  printSettingsGetCollate
  printSettingsSetCollate

-- | The value of ''PrintSettingsReverse''.
printSettingsReverse :: PrintSettingsClass self => Attr self Bool
printSettingsReverse = newAttr
  printSettingsGetReverse
  printSettingsSetReverse

-- | The value of ''PrintSettingsDuplex''.
printSettingsDuplex :: PrintSettingsClass self => Attr self PrintDuplex
printSettingsDuplex = newAttr
  printSettingsGetDuplex
  printSettingsSetDuplex

-- | The value of ''PrintSettingsQuality''.
printSettingsQuality :: PrintSettingsClass self => Attr self PrintQuality
printSettingsQuality = newAttr
  printSettingsGetQuality
  printSettingsSetQuality

-- | The value of 'PrintSettingsNCopies'.
printSettingsNCopies :: PrintSettingsClass self => Attr self Int
printSettingsNCopies = newAttr
  printSettingsGetNCopies
  printSettingsSetNCopies

-- | The value of 'PrintSettingsNumberUp'.
printSettingsNumberUp :: PrintSettingsClass self => Attr self Int
printSettingsNumberUp = newAttr
  printSettingsGetNumberUp
  printSettingsSetNumberUp

-- | The value of 'PrintSettingsResolution'.
printSettingsResolution :: PrintSettingsClass self => Attr self Int
printSettingsResolution = newAttr
  printSettingsGetResolution
  printSettingsSetResolution

-- | The value of 'PrintSettingsScale'.
printSettingsScale :: PrintSettingsClass self => Attr self Double
printSettingsScale = newAttr
  printSettingsGetScale
  printSettingsSetScale

-- | The value of 'PrintSettingsPrintPages'.
printSettingsPrintPages :: PrintSettingsClass self => Attr self PrintPages
printSettingsPrintPages = newAttr
  printSettingsGetPrintPages
  printSettingsSetPrintPages

-- | The value of 'PrintSettingsPageSet'.
printSettingsPageSet :: PrintSettingsClass self => Attr self PageSet
printSettingsPageSet = newAttr
  printSettingsGetPageSet
  printSettingsSetPageSet

-- | The value of 'PrintSettingsDefaultSource'.
printSettingsDefaultSource :: (PrintSettingsClass self, GlibString string) => Attr self string
printSettingsDefaultSource = newAttr
  printSettingsGetDefaultSource
  printSettingsSetDefaultSource

-- | The value of 'PrintSettingsMediaType'.
printSettingsMediaType :: (PrintSettingsClass self, GlibString string) => Attr self string
printSettingsMediaType = newAttr
  printSettingsGetMediaType
  printSettingsSetMediaType

-- | The value of 'PrintSettingsDither'.
printSettingsDither :: (PrintSettingsClass self, GlibString string) => Attr self string
printSettingsDither = newAttr
  printSettingsGetDither
  printSettingsSetDither

-- | The value of 'PrintSettingsFinishings'.
printSettingsFinishings :: (PrintSettingsClass self, GlibString string) => Attr self string
printSettingsFinishings = newAttr
  printSettingsGetFinishings
  printSettingsSetFinishings

-- | The value of 'PrintSettingsOutputBin'.
printSettingsOutputBin :: (PrintSettingsClass self, GlibString string) => Attr self string
printSettingsOutputBin = newAttr
  printSettingsGetOutputBin
  printSettingsSetOutputBin


-- | The value of 'PrintSettingsNumberUpLayout'.
printSettingsNumberUpLayout :: PrintSettingsClass self => Attr self NumberUpLayout
printSettingsNumberUpLayout = newAttr
  printSettingsGetNumberUpLayout
  printSettingsSetNumberUpLayout



-- | The value of 'PrintSettingsPrinterLpi'.
printSettingsPrinterLpi :: PrintSettingsClass self => Attr self Double
printSettingsPrinterLpi = newAttr
  printSettingsGetPrinterLpi
  printSettingsSetPrinterLpi

foreign import ccall safe "gtk_print_settings_new"
  gtk_print_settings_new :: (IO (Ptr PrintSettings))

foreign import ccall safe "gtk_print_settings_new_from_file"
  gtk_print_settings_new_from_file :: ((Ptr CChar) -> ((Ptr (Ptr ())) -> (IO (Ptr PrintSettings))))

foreign import ccall safe "gtk_print_settings_copy"
  gtk_print_settings_copy :: ((Ptr PrintSettings) -> (IO (Ptr PrintSettings)))

foreign import ccall safe "gtk_print_settings_has_key"
  gtk_print_settings_has_key :: ((Ptr PrintSettings) -> ((Ptr CChar) -> (IO CInt)))

foreign import ccall safe "gtk_print_settings_get"
  gtk_print_settings_get :: ((Ptr PrintSettings) -> ((Ptr CChar) -> (IO (Ptr CChar))))

foreign import ccall safe "gtk_print_settings_set"
  gtk_print_settings_set :: ((Ptr PrintSettings) -> ((Ptr CChar) -> ((Ptr CChar) -> (IO ()))))

foreign import ccall safe "gtk_print_settings_unset"
  gtk_print_settings_unset :: ((Ptr PrintSettings) -> ((Ptr CChar) -> (IO ())))

foreign import ccall safe "gtk_print_settings_foreach"
  gtk_print_settings_foreach :: ((Ptr PrintSettings) -> ((FunPtr ((Ptr CChar) -> ((Ptr CChar) -> ((Ptr ()) -> (IO ()))))) -> ((Ptr ()) -> (IO ()))))

foreign import ccall safe "gtk_print_settings_get_bool"
  gtk_print_settings_get_bool :: ((Ptr PrintSettings) -> ((Ptr CChar) -> (IO CInt)))

foreign import ccall safe "gtk_print_settings_set_bool"
  gtk_print_settings_set_bool :: ((Ptr PrintSettings) -> ((Ptr CChar) -> (CInt -> (IO ()))))

foreign import ccall safe "gtk_print_settings_get_double"
  gtk_print_settings_get_double :: ((Ptr PrintSettings) -> ((Ptr CChar) -> (IO CDouble)))

foreign import ccall safe "gtk_print_settings_get_double_with_default"
  gtk_print_settings_get_double_with_default :: ((Ptr PrintSettings) -> ((Ptr CChar) -> (CDouble -> (IO CDouble))))

foreign import ccall safe "gtk_print_settings_set_double"
  gtk_print_settings_set_double :: ((Ptr PrintSettings) -> ((Ptr CChar) -> (CDouble -> (IO ()))))

foreign import ccall safe "gtk_print_settings_get_length"
  gtk_print_settings_get_length :: ((Ptr PrintSettings) -> ((Ptr CChar) -> (CInt -> (IO CDouble))))

foreign import ccall safe "gtk_print_settings_set_length"
  gtk_print_settings_set_length :: ((Ptr PrintSettings) -> ((Ptr CChar) -> (CDouble -> (CInt -> (IO ())))))

foreign import ccall safe "gtk_print_settings_get_int"
  gtk_print_settings_get_int :: ((Ptr PrintSettings) -> ((Ptr CChar) -> (IO CInt)))

foreign import ccall safe "gtk_print_settings_get_int_with_default"
  gtk_print_settings_get_int_with_default :: ((Ptr PrintSettings) -> ((Ptr CChar) -> (CInt -> (IO CInt))))

foreign import ccall safe "gtk_print_settings_set_int"
  gtk_print_settings_set_int :: ((Ptr PrintSettings) -> ((Ptr CChar) -> (CInt -> (IO ()))))

foreign import ccall safe "gtk_print_settings_get_printer"
  gtk_print_settings_get_printer :: ((Ptr PrintSettings) -> (IO (Ptr CChar)))

foreign import ccall safe "gtk_print_settings_set_printer"
  gtk_print_settings_set_printer :: ((Ptr PrintSettings) -> ((Ptr CChar) -> (IO ())))

foreign import ccall safe "gtk_print_settings_get_orientation"
  gtk_print_settings_get_orientation :: ((Ptr PrintSettings) -> (IO CInt))

foreign import ccall safe "gtk_print_settings_set_orientation"
  gtk_print_settings_set_orientation :: ((Ptr PrintSettings) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_print_settings_get_paper_size"
  gtk_print_settings_get_paper_size :: ((Ptr PrintSettings) -> (IO (Ptr ())))

foreign import ccall safe "gtk_print_settings_set_paper_size"
  gtk_print_settings_set_paper_size :: ((Ptr PrintSettings) -> ((Ptr ()) -> (IO ())))

foreign import ccall safe "gtk_print_settings_get_paper_width"
  gtk_print_settings_get_paper_width :: ((Ptr PrintSettings) -> (CInt -> (IO CDouble)))

foreign import ccall safe "gtk_print_settings_set_paper_width"
  gtk_print_settings_set_paper_width :: ((Ptr PrintSettings) -> (CDouble -> (CInt -> (IO ()))))

foreign import ccall safe "gtk_print_settings_get_paper_height"
  gtk_print_settings_get_paper_height :: ((Ptr PrintSettings) -> (CInt -> (IO CDouble)))

foreign import ccall safe "gtk_print_settings_set_paper_height"
  gtk_print_settings_set_paper_height :: ((Ptr PrintSettings) -> (CDouble -> (CInt -> (IO ()))))

foreign import ccall safe "gtk_print_settings_get_use_color"
  gtk_print_settings_get_use_color :: ((Ptr PrintSettings) -> (IO CInt))

foreign import ccall safe "gtk_print_settings_set_use_color"
  gtk_print_settings_set_use_color :: ((Ptr PrintSettings) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_print_settings_get_collate"
  gtk_print_settings_get_collate :: ((Ptr PrintSettings) -> (IO CInt))

foreign import ccall safe "gtk_print_settings_set_collate"
  gtk_print_settings_set_collate :: ((Ptr PrintSettings) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_print_settings_get_reverse"
  gtk_print_settings_get_reverse :: ((Ptr PrintSettings) -> (IO CInt))

foreign import ccall safe "gtk_print_settings_set_reverse"
  gtk_print_settings_set_reverse :: ((Ptr PrintSettings) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_print_settings_get_duplex"
  gtk_print_settings_get_duplex :: ((Ptr PrintSettings) -> (IO CInt))

foreign import ccall safe "gtk_print_settings_set_duplex"
  gtk_print_settings_set_duplex :: ((Ptr PrintSettings) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_print_settings_get_quality"
  gtk_print_settings_get_quality :: ((Ptr PrintSettings) -> (IO CInt))

foreign import ccall safe "gtk_print_settings_set_quality"
  gtk_print_settings_set_quality :: ((Ptr PrintSettings) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_print_settings_get_n_copies"
  gtk_print_settings_get_n_copies :: ((Ptr PrintSettings) -> (IO CInt))

foreign import ccall safe "gtk_print_settings_set_n_copies"
  gtk_print_settings_set_n_copies :: ((Ptr PrintSettings) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_print_settings_get_number_up"
  gtk_print_settings_get_number_up :: ((Ptr PrintSettings) -> (IO CInt))

foreign import ccall safe "gtk_print_settings_set_number_up"
  gtk_print_settings_set_number_up :: ((Ptr PrintSettings) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_print_settings_get_number_up_layout"
  gtk_print_settings_get_number_up_layout :: ((Ptr PrintSettings) -> (IO CInt))

foreign import ccall safe "gtk_print_settings_set_number_up_layout"
  gtk_print_settings_set_number_up_layout :: ((Ptr PrintSettings) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_print_settings_get_resolution"
  gtk_print_settings_get_resolution :: ((Ptr PrintSettings) -> (IO CInt))

foreign import ccall safe "gtk_print_settings_set_resolution"
  gtk_print_settings_set_resolution :: ((Ptr PrintSettings) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_print_settings_set_resolution_xy"
  gtk_print_settings_set_resolution_xy :: ((Ptr PrintSettings) -> (CInt -> (CInt -> (IO ()))))

foreign import ccall safe "gtk_print_settings_get_resolution_x"
  gtk_print_settings_get_resolution_x :: ((Ptr PrintSettings) -> (IO CInt))

foreign import ccall safe "gtk_print_settings_get_resolution_y"
  gtk_print_settings_get_resolution_y :: ((Ptr PrintSettings) -> (IO CInt))

foreign import ccall safe "gtk_print_settings_get_printer_lpi"
  gtk_print_settings_get_printer_lpi :: ((Ptr PrintSettings) -> (IO CDouble))

foreign import ccall safe "gtk_print_settings_set_printer_lpi"
  gtk_print_settings_set_printer_lpi :: ((Ptr PrintSettings) -> (CDouble -> (IO ())))

foreign import ccall safe "gtk_print_settings_get_scale"
  gtk_print_settings_get_scale :: ((Ptr PrintSettings) -> (IO CDouble))

foreign import ccall safe "gtk_print_settings_set_scale"
  gtk_print_settings_set_scale :: ((Ptr PrintSettings) -> (CDouble -> (IO ())))

foreign import ccall safe "gtk_print_settings_get_print_pages"
  gtk_print_settings_get_print_pages :: ((Ptr PrintSettings) -> (IO CInt))

foreign import ccall safe "gtk_print_settings_set_print_pages"
  gtk_print_settings_set_print_pages :: ((Ptr PrintSettings) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_print_settings_get_page_set"
  gtk_print_settings_get_page_set :: ((Ptr PrintSettings) -> (IO CInt))

foreign import ccall safe "gtk_print_settings_set_page_set"
  gtk_print_settings_set_page_set :: ((Ptr PrintSettings) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_print_settings_get_default_source"
  gtk_print_settings_get_default_source :: ((Ptr PrintSettings) -> (IO (Ptr CChar)))

foreign import ccall safe "gtk_print_settings_set_default_source"
  gtk_print_settings_set_default_source :: ((Ptr PrintSettings) -> ((Ptr CChar) -> (IO ())))

foreign import ccall safe "gtk_print_settings_get_media_type"
  gtk_print_settings_get_media_type :: ((Ptr PrintSettings) -> (IO (Ptr CChar)))

foreign import ccall safe "gtk_print_settings_set_media_type"
  gtk_print_settings_set_media_type :: ((Ptr PrintSettings) -> ((Ptr CChar) -> (IO ())))

foreign import ccall safe "gtk_print_settings_get_dither"
  gtk_print_settings_get_dither :: ((Ptr PrintSettings) -> (IO (Ptr CChar)))

foreign import ccall safe "gtk_print_settings_set_dither"
  gtk_print_settings_set_dither :: ((Ptr PrintSettings) -> ((Ptr CChar) -> (IO ())))

foreign import ccall safe "gtk_print_settings_get_finishings"
  gtk_print_settings_get_finishings :: ((Ptr PrintSettings) -> (IO (Ptr CChar)))

foreign import ccall safe "gtk_print_settings_set_finishings"
  gtk_print_settings_set_finishings :: ((Ptr PrintSettings) -> ((Ptr CChar) -> (IO ())))

foreign import ccall safe "gtk_print_settings_get_output_bin"
  gtk_print_settings_get_output_bin :: ((Ptr PrintSettings) -> (IO (Ptr CChar)))

foreign import ccall safe "gtk_print_settings_set_output_bin"
  gtk_print_settings_set_output_bin :: ((Ptr PrintSettings) -> ((Ptr CChar) -> (IO ())))

foreign import ccall safe "gtk_print_settings_load_file"
  gtk_print_settings_load_file :: ((Ptr PrintSettings) -> ((Ptr CChar) -> ((Ptr (Ptr ())) -> (IO CInt))))

foreign import ccall safe "gtk_print_settings_to_file"
  gtk_print_settings_to_file :: ((Ptr PrintSettings) -> ((Ptr CChar) -> ((Ptr (Ptr ())) -> (IO CInt))))
