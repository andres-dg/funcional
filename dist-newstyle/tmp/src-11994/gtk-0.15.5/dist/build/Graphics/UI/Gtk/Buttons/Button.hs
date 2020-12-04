
{-# LINE 2 "./Graphics/UI/Gtk/Buttons/Button.chs" #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LINE 3 "./Graphics/UI/Gtk/Buttons/Button.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget Button
--
-- Author : Axel Simon
--
-- Created: 15 May 2001
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
-- A widget that creates a signal when clicked on
--
module Graphics.UI.Gtk.Buttons.Button (
-- * Detail
--
-- | The 'Button' widget is generally used to attach a function to that is
-- called when the button is pressed. The various signals and how to use them
-- are outlined below.
--
-- The 'Button' widget can hold any valid child widget. That is it can hold
-- most any other standard 'Widget'. The most commonly used child is the
-- 'Label'.

-- * Class Hierarchy
-- |
-- @
-- | 'GObject'
-- | +----'Object'
-- | +----'Widget'
-- | +----'Container'
-- | +----'Bin'
-- | +----Button
-- | +----'ToggleButton'
-- | +----'ColorButton'
-- | +----'FontButton'
-- | +----'OptionMenu'
-- @

-- * Types
  Button,
  ButtonClass,
  castToButton, gTypeButton,
  toButton,

-- * Constructors
  buttonNew,
  buttonNewWithLabel,
  buttonNewWithMnemonic,
  buttonNewFromStock,

-- * Methods
  buttonPressed,
  buttonReleased,
  buttonClicked,
  buttonEnter,
  buttonLeave,
  ReliefStyle(..),
  buttonSetRelief,
  buttonGetRelief,
  buttonSetLabel,
  buttonGetLabel,
  buttonSetUseStock,
  buttonGetUseStock,
  buttonSetUseUnderline,
  buttonGetUseUnderline,

  buttonSetFocusOnClick,
  buttonGetFocusOnClick,
  buttonSetAlignment,
  buttonGetAlignment,


  buttonGetImage,
  buttonSetImage,


  PositionType(..),
  buttonSetImagePosition,
  buttonGetImagePosition,


  buttonGetEventWindow,


-- * Attributes
  buttonLabel,
  buttonUseUnderline,
  buttonUseStock,

  buttonFocusOnClick,

  buttonRelief,

  buttonXalign,
  buttonYalign,


  buttonImage,


  buttonImagePosition,


-- * Signals
  buttonActivated,

-- * Deprecated

  onButtonActivate,
  afterButtonActivate,
  onClicked,
  afterClicked,
  onEnter,
  afterEnter,
  onLeave,
  afterLeave,
  onPressed,
  afterPressed,
  onReleased,
  afterReleased

  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.UI.Gtk.Types
{-# LINE 148 "./Graphics/UI/Gtk/Buttons/Button.chs" #-}
import Graphics.UI.Gtk.Signals
{-# LINE 149 "./Graphics/UI/Gtk/Buttons/Button.chs" #-}
import Graphics.UI.Gtk.General.Enums (ReliefStyle(..), PositionType(..))
import Graphics.UI.Gtk.General.StockItems


{-# LINE 153 "./Graphics/UI/Gtk/Buttons/Button.chs" #-}

--------------------
-- Constructors

-- | Creates a new 'Button' widget. To add a child widget to the button, use
-- 'Graphics.UI.Gtk.Abstract.Container.containerAdd'.
--
buttonNew :: IO Button
buttonNew =
  makeNewObject mkButton $
  liftM (castPtr :: Ptr Widget -> Ptr Button) $
  gtk_button_new
{-# LINE 165 "./Graphics/UI/Gtk/Buttons/Button.chs" #-}

-- | Creates a 'Button' widget with a 'Label' child containing the given text.
--
buttonNewWithLabel :: GlibString string
 => string -- ^ @label@ - The text you want the 'Label' to hold.
 -> IO Button
buttonNewWithLabel label =
  makeNewObject mkButton $
  liftM (castPtr :: Ptr Widget -> Ptr Button) $
  withUTFString label $ \labelPtr ->
  gtk_button_new_with_label
{-# LINE 176 "./Graphics/UI/Gtk/Buttons/Button.chs" #-}
    labelPtr

-- | Creates a new 'Button' containing a label. If characters in @label@ are
-- preceded by an underscore, they are underlined. If you need a literal
-- underscore character in a label, use \'__\' (two underscores). The first
-- underlined character represents a keyboard accelerator called a mnemonic.
-- Pressing Alt and that key activates the button.
--
buttonNewWithMnemonic :: GlibString string
 => string -- ^ @label@ - The text of the button, with an underscore in
              -- front of the mnemonic character
 -> IO Button
buttonNewWithMnemonic label =
  makeNewObject mkButton $
  liftM (castPtr :: Ptr Widget -> Ptr Button) $
  withUTFString label $ \labelPtr ->
  gtk_button_new_with_mnemonic
{-# LINE 193 "./Graphics/UI/Gtk/Buttons/Button.chs" #-}
    labelPtr

-- | Creates a new 'Button' containing the image and text from a stock item.
--
-- If @stockId@ is unknown, then it will be treated as a mnemonic label (as
-- for 'buttonNewWithMnemonic').
--
buttonNewFromStock ::
    StockId -- ^ @stockId@ - the name of the stock item
 -> IO Button
buttonNewFromStock stockId =
  makeNewObject mkButton $
  liftM (castPtr :: Ptr Widget -> Ptr Button) $
  withUTFString stockId $ \stockIdPtr ->
  throwIfNull "buttonNewFromStock: Invalid stock identifier." $
  gtk_button_new_from_stock
{-# LINE 209 "./Graphics/UI/Gtk/Buttons/Button.chs" #-}
    stockIdPtr

--------------------
-- Methods

-- | Emits the button pressed signal for the given 'Button'.
--
buttonPressed :: ButtonClass self => self -> IO ()
buttonPressed self =
  (\(Button arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_button_pressed argPtr1)
{-# LINE 219 "./Graphics/UI/Gtk/Buttons/Button.chs" #-}
    (toButton self)

-- | Emits the button released signal for the given 'Button'.
--
buttonReleased :: ButtonClass self => self -> IO ()
buttonReleased self =
  (\(Button arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_button_released argPtr1)
{-# LINE 226 "./Graphics/UI/Gtk/Buttons/Button.chs" #-}
    (toButton self)

-- | Emits the button clicked signal for the given 'Button'.
--
-- This is similar to calling 'buttonPressed' and 'buttonReleased' in sequence.
--
buttonClicked :: ButtonClass self => self -> IO ()
buttonClicked self =
  (\(Button arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_button_clicked argPtr1)
{-# LINE 235 "./Graphics/UI/Gtk/Buttons/Button.chs" #-}
    (toButton self)

-- | Emit the cursor enters signal to the button.
--
buttonEnter :: ButtonClass self => self -> IO ()
buttonEnter self =
  (\(Button arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_button_enter argPtr1)
{-# LINE 242 "./Graphics/UI/Gtk/Buttons/Button.chs" #-}
    (toButton self)

-- | Emit the cursor leaves signal to the button.
--
buttonLeave :: ButtonClass self => self -> IO ()
buttonLeave self =
  (\(Button arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_button_leave argPtr1)
{-# LINE 249 "./Graphics/UI/Gtk/Buttons/Button.chs" #-}
    (toButton self)

-- | Sets the relief style of the edges of the given 'Button' widget. Three
-- styles exist, 'ReliefNormal', 'ReliefHalf', 'ReliefNone'. The default style
-- is, as one can guess, 'ReliefNormal'.
--
buttonSetRelief :: ButtonClass self => self
 -> ReliefStyle -- ^ @newstyle@ - The 'ReliefStyle' as described above.
 -> IO ()
buttonSetRelief self newstyle =
  (\(Button arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_button_set_relief argPtr1 arg2)
{-# LINE 260 "./Graphics/UI/Gtk/Buttons/Button.chs" #-}
    (toButton self)
    ((fromIntegral . fromEnum) newstyle)

-- | Returns the current relief style of the given 'Button'.
--
buttonGetRelief :: ButtonClass self => self
 -> IO ReliefStyle -- ^ returns The current 'ReliefStyle'
buttonGetRelief self =
  liftM (toEnum . fromIntegral) $
  (\(Button arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_button_get_relief argPtr1)
{-# LINE 270 "./Graphics/UI/Gtk/Buttons/Button.chs" #-}
    (toButton self)

-- | Sets the text of the label of the button. This text is also used
-- to select the stock item if 'buttonSetUseStock' is used.
--
-- This will also clear any previously set labels.
--
buttonSetLabel :: (ButtonClass self, GlibString string) => self -> string -> IO ()
buttonSetLabel self label =
  withUTFString label $ \labelPtr ->
  (\(Button arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_button_set_label argPtr1 arg2)
{-# LINE 281 "./Graphics/UI/Gtk/Buttons/Button.chs" #-}
    (toButton self)
    labelPtr

-- | Gets the text from the label of the button, as set by
-- 'buttonSetLabel'. If the label text has not been set the return value will
-- be @\"\"@.
-- This will be the case if you create an empty button with 'buttonNew' to use
-- as a container.
--
buttonGetLabel :: (ButtonClass self, GlibString string) => self -> IO string
buttonGetLabel self = do
  strPtr <- (\(Button arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_button_get_label argPtr1)
{-# LINE 293 "./Graphics/UI/Gtk/Buttons/Button.chs" #-}
    (toButton self)
  if strPtr==nullPtr then return "" else peekUTFString strPtr

-- | If true, the label set on the button is used as a stock id to select the
-- stock item for the button.
--
-- Setting this property to @True@ will make the button lookup its label in
-- the table of stock items. If there is a match, the button will use the
-- stock item instead of the label. You need to set this flag before you
-- change the label.
--
buttonSetUseStock :: ButtonClass self => self
 -> Bool -- ^ @useStock@ - @True@ if the button should use a stock item
 -> IO ()
buttonSetUseStock self useStock =
  (\(Button arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_button_set_use_stock argPtr1 arg2)
{-# LINE 309 "./Graphics/UI/Gtk/Buttons/Button.chs" #-}
    (toButton self)
    (fromBool useStock)

-- | Returns whether the button label is a stock item.
--
buttonGetUseStock :: ButtonClass self => self
 -> IO Bool -- ^ returns @True@ if the button label is used to select a stock
            -- item instead of being used directly as the label text.
buttonGetUseStock self =
  liftM toBool $
  (\(Button arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_button_get_use_stock argPtr1)
{-# LINE 320 "./Graphics/UI/Gtk/Buttons/Button.chs" #-}
    (toButton self)

-- | If true, an underline in the text of the button label indicates the next
-- character should be used for the mnemonic accelerator key.
--
-- Setting this property will make the button join any underline character
-- into the following letter and inserting this letter as a keyboard shortcut.
-- You need to set this flag before you change the label.
--
buttonSetUseUnderline :: ButtonClass self => self
 -> Bool -- ^ @useUnderline@ - @True@ if underlines in the text indicate
          -- mnemonics
 -> IO ()
buttonSetUseUnderline self useUnderline =
  (\(Button arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_button_set_use_underline argPtr1 arg2)
{-# LINE 335 "./Graphics/UI/Gtk/Buttons/Button.chs" #-}
    (toButton self)
    (fromBool useUnderline)

-- | Returns whether an embedded underline in the button label indicates a
-- mnemonic. See 'buttonSetUseUnderline'.
--
buttonGetUseUnderline :: ButtonClass self => self
 -> IO Bool -- ^ returns @True@ if an embedded underline in the button label
            -- indicates the mnemonic accelerator keys.
buttonGetUseUnderline self =
  liftM toBool $
  (\(Button arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_button_get_use_underline argPtr1)
{-# LINE 347 "./Graphics/UI/Gtk/Buttons/Button.chs" #-}
    (toButton self)


-- | Sets whether the button will grab focus when it is clicked with the
-- mouse. Making mouse clicks not grab focus is useful in places like toolbars
-- where you don't want the keyboard focus removed from the main area of the
-- application.
--
-- * Available since Gtk version 2.4
--
buttonSetFocusOnClick :: ButtonClass self => self
 -> Bool -- ^ @focusOnClick@ - whether the button grabs focus when clicked
          -- with the mouse
 -> IO ()
buttonSetFocusOnClick self focusOnClick =
  (\(Button arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_button_set_focus_on_click argPtr1 arg2)
{-# LINE 363 "./Graphics/UI/Gtk/Buttons/Button.chs" #-}
    (toButton self)
    (fromBool focusOnClick)

-- | Returns whether the button grabs focus when it is clicked with the mouse.
-- See 'buttonSetFocusOnClick'.
--
-- * Available since Gtk version 2.4
--
buttonGetFocusOnClick :: ButtonClass self => self
 -> IO Bool -- ^ returns @True@ if the button grabs focus when it is clicked
            -- with the mouse.
buttonGetFocusOnClick self =
  liftM toBool $
  (\(Button arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_button_get_focus_on_click argPtr1)
{-# LINE 377 "./Graphics/UI/Gtk/Buttons/Button.chs" #-}
    (toButton self)

-- | Sets the alignment of the child. This has no effect unless the child
-- derives from 'Misc' or 'Alignment'.
--
-- * Available since Gtk version 2.4
--
buttonSetAlignment :: ButtonClass self => self
 -> (Float, Float) -- ^ @(xalign, yalign)@ - the horizontal position of the
                   -- child (0.0 is left aligned, 1.0 is right aligned) and
                   -- the vertical position of the child (0.0 is top aligned,
                   -- 1.0 is bottom aligned)
 -> IO ()
buttonSetAlignment self (xalign, yalign) =
  (\(Button arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_button_set_alignment argPtr1 arg2 arg3)
{-# LINE 392 "./Graphics/UI/Gtk/Buttons/Button.chs" #-}
    (toButton self)
    (realToFrac xalign)
    (realToFrac yalign)

-- | Gets the alignment of the child in the button.
--
-- * Available since Gtk version 2.4
--
buttonGetAlignment :: ButtonClass self => self
 -> IO (Float, Float) -- ^ @(xalign, yalign)@ - horizontal and vertical
                      -- alignment
buttonGetAlignment self =
  alloca $ \xalignPtr ->
  alloca $ \yalignPtr -> do
  (\(Button arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_button_get_alignment argPtr1 arg2 arg3)
{-# LINE 407 "./Graphics/UI/Gtk/Buttons/Button.chs" #-}
    (toButton self)
    xalignPtr
    yalignPtr
  xalign <- peek xalignPtr
  yalign <- peek yalignPtr
  return (realToFrac xalign, realToFrac yalign)



-- | Gets the widget that is currenty set as the image of the button. This may
-- have been explicitly set by 'buttonSetImage' or constructed by
-- 'buttonNewFromStock'.
--
-- * Available since Gtk+ version 2.6
--
buttonGetImage :: ButtonClass self => self
 -> IO (Maybe Widget) -- ^ a 'Widget' or @Nothing@ in case there is no image
buttonGetImage self =
  maybeNull (makeNewObject mkWidget) $
  (\(Button arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_button_get_image argPtr1)
{-# LINE 427 "./Graphics/UI/Gtk/Buttons/Button.chs" #-}
    (toButton self)

-- | Set the image of the button to the given widget. Note that it depends on
-- the \"gtk-button-images\" setting whether the image will be displayed or not.
--
-- * Available since Gtk+ version 2.6
--
buttonSetImage :: (ButtonClass self, WidgetClass image) => self
 -> image -- ^ a widget to set as the image for the button
 -> IO ()
buttonSetImage self image =
  (\(Button arg1) (Widget arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_button_set_image argPtr1 argPtr2)
{-# LINE 439 "./Graphics/UI/Gtk/Buttons/Button.chs" #-}
    (toButton self)
    (toWidget image)



-- %hash c:e7a6 d:7a76
-- | Sets the position of the image relative to the text inside the button.
--
-- * Available since Gtk+ version 2.10
--
buttonSetImagePosition :: ButtonClass self => self
 -> PositionType -- ^ @position@ - the position
 -> IO ()
buttonSetImagePosition self position =
  (\(Button arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_button_set_image_position argPtr1 arg2)
{-# LINE 454 "./Graphics/UI/Gtk/Buttons/Button.chs" #-}
    (toButton self)
    ((fromIntegral . fromEnum) position)

-- %hash c:3841 d:1f6a
-- | Gets the position of the image relative to the text inside the button.
--
-- * Available since Gtk+ version 2.10
--
buttonGetImagePosition :: ButtonClass self => self
 -> IO PositionType -- ^ returns the position
buttonGetImagePosition self =
  liftM (toEnum . fromIntegral) $
  (\(Button arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_button_get_image_position argPtr1)
{-# LINE 467 "./Graphics/UI/Gtk/Buttons/Button.chs" #-}
    (toButton self)



-- | Returns the button's event window if it is realized, 'Nothing' otherwise.
--
-- * Available since Gtk+ version 2.22
--
buttonGetEventWindow :: ButtonClass self => self
                       -> IO (Maybe DrawWindow) -- ^ returns button's event window or 'Nothing'
buttonGetEventWindow self =
  maybeNull (makeNewGObject mkDrawWindow) $
  (\(Button arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_button_get_event_window argPtr1)
{-# LINE 480 "./Graphics/UI/Gtk/Buttons/Button.chs" #-}
    (toButton self)


--------------------
-- Attributes

-- | Text of the label widget inside the button, if the button contains a
-- label widget.
--
-- Default value: @\"\"@
--
buttonLabel :: (ButtonClass self, GlibString string) => Attr self string
buttonLabel = newAttr
  buttonGetLabel
  buttonSetLabel

-- | If set, an underline in the text indicates the next character should be
-- used for the mnemonic accelerator key.
--
-- Default value: @False@
--
buttonUseUnderline :: ButtonClass self => Attr self Bool
buttonUseUnderline = newAttr
  buttonGetUseUnderline
  buttonSetUseUnderline

-- | If set, the label is used to pick a stock item instead of being
-- displayed.
--
-- Default value: @False@
--
buttonUseStock :: ButtonClass self => Attr self Bool
buttonUseStock = newAttr
  buttonGetUseStock
  buttonSetUseStock


-- | Whether the button grabs focus when it is clicked with the mouse.
--
-- Default value: @True@
--
buttonFocusOnClick :: ButtonClass self => Attr self Bool
buttonFocusOnClick = newAttr
  buttonGetFocusOnClick
  buttonSetFocusOnClick


-- | The border relief style.
--
-- Default value: 'ReliefNormal'
--
buttonRelief :: ButtonClass self => Attr self ReliefStyle
buttonRelief = newAttr
  buttonGetRelief
  buttonSetRelief


-- | If the child of the button is a 'Misc' or 'Alignment', this property can
-- be used to control it's horizontal alignment. 0.0 is left aligned, 1.0 is
-- right aligned.
--
-- Allowed values: [0,1]
--
-- Default value: 0.5
--
buttonXalign :: ButtonClass self => Attr self Float
buttonXalign = newAttrFromFloatProperty "xalign"

-- | If the child of the button is a 'Misc' or 'Alignment', this property can
-- be used to control it's vertical alignment. 0.0 is top aligned, 1.0 is
-- bottom aligned.
--
-- Allowed values: [0,1]
--
-- Default value: 0.5
--
buttonYalign :: ButtonClass self => Attr self Float
buttonYalign = newAttrFromFloatProperty "yalign"



-- | Child widget to appear next to the button text.
--
-- * Available since Gtk version 2.6
--
buttonImage :: (ButtonClass self, WidgetClass image) => ReadWriteAttr self (Maybe Widget) image
buttonImage = newAttr
  buttonGetImage
  buttonSetImage



-- %hash c:20f4 d:8ca6
-- | The position of the image relative to the text inside the button.
--
-- Default value: 'PosLeft'
--
-- * Available since Gtk+ version 2.10
--
buttonImagePosition :: ButtonClass self => Attr self PositionType
buttonImagePosition = newAttrFromEnumProperty "image-position"
                        gtk_position_type_get_type
{-# LINE 582 "./Graphics/UI/Gtk/Buttons/Button.chs" #-}


--------------------
-- Signals

-- %hash c:b660 d:ab72
-- | Emitted when the button has been activated (pressed and released).
--
buttonActivated :: ButtonClass self => Signal self (IO ())
buttonActivated = Signal (connect_NONE__NONE "clicked")


--------------------
-- Deprecated Signals


-- | The button has been depressed (but not
-- necessarily released yet). See @clicked@ signal.
--
onButtonActivate, afterButtonActivate :: ButtonClass b => b -> IO () ->
                                         IO (ConnectId b)
onButtonActivate = connect_NONE__NONE "activate" False
afterButtonActivate = connect_NONE__NONE "activate" True

-- | The button was clicked. This is only emitted if
-- the mouse cursor was over the button when it was released.
--
onClicked, afterClicked :: ButtonClass b => b -> IO () -> IO (ConnectId b)
onClicked = connect_NONE__NONE "clicked" False
afterClicked = connect_NONE__NONE "clicked" True

-- | The cursor enters the button box.
--
onEnter, afterEnter :: ButtonClass b => b -> IO () -> IO (ConnectId b)
onEnter = connect_NONE__NONE "enter" False
afterEnter = connect_NONE__NONE "enter" True

-- | The cursor leaves the button box.
--
onLeave, afterLeave :: ButtonClass b => b -> IO () -> IO (ConnectId b)
onLeave = connect_NONE__NONE "leave" False
afterLeave = connect_NONE__NONE "leave" True

-- | The button is pressed.
--
onPressed, afterPressed :: ButtonClass b => b -> IO () -> IO (ConnectId b)
onPressed = connect_NONE__NONE "pressed" False
afterPressed = connect_NONE__NONE "pressed" True

-- | The button is released.
--
onReleased, afterReleased :: ButtonClass b => b -> IO () -> IO (ConnectId b)
onReleased = connect_NONE__NONE "released" False
afterReleased = connect_NONE__NONE "released" True

foreign import ccall unsafe "gtk_button_new"
  gtk_button_new :: (IO (Ptr Widget))

foreign import ccall unsafe "gtk_button_new_with_label"
  gtk_button_new_with_label :: ((Ptr CChar) -> (IO (Ptr Widget)))

foreign import ccall unsafe "gtk_button_new_with_mnemonic"
  gtk_button_new_with_mnemonic :: ((Ptr CChar) -> (IO (Ptr Widget)))

foreign import ccall unsafe "gtk_button_new_from_stock"
  gtk_button_new_from_stock :: ((Ptr CChar) -> (IO (Ptr Widget)))

foreign import ccall safe "gtk_button_pressed"
  gtk_button_pressed :: ((Ptr Button) -> (IO ()))

foreign import ccall safe "gtk_button_released"
  gtk_button_released :: ((Ptr Button) -> (IO ()))

foreign import ccall safe "gtk_button_clicked"
  gtk_button_clicked :: ((Ptr Button) -> (IO ()))

foreign import ccall safe "gtk_button_enter"
  gtk_button_enter :: ((Ptr Button) -> (IO ()))

foreign import ccall safe "gtk_button_leave"
  gtk_button_leave :: ((Ptr Button) -> (IO ()))

foreign import ccall safe "gtk_button_set_relief"
  gtk_button_set_relief :: ((Ptr Button) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_button_get_relief"
  gtk_button_get_relief :: ((Ptr Button) -> (IO CInt))

foreign import ccall safe "gtk_button_set_label"
  gtk_button_set_label :: ((Ptr Button) -> ((Ptr CChar) -> (IO ())))

foreign import ccall unsafe "gtk_button_get_label"
  gtk_button_get_label :: ((Ptr Button) -> (IO (Ptr CChar)))

foreign import ccall safe "gtk_button_set_use_stock"
  gtk_button_set_use_stock :: ((Ptr Button) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_button_get_use_stock"
  gtk_button_get_use_stock :: ((Ptr Button) -> (IO CInt))

foreign import ccall safe "gtk_button_set_use_underline"
  gtk_button_set_use_underline :: ((Ptr Button) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_button_get_use_underline"
  gtk_button_get_use_underline :: ((Ptr Button) -> (IO CInt))

foreign import ccall unsafe "gtk_button_set_focus_on_click"
  gtk_button_set_focus_on_click :: ((Ptr Button) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_button_get_focus_on_click"
  gtk_button_get_focus_on_click :: ((Ptr Button) -> (IO CInt))

foreign import ccall unsafe "gtk_button_set_alignment"
  gtk_button_set_alignment :: ((Ptr Button) -> (CFloat -> (CFloat -> (IO ()))))

foreign import ccall unsafe "gtk_button_get_alignment"
  gtk_button_get_alignment :: ((Ptr Button) -> ((Ptr CFloat) -> ((Ptr CFloat) -> (IO ()))))

foreign import ccall safe "gtk_button_get_image"
  gtk_button_get_image :: ((Ptr Button) -> (IO (Ptr Widget)))

foreign import ccall safe "gtk_button_set_image"
  gtk_button_set_image :: ((Ptr Button) -> ((Ptr Widget) -> (IO ())))

foreign import ccall safe "gtk_button_set_image_position"
  gtk_button_set_image_position :: ((Ptr Button) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_button_get_image_position"
  gtk_button_get_image_position :: ((Ptr Button) -> (IO CInt))

foreign import ccall safe "gtk_button_get_event_window"
  gtk_button_get_event_window :: ((Ptr Button) -> (IO (Ptr DrawWindow)))

foreign import ccall unsafe "gtk_position_type_get_type"
  gtk_position_type_get_type :: CULong
