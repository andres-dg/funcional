
{-# LINE 2 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget TextView
--
-- Author : Axel Simon
--
-- Created: 23 February 2002
--
-- Copyright (C) 2002-2005 Axel Simon
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
--
-- If PangoTabArray is bound:
-- Fucntions: textViewSetTabs and textViewGetTabs
-- Properties: textViewTabs
--
-- All on... and after... signales had incorrect names (underscore instead of hypens). Thus
-- they could not have been used in applications and removing them can't break anything.
-- Thus, I've removed them. Also, all key-binding singals are now removed as there is
-- no way to add additional key bindings programatically in a type-safe way, let alone
-- use these signals.
--
-- |
-- Maintainer : gtk2hs-users@lists.sourceforge.net
-- Stability : provisional
-- Portability : portable (depends on GHC)
--
-- Widget that displays a 'TextBuffer'
--
module Graphics.UI.Gtk.Multiline.TextView (
-- * Detail
--
-- | You may wish to begin by reading the text widget conceptual overview
-- which gives an overview of all the objects and data types related to the
-- text widget and how they work together.
--
-- Throughout we distinguish between buffer coordinates which are pixels with
-- the origin at the upper left corner of the first character on the first
-- line. Window coordinates are relative to the top left pixel which is visible
-- in the current 'TextView'. Coordinates from Events are in the latter
-- relation. The conversion can be done with 'textViewWindowToBufferCoords'.

-- * Class Hierarchy
-- |
-- @
-- | 'GObject'
-- | +----'Object'
-- | +----'Widget'
-- | +----'Container'
-- | +----TextView
-- |
-- |
-- | 'GObject'
-- | +----TextChildAnchor
-- @

-- * Types
  TextView,
  TextViewClass,
  TextChildAnchor,
  TextChildAnchorClass,
  castToTextView, gTypeTextView,
  toTextView,
  DeleteType(..),
  DirectionType(..),
  Justification(..),
  MovementStep(..),
  TextWindowType(..),
  WrapMode(..),

-- * Constructors
  textViewNew,
  textViewNewWithBuffer,

-- * Methods
  textViewSetBuffer,
  textViewGetBuffer,
  textViewScrollToMark,
  textViewScrollToIter,
  textViewScrollMarkOnscreen,
  textViewMoveMarkOnscreen,
  textViewPlaceCursorOnscreen,
  textViewGetLineAtY,
  textViewGetLineYrange,
  textViewGetIterAtLocation,
  textViewBufferToWindowCoords,
  textViewWindowToBufferCoords,
  textViewGetWindow,
  textViewGetWindowType,
  textViewSetBorderWindowSize,
  textViewGetBorderWindowSize,
  textViewForwardDisplayLine,
  textViewBackwardDisplayLine,
  textViewForwardDisplayLineEnd,
  textViewBackwardDisplayLineStart,
  textViewStartsDisplayLine,
  textViewMoveVisually,
  textViewAddChildAtAnchor,
  textChildAnchorNew,
  textChildAnchorGetWidgets,
  textChildAnchorGetDeleted,
  textViewAddChildInWindow,
  textViewMoveChild,
  textViewSetWrapMode,
  textViewGetWrapMode,
  textViewSetEditable,
  textViewGetEditable,
  textViewSetCursorVisible,
  textViewGetCursorVisible,
  textViewSetPixelsAboveLines,
  textViewGetPixelsAboveLines,
  textViewSetPixelsBelowLines,
  textViewGetPixelsBelowLines,
  textViewSetPixelsInsideWrap,
  textViewGetPixelsInsideWrap,
  textViewSetJustification,
  textViewGetJustification,
  textViewSetLeftMargin,
  textViewGetLeftMargin,
  textViewSetRightMargin,
  textViewGetRightMargin,
  textViewSetIndent,
  textViewGetIndent,
  textViewGetDefaultAttributes,
  textViewGetVisibleRect,
  textViewGetIterLocation,

  textViewGetIterAtPosition,


  textViewSetOverwrite,
  textViewGetOverwrite,
  textViewSetAcceptsTab,
  textViewGetAcceptsTab,


  textViewGetHadjustment,
  textViewGetVadjustment,
  textViewImContextFilterKeypress,
  textViewResetImContext,


-- * Attributes
  textViewPixelsAboveLines,
  textViewPixelsBelowLines,
  textViewPixelsInsideWrap,
  textViewEditable,
  textViewImModule,
  textViewWrapMode,
  textViewJustification,
  textViewLeftMargin,
  textViewRightMargin,
  textViewIndent,
  textViewCursorVisible,
  textViewBuffer,

  textViewOverwrite,
  textViewAcceptsTab,


-- * Signals
  backspace,
  copyClipboard,
  cutClipboard,
  deleteFromCursor,
  insertAtCursor,
  moveCursor,
  moveViewport,
  moveFocus,
  pageHorizontally,
  pasteClipboard,
  populatePopup,
  selectAll,
  setAnchor,
  setTextViewScrollAdjustments,
  toggleCursorVisible,
  toggleOverwrite,
  textViewPreeditChanged
  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import System.Glib.Properties (newAttrFromStringProperty)
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.UI.Gtk.Gdk.EventM (EventM, EKey)
import Control.Monad.Reader ( ask )
import Control.Monad.Trans ( liftIO )
import Graphics.UI.Gtk.Types
{-# LINE 202 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
import Graphics.UI.Gtk.Signals
{-# LINE 203 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
import Graphics.UI.Gtk.Multiline.Types
{-# LINE 204 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
import Graphics.UI.Gtk.Multiline.TextTag
{-# LINE 205 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
import Graphics.UI.Gtk.General.Enums (TextWindowType(..), DeleteType(..),
                                         DirectionType(..), Justification(..),
                                         MovementStep(..), WrapMode(..),
                                         ScrollStep (..))
import System.Glib.GList (fromGList)
import Graphics.UI.Gtk.General.Structs (Rectangle(..))


{-# LINE 213 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}

--------------------
-- Constructors

-- | Creates a new 'TextView'. If you don't call 'textViewSetBuffer' before
-- using the text view, an empty default buffer will be created for you. Get
-- the buffer with 'textViewGetBuffer'. If you want to specify your own buffer,
-- consider 'textViewNewWithBuffer'.
--
textViewNew :: IO TextView
textViewNew =
  makeNewObject mkTextView $
  liftM (castPtr :: Ptr Widget -> Ptr TextView) $
  gtk_text_view_new
{-# LINE 227 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}

-- | Creates a new 'TextView' widget displaying the buffer @buffer@. One
-- buffer can be shared among many widgets.
--
textViewNewWithBuffer :: TextBufferClass buffer => buffer -> IO TextView
textViewNewWithBuffer buffer =
  makeNewObject mkTextView $
  liftM (castPtr :: Ptr Widget -> Ptr TextView) $
  (\(TextBuffer arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_text_view_new_with_buffer argPtr1)
{-# LINE 236 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextBuffer buffer)

--------------------
-- Methods

-- | Sets the given buffer as the buffer being displayed by the text view.
--
textViewSetBuffer :: (TextViewClass self, TextBufferClass buffer) => self -> buffer -> IO ()
textViewSetBuffer self buffer =
  (\(TextView arg1) (TextBuffer arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_text_view_set_buffer argPtr1 argPtr2)
{-# LINE 246 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)
    (toTextBuffer buffer)

-- | Returns the 'TextBuffer' being displayed by this text view.
--
textViewGetBuffer :: TextViewClass self => self -> IO TextBuffer
textViewGetBuffer self =
  makeNewGObject mkTextBuffer $
  (\(TextView arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_text_view_get_buffer argPtr1)
{-# LINE 255 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)

-- | Scrolls the text view so that @mark@ is on the screen in the position
-- indicated by @xalign@ and @yalign@. An alignment of 0.0 indicates left or
-- top, 1.0 indicates right or bottom, 0.5 means center. If the alignment is
-- @Nothing@, the text scrolls the minimal distance to get the mark onscreen,
-- possibly not scrolling at all. The effective screen for purposes of this
-- function is reduced by a margin of size @withinMargin@.
--
textViewScrollToMark :: (TextViewClass self, TextMarkClass mark) => self
 -> mark -- ^ @mark@ - a 'TextMark'
 -> Double -- ^ @withinMargin@ - margin as a [0.0,0.5) fraction of screen size
           -- and imposes an extra margin at all four sides of the window
           -- within which @xalign@ and @yalign@ are evaluated.
 -> Maybe (Double, Double) -- ^ @Just (xalign, yalign)@ - horizontal and
           -- vertical alignment of mark within visible area (if @Nothing@,
           -- scroll just enough to get the mark onscreen)
 -> IO ()
textViewScrollToMark self mark withinMargin align =
  let (useAlign, xalign, yalign) = case align of
        Nothing -> (False, 0, 0)
        Just (xalign, yalign) -> (True, xalign, yalign)
  in
  (\(TextView arg1) (TextMark arg2) arg3 arg4 arg5 arg6 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_text_view_scroll_to_mark argPtr1 argPtr2 arg3 arg4 arg5 arg6)
{-# LINE 279 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)
    (toTextMark mark)
    (realToFrac withinMargin)
    (fromBool useAlign)
    (realToFrac xalign)
    (realToFrac yalign)

-- | Scrolls the text view so that @iter@ is on the screen in the position
-- indicated by @xalign@ and @yalign@. An alignment of 0.0 indicates left or
-- top, 1.0 indicates right or bottom, 0.5 means center. If the alignment is
-- @Nothing@, the text scrolls the minimal distance to get the mark onscreen,
-- possibly not scrolling at all. The effective screen for purposes of this
-- function is reduced by a margin of size @withinMargin@.
--
-- * This function
-- uses the currently-computed height of the lines in the text buffer. Note
-- that line heights are computed in an idle handler; so this function may
-- not
-- have the desired effect if it's called before the height computations. To
-- avoid oddness, consider using 'textViewScrollToMark' which saves a point
-- to be scrolled to after line validation. This is particularly important
-- if you add new text to the buffer and immediately ask the view to scroll
-- to it (which it can't since it is not updated until the main loop runs).
--
textViewScrollToIter :: TextViewClass self => self
 -> TextIter -- ^ @iter@ - a 'TextIter'
 -> Double -- ^ @withinMargin@ - margin as a [0.0,0.5) fraction of screen
             -- size
 -> Maybe (Double, Double) -- ^ @Just (xalign, yalign)@ - horizontal and
             -- vertical alignment of mark within visible area (if @Nothing@,
             -- scroll just enough to get the iterator onscreen)
 -> IO Bool -- ^ returns @True@ if scrolling occurred
textViewScrollToIter self iter withinMargin align =
  let (useAlign, xalign, yalign) = case align of
        Nothing -> (False, 0, 0)
        Just (xalign, yalign) -> (True, xalign, yalign)
  in
  liftM toBool $
  (\(TextView arg1) (TextIter arg2) arg3 arg4 arg5 arg6 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_text_view_scroll_to_iter argPtr1 argPtr2 arg3 arg4 arg5 arg6)
{-# LINE 318 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)
    iter
    (realToFrac withinMargin)
    (fromBool useAlign)
    (realToFrac xalign)
    (realToFrac yalign)

-- | Scrolls the text view the minimum distance such that @mark@ is contained
-- within the visible area of the widget.
--
textViewScrollMarkOnscreen :: (TextViewClass self, TextMarkClass mark) => self
 -> mark -- ^ @mark@ - a mark in the buffer for the text view
 -> IO ()
textViewScrollMarkOnscreen self mark =
  (\(TextView arg1) (TextMark arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_text_view_scroll_mark_onscreen argPtr1 argPtr2)
{-# LINE 333 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)
    (toTextMark mark)

-- | Moves a mark within the buffer so that it's located within the
-- currently-visible text area.
--
textViewMoveMarkOnscreen :: (TextViewClass self, TextMarkClass mark) => self
 -> mark -- ^ @mark@ - a 'TextMark'
 -> IO Bool -- ^ returns @True@ if the mark moved (wasn't already onscreen)
textViewMoveMarkOnscreen self mark =
  liftM toBool $
  (\(TextView arg1) (TextMark arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_text_view_move_mark_onscreen argPtr1 argPtr2)
{-# LINE 345 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)
    (toTextMark mark)

-- | Moves the cursor to the currently visible region of the buffer, it it
-- isn't there already.
--
textViewPlaceCursorOnscreen :: TextViewClass self => self
 -> IO Bool -- ^ returns @True@ if the cursor had to be moved.
textViewPlaceCursorOnscreen self =
  liftM toBool $
  (\(TextView arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_text_view_place_cursor_onscreen argPtr1)
{-# LINE 356 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)

-- | Returns the currently-visible region of the buffer, in
-- buffer coordinates. Convert to window coordinates with
-- 'textViewBufferToWindowCoords'.
--
textViewGetVisibleRect :: TextViewClass self => self -> IO Rectangle
textViewGetVisibleRect self =
  alloca $ \rectPtr -> do
  (\(TextView arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_text_view_get_visible_rect argPtr1 arg2)
{-# LINE 366 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)
    (castPtr rectPtr)
  peek rectPtr

-- | Gets a rectangle which roughly contains the character at @iter@. The
-- rectangle position is in buffer coordinates; use
-- 'textViewBufferToWindowCoords' to convert these coordinates to coordinates
-- for one of the windows in the text view.
--
textViewGetIterLocation :: TextViewClass self => self -> TextIter -> IO Rectangle
textViewGetIterLocation self iter =
  alloca $ \rectPtr -> do
  (\(TextView arg1) (TextIter arg2) arg3 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_text_view_get_iter_location argPtr1 argPtr2 arg3)
{-# LINE 379 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)
    iter
    (castPtr rectPtr)
  peek rectPtr

-- | Gets the 'TextIter' at the start of the line containing the coordinate
-- @y@. @y@ is in buffer coordinates, convert from window coordinates with
-- 'textViewWindowToBufferCoords'. Also returns @lineTop@ the
-- coordinate of the top edge of the line.
--
textViewGetLineAtY :: TextViewClass self => self
 -> Int -- ^ @y@ - a y coordinate
 -> IO (TextIter, Int) -- ^ @(targetIter, lineTop)@ - returns the iter and the
                       -- top coordinate of the line
textViewGetLineAtY self y =
  makeEmptyTextIter >>= \targetIter ->
  alloca $ \lineTopPtr -> do
  (\(TextView arg1) (TextIter arg2) arg3 arg4 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_text_view_get_line_at_y argPtr1 argPtr2 arg3 arg4)
{-# LINE 397 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)
    targetIter
    (fromIntegral y)
    lineTopPtr
  lineTop <- peek lineTopPtr
  return (targetIter, fromIntegral lineTop)

-- | Gets the y coordinate of the top of the line containing @iter@, and the
-- height of the line. The coordinate is a buffer coordinate; convert to window
-- coordinates with 'textViewBufferToWindowCoords'.
--
textViewGetLineYrange :: TextViewClass self => self
 -> TextIter -- ^ @iter@ - a 'TextIter'
 -> IO (Int, Int) -- ^ @(y, height)@ - y coordinate and height of the line
textViewGetLineYrange self iter =
  alloca $ \yPtr ->
  alloca $ \heightPtr -> do
  (\(TextView arg1) (TextIter arg2) arg3 arg4 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_text_view_get_line_yrange argPtr1 argPtr2 arg3 arg4)
{-# LINE 415 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)
    iter
    yPtr
    heightPtr
  y <- peek yPtr
  height <- peek heightPtr
  return (fromIntegral y, fromIntegral height)

-- | Retrieves the iterator at buffer coordinates @x@ and @y@. Buffer
-- coordinates are coordinates for the entire buffer, not just the
-- currently-displayed portion. If you have coordinates from an event, you have
-- to convert those to buffer coordinates with 'textViewWindowToBufferCoords'.
--
textViewGetIterAtLocation :: TextViewClass self => self
 -> Int -- ^ @x@ - x position, in buffer coordinates
 -> Int -- ^ @y@ - y position, in buffer coordinates
 -> IO TextIter
textViewGetIterAtLocation self x y = do
  iter <- makeEmptyTextIter
  (\(TextView arg1) (TextIter arg2) arg3 arg4 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_text_view_get_iter_at_location argPtr1 argPtr2 arg3 arg4)
{-# LINE 435 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)
    iter
    (fromIntegral x)
    (fromIntegral y)
  return iter

-- | Converts coordinate @(bufferX, bufferY)@ to coordinates for the window
-- @win@
--
-- Note that you can't convert coordinates for a nonexisting window (see
-- 'textViewSetBorderWindowSize').
--
textViewBufferToWindowCoords :: TextViewClass self => self
 -> TextWindowType -- ^ @win@ - a 'TextWindowType' except 'TextWindowPrivate'
 -> (Int, Int) -- ^ @(bufferX, bufferY)@ - buffer x and y coordinates
 -> IO (Int, Int) -- ^ returns window x and y coordinates
textViewBufferToWindowCoords self win (bufferX, bufferY) =
  alloca $ \windowXPtr ->
  alloca $ \windowYPtr -> do
  (\(TextView arg1) arg2 arg3 arg4 arg5 arg6 -> withForeignPtr arg1 $ \argPtr1 ->gtk_text_view_buffer_to_window_coords argPtr1 arg2 arg3 arg4 arg5 arg6)
{-# LINE 455 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)
    ((fromIntegral . fromEnum) win)
    (fromIntegral bufferX)
    (fromIntegral bufferY)
    windowXPtr
    windowYPtr
  windowX <- peek windowXPtr
  windowY <- peek windowYPtr
  return (fromIntegral windowX, fromIntegral windowY)

-- | Converts coordinates on the window identified by @win@ to buffer
-- coordinates.
--
-- Note that you can't convert coordinates for a nonexisting window (see
-- 'textViewSetBorderWindowSize').
--
textViewWindowToBufferCoords :: TextViewClass self => self
 -> TextWindowType -- ^ @win@ - a 'TextWindowType' except 'TextWindowPrivate'
 -> (Int, Int) -- ^ @(windowX, windowY)@ - window x and y coordinates
 -> IO (Int, Int) -- ^ returns buffer x and y coordinates
textViewWindowToBufferCoords self win (windowX, windowY) =
  alloca $ \bufferXPtr ->
  alloca $ \bufferYPtr -> do
  (\(TextView arg1) arg2 arg3 arg4 arg5 arg6 -> withForeignPtr arg1 $ \argPtr1 ->gtk_text_view_window_to_buffer_coords argPtr1 arg2 arg3 arg4 arg5 arg6)
{-# LINE 479 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)
    ((fromIntegral . fromEnum) win)
    (fromIntegral windowX)
    (fromIntegral windowY)
    bufferXPtr
    bufferYPtr
  bufferX <- peek bufferXPtr
  bufferY <- peek bufferYPtr
  return (fromIntegral bufferX, fromIntegral bufferY)

-- | Retrieves the 'DrawWindow' corresponding to an area of the text view;
-- possible windows include the overall widget window, child windows on the
-- left, right, top, bottom, and the window that displays the text buffer.
-- Windows are @Nothing@ and nonexistent if their width or height is 0, and are
-- nonexistent before the widget has been realized.
--
textViewGetWindow :: TextViewClass self => self
 -> TextWindowType -- ^ @win@ - window to get
 -> IO (Maybe DrawWindow) -- ^ returns a 'DrawWindow', or @Nothing@
textViewGetWindow self win =
  maybeNull (makeNewGObject mkDrawWindow) $
  (\(TextView arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_text_view_get_window argPtr1 arg2)
{-# LINE 501 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)
    ((fromIntegral . fromEnum) win)

-- | Retrieve the type of window the 'TextView' widget contains.
--
-- Usually used to find out which window an event corresponds to. An emission
-- of an event signal of 'TextView' yields a 'DrawWindow'. This function can be
-- used to see if the event actually belongs to the main text window.
--
textViewGetWindowType :: TextViewClass self => self
 -> DrawWindow
 -> IO TextWindowType
textViewGetWindowType self window =
  liftM (toEnum . fromIntegral) $
  (\(TextView arg1) (DrawWindow arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_text_view_get_window_type argPtr1 argPtr2)
{-# LINE 516 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)
    window

-- | Sets the width of 'TextWindowLeft' or 'TextWindowRight', or the height of
-- 'TextWindowTop' or 'TextWindowBottom'. Automatically destroys the
-- corresponding window if the size is set to 0, and creates the window if the
-- size is set to non-zero. This function can only be used for the \"border
-- windows\", it doesn't work with 'TextWindowWidget', 'TextWindowText', or
-- 'TextWindowPrivate'.
--
textViewSetBorderWindowSize :: TextViewClass self => self
 -> TextWindowType -- ^ @type@ - window to affect
 -> Int -- ^ @size@ - width or height of the window
 -> IO ()
textViewSetBorderWindowSize self type_ size =
  (\(TextView arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_text_view_set_border_window_size argPtr1 arg2 arg3)
{-# LINE 532 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)
    ((fromIntegral . fromEnum) type_)
    (fromIntegral size)

-- | Gets the width of the specified border window. See
-- 'textViewSetBorderWindowSize'.
--
textViewGetBorderWindowSize :: TextViewClass self => self
 -> TextWindowType -- ^ @type@ - window to return size from
 -> IO Int -- ^ returns width of window
textViewGetBorderWindowSize self type_ =
  liftM fromIntegral $
  (\(TextView arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_text_view_get_border_window_size argPtr1 arg2)
{-# LINE 545 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)
    ((fromIntegral . fromEnum) type_)

-- | Moves the given @iter@ forward by one display (wrapped) line. A display
-- line is different from a paragraph. Paragraphs are separated by newlines or
-- other paragraph separator characters. Display lines are created by
-- line-wrapping a paragraph. If wrapping is turned off, display lines and
-- paragraphs will be the same. Display lines are divided differently for each
-- view, since they depend on the view's width; paragraphs are the same in all
-- views, since they depend on the contents of the 'TextBuffer'.
--
textViewForwardDisplayLine :: TextViewClass self => self
 -> TextIter -- ^ @iter@ - a 'TextIter'
 -> IO Bool -- ^ returns @True@ if @iter@ was moved and is not on the end
             -- iterator
textViewForwardDisplayLine self iter =
  liftM toBool $
  (\(TextView arg1) (TextIter arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_text_view_forward_display_line argPtr1 argPtr2)
{-# LINE 563 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)
    iter

-- | Moves the given @iter@ backward by one display (wrapped) line. A display
-- line is different from a paragraph. Paragraphs are separated by newlines or
-- other paragraph separator characters. Display lines are created by
-- line-wrapping a paragraph. If wrapping is turned off, display lines and
-- paragraphs will be the same. Display lines are divided differently for each
-- view, since they depend on the view's width; paragraphs are the same in all
-- views, since they depend on the contents of the 'TextBuffer'.
--
textViewBackwardDisplayLine :: TextViewClass self => self
 -> TextIter -- ^ @iter@ - a 'TextIter'
 -> IO Bool -- ^ returns @True@ if @iter@ was moved and is not on the end
             -- iterator
textViewBackwardDisplayLine self iter =
  liftM toBool $
  (\(TextView arg1) (TextIter arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_text_view_backward_display_line argPtr1 argPtr2)
{-# LINE 581 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)
    iter

-- | Moves the given @iter@ forward to the next display line end. A display
-- line is different from a paragraph. Paragraphs are separated by newlines or
-- other paragraph separator characters. Display lines are created by
-- line-wrapping a paragraph. If wrapping is turned off, display lines and
-- paragraphs will be the same. Display lines are divided differently for each
-- view, since they depend on the view's width; paragraphs are the same in all
-- views, since they depend on the contents of the 'TextBuffer'.
--
textViewForwardDisplayLineEnd :: TextViewClass self => self
 -> TextIter -- ^ @iter@ - a 'TextIter'
 -> IO Bool -- ^ returns @True@ if @iter@ was moved and is not on the end
             -- iterator
textViewForwardDisplayLineEnd self iter =
  liftM toBool $
  (\(TextView arg1) (TextIter arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_text_view_forward_display_line_end argPtr1 argPtr2)
{-# LINE 599 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)
    iter

-- | Moves the given @iter@ backward to the next display line start. A display
-- line is different from a paragraph. Paragraphs are separated by newlines or
-- other paragraph separator characters. Display lines are created by
-- line-wrapping a paragraph. If wrapping is turned off, display lines and
-- paragraphs will be the same. Display lines are divided differently for each
-- view, since they depend on the view's width; paragraphs are the same in all
-- views, since they depend on the contents of the 'TextBuffer'.
--
textViewBackwardDisplayLineStart :: TextViewClass self => self
 -> TextIter -- ^ @iter@ - a 'TextIter'
 -> IO Bool -- ^ returns @True@ if @iter@ was moved and is not on the end
             -- iterator
textViewBackwardDisplayLineStart self iter =
  liftM toBool $
  (\(TextView arg1) (TextIter arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_text_view_backward_display_line_start argPtr1 argPtr2)
{-# LINE 617 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)
    iter

-- | Determines whether @iter@ is at the start of a display line. See
-- 'textViewForwardDisplayLine' for an explanation of display lines vs.
-- paragraphs.
--
textViewStartsDisplayLine :: TextViewClass self => self
 -> TextIter -- ^ @iter@ - a 'TextIter'
 -> IO Bool -- ^ returns @True@ if @iter@ begins a wrapped line
textViewStartsDisplayLine self iter =
  liftM toBool $
  (\(TextView arg1) (TextIter arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_text_view_starts_display_line argPtr1 argPtr2)
{-# LINE 630 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)
    iter

-- | Move the iterator a given number of characters visually, treating it as
-- the strong cursor position. If @count@ is positive, then the new strong
-- cursor position will be @count@ positions to the right of the old cursor
-- position. If @count@ is negative then the new strong cursor position will be
-- @count@ positions to the left of the old cursor position.
--
-- In the presence of bidirection text, the correspondence between logical
-- and visual order will depend on the direction of the current run, and there
-- may be jumps when the cursor is moved off of the end of a run.
--
textViewMoveVisually :: TextViewClass self => self
 -> TextIter -- ^ @iter@ - a 'TextIter'
 -> Int -- ^ @count@ - number of characters to move (negative moves left,
             -- positive moves right)
 -> IO Bool -- ^ returns @True@ if @iter@ moved and is not on the end
             -- iterator
textViewMoveVisually self iter count =
  liftM toBool $
  (\(TextView arg1) (TextIter arg2) arg3 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_text_view_move_visually argPtr1 argPtr2 arg3)
{-# LINE 652 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)
    iter
    (fromIntegral count)

-- | Adds a child widget in the text buffer, at the given @anchor@.
--
textViewAddChildAtAnchor :: (TextViewClass self, WidgetClass child) => self
 -> child -- ^ @child@ - a 'Widget'
 -> TextChildAnchor -- ^ @anchor@ - a 'TextChildAnchor' in the 'TextBuffer'
                    -- for the text view
 -> IO ()
textViewAddChildAtAnchor self child anchor =
  (\(TextView arg1) (Widget arg2) (TextChildAnchor arg3) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->withForeignPtr arg3 $ \argPtr3 ->gtk_text_view_add_child_at_anchor argPtr1 argPtr2 argPtr3)
{-# LINE 665 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)
    (toWidget child)
    anchor

-- | Create a new 'TextChildAnchor'.
--
-- * Using 'textBufferCreateChildAnchor' is usually simpler then
-- executing this function and 'textBufferInsertChildAnchor'.
--
textChildAnchorNew :: IO TextChildAnchor
textChildAnchorNew =
  wrapNewGObject mkTextChildAnchor
  gtk_text_child_anchor_new
{-# LINE 678 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}

-- | Retrieve all 'Widget's at this
-- 'TextChildAnchor'.
--
-- * The widgets in the returned list need to be upcasted to what they were.
--
textChildAnchorGetWidgets :: TextChildAnchor -> IO [Widget]
textChildAnchorGetWidgets tca = do
  gList <- (\(TextChildAnchor arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_text_child_anchor_get_widgets argPtr1) tca
  wList <- fromGList gList
  mapM (makeNewObject mkWidget) (map return wList)

-- | Query if an anchor was deleted.
--
textChildAnchorGetDeleted :: TextChildAnchor -> IO Bool
textChildAnchorGetDeleted tca =
  liftM toBool $
  (\(TextChildAnchor arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_text_child_anchor_get_deleted argPtr1) tca

-- | Adds a child at fixed coordinates in one of the text widget's windows.
-- The window must have nonzero size (see 'textViewSetBorderWindowSize'). Note
-- that the child coordinates are given relative to the 'DrawWindow' in
-- question, and that these coordinates have no sane relationship to scrolling.
-- When placing a child in 'TextWindowWidget', scrolling is irrelevant, the
-- child floats above all scrollable areas. If you want the widget to move when
-- the text view scrolls, use 'textViewAddChildAtAnchor' instead.
--
textViewAddChildInWindow :: (TextViewClass self, WidgetClass child) => self
 -> child -- ^ @child@ - a 'Widget'
 -> TextWindowType -- ^ @whichWindow@ - which window the child should appear
                   -- in
 -> Int -- ^ @xpos@ - X position of child in window coordinates
 -> Int -- ^ @ypos@ - Y position of child in window coordinates
 -> IO ()
textViewAddChildInWindow self child whichWindow xpos ypos =
  (\(TextView arg1) (Widget arg2) arg3 arg4 arg5 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_text_view_add_child_in_window argPtr1 argPtr2 arg3 arg4 arg5)
{-# LINE 714 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)
    (toWidget child)
    ((fromIntegral . fromEnum) whichWindow)
    (fromIntegral xpos)
    (fromIntegral ypos)

-- | Move a child widget within the 'TextView'. This is really only apprpriate
-- for \"floating\" child widgets added using 'textViewAddChildInWindow'.
--
textViewMoveChild :: (TextViewClass self, WidgetClass child) => self
 -> child -- ^ @child@ - child widget already added to the text view
 -> Int -- ^ @xpos@ - new X position in window coordinates
 -> Int -- ^ @ypos@ - new Y position in window coordinates
 -> IO ()
textViewMoveChild self child xpos ypos =
  (\(TextView arg1) (Widget arg2) arg3 arg4 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_text_view_move_child argPtr1 argPtr2 arg3 arg4)
{-# LINE 730 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)
    (toWidget child)
    (fromIntegral xpos)
    (fromIntegral ypos)

-- | Sets the line wrapping for the view.
--
textViewSetWrapMode :: TextViewClass self => self -> WrapMode -> IO ()
textViewSetWrapMode self wrapMode =
  (\(TextView arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_text_view_set_wrap_mode argPtr1 arg2)
{-# LINE 740 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)
    ((fromIntegral . fromEnum) wrapMode)

-- | Gets the line wrapping for the view.
--
textViewGetWrapMode :: TextViewClass self => self -> IO WrapMode
textViewGetWrapMode self =
  liftM (toEnum . fromIntegral) $
  (\(TextView arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_text_view_get_wrap_mode argPtr1)
{-# LINE 749 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)

-- | Sets the default editability of the 'TextView'. You can override this
-- default setting with tags in the buffer, using the \"editable\" attribute of
-- tags.
--
textViewSetEditable :: TextViewClass self => self -> Bool -> IO ()
textViewSetEditable self setting =
  (\(TextView arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_text_view_set_editable argPtr1 arg2)
{-# LINE 758 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)
    (fromBool setting)

-- | Returns the default editability of the 'TextView'. Tags in the buffer may
-- override this setting for some ranges of text.
--
textViewGetEditable :: TextViewClass self => self -> IO Bool
textViewGetEditable self =
  liftM toBool $
  (\(TextView arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_text_view_get_editable argPtr1)
{-# LINE 768 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)

-- | Toggles whether the insertion point is displayed. A buffer with no
-- editable text probably shouldn't have a visible cursor, so you may want to
-- turn the cursor off.
--
textViewSetCursorVisible :: TextViewClass self => self -> Bool -> IO ()
textViewSetCursorVisible self setting =
  (\(TextView arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_text_view_set_cursor_visible argPtr1 arg2)
{-# LINE 777 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)
    (fromBool setting)

-- | Find out whether the cursor is being displayed.
--
textViewGetCursorVisible :: TextViewClass self => self -> IO Bool
textViewGetCursorVisible self =
  liftM toBool $
  (\(TextView arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_text_view_get_cursor_visible argPtr1)
{-# LINE 786 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)

-- | Sets the default number of blank pixels above paragraphs in the text view.
-- Tags in the buffer for the text view may override the defaults.
--
-- * Tags in the buffer may override this default.
--
textViewSetPixelsAboveLines :: TextViewClass self => self -> Int -> IO ()
textViewSetPixelsAboveLines self pixelsAboveLines =
  (\(TextView arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_text_view_set_pixels_above_lines argPtr1 arg2)
{-# LINE 796 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)
    (fromIntegral pixelsAboveLines)

-- | Gets the default number of pixels to put above paragraphs.
--
textViewGetPixelsAboveLines :: TextViewClass self => self -> IO Int
textViewGetPixelsAboveLines self =
  liftM fromIntegral $
  (\(TextView arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_text_view_get_pixels_above_lines argPtr1)
{-# LINE 805 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)

-- | Sets the default number of pixels of blank space to put below paragraphs
-- in the text view. May be overridden by tags applied to the text view's
-- buffer.
--
textViewSetPixelsBelowLines :: TextViewClass self => self -> Int -> IO ()
textViewSetPixelsBelowLines self pixelsBelowLines =
  (\(TextView arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_text_view_set_pixels_below_lines argPtr1 arg2)
{-# LINE 814 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)
    (fromIntegral pixelsBelowLines)

-- | Gets the default number of blank pixels below each paragraph.
--
textViewGetPixelsBelowLines :: TextViewClass self => self -> IO Int
textViewGetPixelsBelowLines self =
  liftM fromIntegral $
  (\(TextView arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_text_view_get_pixels_below_lines argPtr1)
{-# LINE 823 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)

-- | Sets the default number of pixels of blank space to leave between
-- display\/wrapped lines within a paragraph. May be overridden by tags in
-- the text view's buffer.
--
textViewSetPixelsInsideWrap :: TextViewClass self => self -> Int -> IO ()
textViewSetPixelsInsideWrap self pixelsInsideWrap =
  (\(TextView arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_text_view_set_pixels_inside_wrap argPtr1 arg2)
{-# LINE 832 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)
    (fromIntegral pixelsInsideWrap)

-- | Gets the default number of pixels of blank space between lines in a
-- wrapped paragraph.
--
textViewGetPixelsInsideWrap :: TextViewClass self => self -> IO Int
textViewGetPixelsInsideWrap self =
  liftM fromIntegral $
  (\(TextView arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_text_view_get_pixels_inside_wrap argPtr1)
{-# LINE 842 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)

-- | Sets the default justification of text in the text view. Tags in the
-- view's buffer may override the default.
--
textViewSetJustification :: TextViewClass self => self -> Justification -> IO ()
textViewSetJustification self justification =
  (\(TextView arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_text_view_set_justification argPtr1 arg2)
{-# LINE 850 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)
    ((fromIntegral . fromEnum) justification)

-- | Gets the default justification of paragraphs in the text view. Tags in the
-- buffer may override the default.
--
textViewGetJustification :: TextViewClass self => self -> IO Justification
textViewGetJustification self =
  liftM (toEnum . fromIntegral) $
  (\(TextView arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_text_view_get_justification argPtr1)
{-# LINE 860 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)

-- | Sets the default left margin for text in the text view. Tags in the buffer
-- may override the default.
--
textViewSetLeftMargin :: TextViewClass self => self
 -> Int -- ^ @leftMargin@ - left margin in pixels
 -> IO ()
textViewSetLeftMargin self leftMargin =
  (\(TextView arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_text_view_set_left_margin argPtr1 arg2)
{-# LINE 870 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)
    (fromIntegral leftMargin)

-- | Gets the default left margin size of paragraphs in the text view. Tags
-- in the buffer may override the default.
--
textViewGetLeftMargin :: TextViewClass self => self
 -> IO Int -- ^ returns left margin in pixels
textViewGetLeftMargin self =
  liftM fromIntegral $
  (\(TextView arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_text_view_get_left_margin argPtr1)
{-# LINE 881 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)

-- | Sets the default right margin for text in the text view. Tags in the
-- buffer may override the default.
--
textViewSetRightMargin :: TextViewClass self => self
 -> Int -- ^ @rightMargin@ - right margin in pixels
 -> IO ()
textViewSetRightMargin self rightMargin =
  (\(TextView arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_text_view_set_right_margin argPtr1 arg2)
{-# LINE 891 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)
    (fromIntegral rightMargin)

-- | Gets the default right margin for text in the text view. Tags in the
-- buffer may override the default.
--
textViewGetRightMargin :: TextViewClass self => self
 -> IO Int -- ^ returns right margin in pixels
textViewGetRightMargin self =
  liftM fromIntegral $
  (\(TextView arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_text_view_get_right_margin argPtr1)
{-# LINE 902 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)

-- | Sets the default indentation for paragraphs in the text view. Tags in the
-- buffer may override the default.
--
textViewSetIndent :: TextViewClass self => self
 -> Int -- ^ @indent@ - indentation in pixels (may be negative)
 -> IO ()
textViewSetIndent self indent =
  (\(TextView arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_text_view_set_indent argPtr1 arg2)
{-# LINE 912 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)
    (fromIntegral indent)

-- | Gets the default indentation of paragraphs in the text view. Tags in the
-- view's buffer may override the default. The indentation may be negative.
--
textViewGetIndent :: TextViewClass self => self
 -> IO Int -- ^ returns number of pixels of indentation
textViewGetIndent self =
  liftM fromIntegral $
  (\(TextView arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_text_view_get_indent argPtr1)
{-# LINE 923 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)

-- | Obtains a copy of the default text attributes. These are the attributes
-- used for text unless a tag overrides them. You'd typically pass the default
-- attributes in to 'textIterGetAttributes' in order to get the attributes in
-- effect at a given text position.
--
textViewGetDefaultAttributes :: TextViewClass self => self -> IO TextAttributes
textViewGetDefaultAttributes self =
  (\(TextView arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_text_view_get_default_attributes argPtr1)
{-# LINE 933 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)
  >>= makeNewTextAttributes


-- | Retrieves the iterator pointing to the character at buffer coordinates
-- @x@ and @y@. Buffer coordinates are coordinates for the entire buffer, not
-- just the currently-displayed portion. If you have coordinates from an event,
-- you have to convert those to buffer coordinates with
-- 'textViewWindowToBufferCoords'.
--
-- Note that this is different from 'textViewGetIterAtLocation', which
-- returns cursor locations, i.e. positions /between/ characters.
--
-- * Available since Gtk+ version 2.6
--
textViewGetIterAtPosition :: TextViewClass self => self
 -> Int -- ^ @x@ - x position, in buffer coordinates
 -> Int -- ^ @y@ - y position, in buffer coordinates
 -> IO (TextIter, Int) -- ^ @(iter, trailing)@ - returns the iterator and
                         -- an integer indicating where in the grapheme the
                         -- user clicked. It will either be zero, or the
                         -- number of characters in the grapheme. 0 represents
                         -- the trailing edge of the grapheme.
textViewGetIterAtPosition self x y =
  alloca $ \trailingPtr -> do
  iter <- makeEmptyTextIter
  (\(TextView arg1) (TextIter arg2) arg3 arg4 arg5 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_text_view_get_iter_at_position argPtr1 argPtr2 arg3 arg4 arg5)
{-# LINE 960 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)
    iter
    trailingPtr
    (fromIntegral x)
    (fromIntegral y)
  trailing <- peek trailingPtr
  return (iter, fromIntegral trailing)



-- | Changes the 'TextView' overwrite mode.
--
-- * Available since Gtk+ version 2.4
--
textViewSetOverwrite :: TextViewClass self => self
 -> Bool -- ^ @overwrite@ - @True@ to turn on overwrite mode, @False@ to turn
          -- it off
 -> IO ()
textViewSetOverwrite self overwrite =
  (\(TextView arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_text_view_set_overwrite argPtr1 arg2)
{-# LINE 980 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)
    (fromBool overwrite)

-- | Returns whether the 'TextView' is in overwrite mode or not.
--
-- * Available since Gtk+ version 2.4
--
textViewGetOverwrite :: TextViewClass self => self -> IO Bool
textViewGetOverwrite self =
  liftM toBool $
  (\(TextView arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_text_view_get_overwrite argPtr1)
{-# LINE 991 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)

-- | Sets the behavior of the text widget when the Tab key is pressed. If
-- @acceptsTab@ is @True@ a tab character is inserted. If @acceptsTab@ is
-- @False@ the keyboard focus is moved to the next widget in the focus chain.
--
-- * Available since Gtk+ version 2.4
--
textViewSetAcceptsTab :: TextViewClass self => self
 -> Bool -- ^ @acceptsTab@ - @True@ if pressing the Tab key should insert a
          -- tab character, @False@, if pressing the Tab key should move the
          -- keyboard focus.
 -> IO ()
textViewSetAcceptsTab self acceptsTab =
  (\(TextView arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_text_view_set_accepts_tab argPtr1 arg2)
{-# LINE 1006 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)
    (fromBool acceptsTab)

-- | Returns whether pressing the Tab key inserts a tab characters.
-- 'textViewSetAcceptsTab'.
--
-- * Available since Gtk+ version 2.4
--
textViewGetAcceptsTab :: TextViewClass self => self
 -> IO Bool -- ^ returns @True@ if pressing the Tab key inserts a tab
            -- character, @False@ if pressing the Tab key moves the keyboard
            -- focus.
textViewGetAcceptsTab self =
  liftM toBool $
  (\(TextView arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_text_view_get_accepts_tab argPtr1)
{-# LINE 1021 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)



-- | Gets the horizontal-scrolling 'Adjustment'.
--
-- * Available since Gtk+ version 2.22
--
textViewGetHadjustment :: TextViewClass self => self -> IO Adjustment
textViewGetHadjustment self =
  makeNewObject mkAdjustment $
  (\(TextView arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_text_view_get_hadjustment argPtr1)
{-# LINE 1033 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)

-- | Gets the vertical-scrolling 'Adjustment'.
--
-- * Available since Gtk+ version 2.22
--
textViewGetVadjustment :: TextViewClass self => self -> IO Adjustment
textViewGetVadjustment self =
  makeNewObject mkAdjustment $
  (\(TextView arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_text_view_get_vadjustment argPtr1)
{-# LINE 1043 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
    (toTextView self)

-- | Allow the 'TextView' input method to internally handle key press and release events. If this
-- function returns 'True', then no further processing should be done for this key event. See
-- 'imContextFilterKeypress'.
--
-- Note that you are expected to call this function from your handler when overriding key event
-- handling. This is needed in the case when you need to insert your own key handling between the input
-- method and the default key event handling of the 'TextView'.
--
-- * Available since Gtk+ version 2.22
--
textViewImContextFilterKeypress :: TextViewClass self => self -> EventM EKey Bool
textViewImContextFilterKeypress self = do
  ptr <- ask
  liftIO $ liftM toBool $
    (\(TextView arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_text_view_im_context_filter_keypress argPtr1 arg2)
{-# LINE 1060 "./Graphics/UI/Gtk/Multiline/TextView.chs" #-}
      (toTextView self)
      (castPtr ptr)

-- | Reset the input method context of the text view if needed.
--
-- This can be necessary in the case where modifying the buffer would confuse on-going input method
-- behavior.
--
-- * Available since Gtk+ version 2.22
--
textViewResetImContext :: TextViewClass self => self -> IO ()
textViewResetImContext self =
  (\(TextView arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_text_view_reset_im_context argPtr1) (toTextView self)




--------------------
-- Attributes

-- | Pixels of blank space above paragraphs.
--
-- Allowed values: >= 0
--
-- Default value: 0
--
textViewPixelsAboveLines :: TextViewClass self => Attr self Int
textViewPixelsAboveLines = newAttr
  textViewGetPixelsAboveLines
  textViewSetPixelsAboveLines

-- | Pixels of blank space below paragraphs.
--
-- Allowed values: >= 0
--
-- Default value: 0
--
textViewPixelsBelowLines :: TextViewClass self => Attr self Int
textViewPixelsBelowLines = newAttr
  textViewGetPixelsBelowLines
  textViewSetPixelsBelowLines

-- | Pixels of blank space between wrapped lines in a paragraph.
--
-- Allowed values: >= 0
--
-- Default value: 0
--
textViewPixelsInsideWrap :: TextViewClass self => Attr self Int
textViewPixelsInsideWrap = newAttr
  textViewGetPixelsInsideWrap
  textViewSetPixelsInsideWrap

-- | Whether the text can be modified by the user.
--
-- Default value: @True@
--
textViewEditable :: TextViewClass self => Attr self Bool
textViewEditable = newAttr
  textViewGetEditable
  textViewSetEditable

-- | Which IM (input method) module should be used for this entry. See GtkIMContext.
-- Setting this to a non-empty value overrides the system-wide IM module setting.
-- See the GtkSettings "gtk-im-module" property.
--
-- Default value: \"\"
--
textViewImModule :: TextViewClass self => Attr self DefaultGlibString
textViewImModule =
  newAttrFromStringProperty "im-module"

-- | Whether to wrap lines never, at word boundaries, or at character
-- boundaries.
--
-- Default value: 'WrapNone'
--
textViewWrapMode :: TextViewClass self => Attr self WrapMode
textViewWrapMode = newAttr
  textViewGetWrapMode
  textViewSetWrapMode

-- | Left, right, or center justification.
--
-- Default value: 'JustifyLeft'
--
textViewJustification :: TextViewClass self => Attr self Justification
textViewJustification = newAttr
  textViewGetJustification
  textViewSetJustification

-- | Width of the left margin in pixels.
--
-- Allowed values: >= 0
--
-- Default value: 0
--
textViewLeftMargin :: TextViewClass self => Attr self Int
textViewLeftMargin = newAttr
  textViewGetLeftMargin
  textViewSetLeftMargin

-- | Width of the right margin in pixels.
--
-- Allowed values: >= 0
--
-- Default value: 0
--
textViewRightMargin :: TextViewClass self => Attr self Int
textViewRightMargin = newAttr
  textViewGetRightMargin
  textViewSetRightMargin

-- | Amount to indent the paragraph, in pixels.
--
-- Allowed values: >= 0
--
-- Default value: 0
--
textViewIndent :: TextViewClass self => Attr self Int
textViewIndent = newAttr
  textViewGetIndent
  textViewSetIndent

-- | If the insertion cursor is shown.
--
-- Default value: @True@
--
textViewCursorVisible :: TextViewClass self => Attr self Bool
textViewCursorVisible = newAttr
  textViewGetCursorVisible
  textViewSetCursorVisible

-- | The buffer which is displayed.
--
textViewBuffer :: TextViewClass self => Attr self TextBuffer
textViewBuffer = newAttr
  textViewGetBuffer
  textViewSetBuffer


-- | Whether entered text overwrites existing contents.
--
-- Default value: @False@
--
textViewOverwrite :: TextViewClass self => Attr self Bool
textViewOverwrite = newAttr
  textViewGetOverwrite
  textViewSetOverwrite

-- | Whether Tab will result in a tab character being entered.
--
-- Default value: @True@
--
textViewAcceptsTab :: TextViewClass self => Attr self Bool
textViewAcceptsTab = newAttr
  textViewGetAcceptsTab
  textViewSetAcceptsTab


--------------------
-- Signals
-- | The 'backspace' signal is a keybinding signal which gets emitted when the user asks for it.
--
-- The default bindings for this signal are Backspace and Shift-Backspace.
--
backspace :: TextViewClass self => Signal self (IO ())
backspace = Signal (connect_NONE__NONE "on-backspace")

-- | Copying to the clipboard.
--
-- * This signal is emitted when a selection is copied to the clipboard.
--
-- * The action itself happens when the 'TextView' processes this
-- signal.
--
copyClipboard :: TextViewClass self => Signal self (IO ())
copyClipboard = Signal (connect_NONE__NONE "copy-clipboard")

-- | Cutting to the clipboard.
--
-- * This signal is emitted when a selection is cut out and copied to the
-- clipboard. The action itself happens when the textview processed this
-- request.
--
cutClipboard :: TextViewClass self => Signal self (IO ())
cutClipboard = Signal (connect_NONE__NONE "cut-clipboard")

-- | Deleting text.
--
-- * The widget will remove the specified number of units in the text where
-- the meaning of units depends on the kind of deletion.
--
-- * The action itself happens when the 'TextView' processes this
-- signal.
--
deleteFromCursor :: TextViewClass self => Signal self (DeleteType -> Int -> IO ())
deleteFromCursor = Signal (connect_ENUM_INT__NONE "delete-from-cursor")

-- | Inserting text.
--
-- * The widget will insert the string into the text where the meaning
-- of units depends on the kind of deletion.
--
-- * The action itself happens when the 'TextView' processes this
-- signal.
--
insertAtCursor :: (TextViewClass self, GlibString string) => Signal self (string -> IO ())
insertAtCursor = Signal (connect_GLIBSTRING__NONE "insert-at-cursor")

-- | Moving the cursor.
--
-- * The signal specifies what kind and how many steps the cursor will do.
-- The flag is set to @True@ if this movement extends a selection.
--
-- * The action itself happens when the 'TextView' processes this
-- signal.
--
moveCursor :: TextViewClass self => Signal self (MovementStep -> Int -> Bool -> IO ())
moveCursor = Signal (connect_ENUM_INT_BOOL__NONE "move-cursor")

-- | The 'moveViewport' signal is a keybinding signal which can be bound to key combinations
-- to allow the user to move the viewport, i.e.
-- change what part of the text view is visible in a containing scrolled window.
-- There are no default bindings for this signal.
--
moveViewport :: TextViewClass self => Signal self (ScrollStep -> Int -> IO ())
moveViewport = Signal (connect_ENUM_INT__NONE "move-viewport")

-- | Moving the focus.
--
-- * The action itself happens when the 'TextView' processes this
-- signal.
--
moveFocus :: TextViewClass self => Signal self (DirectionType -> IO ())
moveFocus = Signal (connect_ENUM__NONE "move-focus")

-- | Page change signals.
--
-- * The signal specifies how many pages the view should move up or down.
-- The flag is set to @True@ if this movement extends a selection.
--
-- * The action itself happens when the 'TextView' processes this
-- signal.
--
-- * Figure out why this signal is called horizontally, not vertically.
--
pageHorizontally :: TextViewClass self => Signal self (Int -> Bool -> IO ())
pageHorizontally = Signal (connect_INT_BOOL__NONE "page-horizontally")

-- | Pasting from the clipboard.
--
-- * This signal is emitted when something is pasted from the clipboard.
--
-- * The action itself happens when the 'TextView' processes this
-- signal.
--
pasteClipboard :: TextViewClass self => Signal self (IO ())
pasteClipboard = Signal (connect_NONE__NONE "paste-clipboard")

-- | Add menu entries to context menus.
--
-- * This signal is emitted if a context menu within the 'TextView'
-- is opened. This signal can be used to add application specific menu
-- items to this popup.
--
populatePopup :: TextViewClass self => Signal self (Menu -> IO ())
populatePopup = Signal (connect_OBJECT__NONE "populate-popup")

-- | Inserting an anchor.
--
-- * This signal is emitted when anchor is inserted into the text.
--
-- * The action itself happens when the 'TextView' processes this
-- signal.
--
selectAll :: TextViewClass self => Signal self (Bool -> IO ())
selectAll = Signal (connect_BOOL__NONE "select-all")

-- | The scroll-bars changed.
--
setAnchor :: TextViewClass self => Signal self (IO ())
setAnchor = Signal (connect_NONE__NONE "set-anchor")

-- | The 'setTextViewScrollAdjustments' signal is a keybinding signal which
-- gets emitted to toggle the visibility of the cursor.
-- The default binding for this signal is F7.
--
setTextViewScrollAdjustments :: TextViewClass self => Signal self (Adjustment -> Adjustment -> IO ())
setTextViewScrollAdjustments = Signal (connect_OBJECT_OBJECT__NONE "set-scroll-adjustments")

-- | The 'toggleCursorVisible' signal is a keybinding signal
-- which gets emitted to toggle the visibility of the cursor.
-- The default binding for this signal is F7.
--
toggleCursorVisible :: TextViewClass self => Signal self (IO ())
toggleCursorVisible = Signal (connect_NONE__NONE "toggle-cursor-visible")

-- | Insert Overwrite mode has changed.
--
-- * This signal is emitted when the 'TextView' changes from
-- inserting mode to overwriting mode and vice versa.
--
-- * The action itself happens when the 'TextView' processes this
-- signal.
--
toggleOverwrite :: TextViewClass self => Signal self (IO ())
toggleOverwrite = Signal (connect_NONE__NONE "toggle-overwrite")

-- | If an input method is used, the typed text will not immediately be committed to the buffer. So if
-- you are interested in the text, connect to this signal.
--
-- This signal is only emitted if the text at the given position is actually editable.
textViewPreeditChanged :: (TextViewClass self, GlibString string) => Signal self (string -> IO ())
textViewPreeditChanged = Signal (connect_GLIBSTRING__NONE "preedit-changed")

foreign import ccall unsafe "gtk_text_view_new"
  gtk_text_view_new :: (IO (Ptr Widget))

foreign import ccall safe "gtk_text_view_new_with_buffer"
  gtk_text_view_new_with_buffer :: ((Ptr TextBuffer) -> (IO (Ptr Widget)))

foreign import ccall safe "gtk_text_view_set_buffer"
  gtk_text_view_set_buffer :: ((Ptr TextView) -> ((Ptr TextBuffer) -> (IO ())))

foreign import ccall unsafe "gtk_text_view_get_buffer"
  gtk_text_view_get_buffer :: ((Ptr TextView) -> (IO (Ptr TextBuffer)))

foreign import ccall safe "gtk_text_view_scroll_to_mark"
  gtk_text_view_scroll_to_mark :: ((Ptr TextView) -> ((Ptr TextMark) -> (CDouble -> (CInt -> (CDouble -> (CDouble -> (IO ())))))))

foreign import ccall safe "gtk_text_view_scroll_to_iter"
  gtk_text_view_scroll_to_iter :: ((Ptr TextView) -> ((Ptr TextIter) -> (CDouble -> (CInt -> (CDouble -> (CDouble -> (IO CInt)))))))

foreign import ccall safe "gtk_text_view_scroll_mark_onscreen"
  gtk_text_view_scroll_mark_onscreen :: ((Ptr TextView) -> ((Ptr TextMark) -> (IO ())))

foreign import ccall safe "gtk_text_view_move_mark_onscreen"
  gtk_text_view_move_mark_onscreen :: ((Ptr TextView) -> ((Ptr TextMark) -> (IO CInt)))

foreign import ccall safe "gtk_text_view_place_cursor_onscreen"
  gtk_text_view_place_cursor_onscreen :: ((Ptr TextView) -> (IO CInt))

foreign import ccall unsafe "gtk_text_view_get_visible_rect"
  gtk_text_view_get_visible_rect :: ((Ptr TextView) -> ((Ptr ()) -> (IO ())))

foreign import ccall unsafe "gtk_text_view_get_iter_location"
  gtk_text_view_get_iter_location :: ((Ptr TextView) -> ((Ptr TextIter) -> ((Ptr ()) -> (IO ()))))

foreign import ccall unsafe "gtk_text_view_get_line_at_y"
  gtk_text_view_get_line_at_y :: ((Ptr TextView) -> ((Ptr TextIter) -> (CInt -> ((Ptr CInt) -> (IO ())))))

foreign import ccall unsafe "gtk_text_view_get_line_yrange"
  gtk_text_view_get_line_yrange :: ((Ptr TextView) -> ((Ptr TextIter) -> ((Ptr CInt) -> ((Ptr CInt) -> (IO ())))))

foreign import ccall unsafe "gtk_text_view_get_iter_at_location"
  gtk_text_view_get_iter_at_location :: ((Ptr TextView) -> ((Ptr TextIter) -> (CInt -> (CInt -> (IO ())))))

foreign import ccall unsafe "gtk_text_view_buffer_to_window_coords"
  gtk_text_view_buffer_to_window_coords :: ((Ptr TextView) -> (CInt -> (CInt -> (CInt -> ((Ptr CInt) -> ((Ptr CInt) -> (IO ())))))))

foreign import ccall unsafe "gtk_text_view_window_to_buffer_coords"
  gtk_text_view_window_to_buffer_coords :: ((Ptr TextView) -> (CInt -> (CInt -> (CInt -> ((Ptr CInt) -> ((Ptr CInt) -> (IO ())))))))

foreign import ccall unsafe "gtk_text_view_get_window"
  gtk_text_view_get_window :: ((Ptr TextView) -> (CInt -> (IO (Ptr DrawWindow))))

foreign import ccall unsafe "gtk_text_view_get_window_type"
  gtk_text_view_get_window_type :: ((Ptr TextView) -> ((Ptr DrawWindow) -> (IO CInt)))

foreign import ccall safe "gtk_text_view_set_border_window_size"
  gtk_text_view_set_border_window_size :: ((Ptr TextView) -> (CInt -> (CInt -> (IO ()))))

foreign import ccall unsafe "gtk_text_view_get_border_window_size"
  gtk_text_view_get_border_window_size :: ((Ptr TextView) -> (CInt -> (IO CInt)))

foreign import ccall unsafe "gtk_text_view_forward_display_line"
  gtk_text_view_forward_display_line :: ((Ptr TextView) -> ((Ptr TextIter) -> (IO CInt)))

foreign import ccall unsafe "gtk_text_view_backward_display_line"
  gtk_text_view_backward_display_line :: ((Ptr TextView) -> ((Ptr TextIter) -> (IO CInt)))

foreign import ccall unsafe "gtk_text_view_forward_display_line_end"
  gtk_text_view_forward_display_line_end :: ((Ptr TextView) -> ((Ptr TextIter) -> (IO CInt)))

foreign import ccall unsafe "gtk_text_view_backward_display_line_start"
  gtk_text_view_backward_display_line_start :: ((Ptr TextView) -> ((Ptr TextIter) -> (IO CInt)))

foreign import ccall unsafe "gtk_text_view_starts_display_line"
  gtk_text_view_starts_display_line :: ((Ptr TextView) -> ((Ptr TextIter) -> (IO CInt)))

foreign import ccall unsafe "gtk_text_view_move_visually"
  gtk_text_view_move_visually :: ((Ptr TextView) -> ((Ptr TextIter) -> (CInt -> (IO CInt))))

foreign import ccall safe "gtk_text_view_add_child_at_anchor"
  gtk_text_view_add_child_at_anchor :: ((Ptr TextView) -> ((Ptr Widget) -> ((Ptr TextChildAnchor) -> (IO ()))))

foreign import ccall unsafe "gtk_text_child_anchor_new"
  gtk_text_child_anchor_new :: (IO (Ptr TextChildAnchor))

foreign import ccall safe "gtk_text_child_anchor_get_widgets"
  gtk_text_child_anchor_get_widgets :: ((Ptr TextChildAnchor) -> (IO (Ptr ())))

foreign import ccall unsafe "gtk_text_child_anchor_get_deleted"
  gtk_text_child_anchor_get_deleted :: ((Ptr TextChildAnchor) -> (IO CInt))

foreign import ccall safe "gtk_text_view_add_child_in_window"
  gtk_text_view_add_child_in_window :: ((Ptr TextView) -> ((Ptr Widget) -> (CInt -> (CInt -> (CInt -> (IO ()))))))

foreign import ccall safe "gtk_text_view_move_child"
  gtk_text_view_move_child :: ((Ptr TextView) -> ((Ptr Widget) -> (CInt -> (CInt -> (IO ())))))

foreign import ccall safe "gtk_text_view_set_wrap_mode"
  gtk_text_view_set_wrap_mode :: ((Ptr TextView) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_text_view_get_wrap_mode"
  gtk_text_view_get_wrap_mode :: ((Ptr TextView) -> (IO CInt))

foreign import ccall safe "gtk_text_view_set_editable"
  gtk_text_view_set_editable :: ((Ptr TextView) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_text_view_get_editable"
  gtk_text_view_get_editable :: ((Ptr TextView) -> (IO CInt))

foreign import ccall safe "gtk_text_view_set_cursor_visible"
  gtk_text_view_set_cursor_visible :: ((Ptr TextView) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_text_view_get_cursor_visible"
  gtk_text_view_get_cursor_visible :: ((Ptr TextView) -> (IO CInt))

foreign import ccall safe "gtk_text_view_set_pixels_above_lines"
  gtk_text_view_set_pixels_above_lines :: ((Ptr TextView) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_text_view_get_pixels_above_lines"
  gtk_text_view_get_pixels_above_lines :: ((Ptr TextView) -> (IO CInt))

foreign import ccall safe "gtk_text_view_set_pixels_below_lines"
  gtk_text_view_set_pixels_below_lines :: ((Ptr TextView) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_text_view_get_pixels_below_lines"
  gtk_text_view_get_pixels_below_lines :: ((Ptr TextView) -> (IO CInt))

foreign import ccall safe "gtk_text_view_set_pixels_inside_wrap"
  gtk_text_view_set_pixels_inside_wrap :: ((Ptr TextView) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_text_view_get_pixels_inside_wrap"
  gtk_text_view_get_pixels_inside_wrap :: ((Ptr TextView) -> (IO CInt))

foreign import ccall safe "gtk_text_view_set_justification"
  gtk_text_view_set_justification :: ((Ptr TextView) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_text_view_get_justification"
  gtk_text_view_get_justification :: ((Ptr TextView) -> (IO CInt))

foreign import ccall safe "gtk_text_view_set_left_margin"
  gtk_text_view_set_left_margin :: ((Ptr TextView) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_text_view_get_left_margin"
  gtk_text_view_get_left_margin :: ((Ptr TextView) -> (IO CInt))

foreign import ccall safe "gtk_text_view_set_right_margin"
  gtk_text_view_set_right_margin :: ((Ptr TextView) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_text_view_get_right_margin"
  gtk_text_view_get_right_margin :: ((Ptr TextView) -> (IO CInt))

foreign import ccall safe "gtk_text_view_set_indent"
  gtk_text_view_set_indent :: ((Ptr TextView) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_text_view_get_indent"
  gtk_text_view_get_indent :: ((Ptr TextView) -> (IO CInt))

foreign import ccall safe "gtk_text_view_get_default_attributes"
  gtk_text_view_get_default_attributes :: ((Ptr TextView) -> (IO (Ptr TextAttributes)))

foreign import ccall safe "gtk_text_view_get_iter_at_position"
  gtk_text_view_get_iter_at_position :: ((Ptr TextView) -> ((Ptr TextIter) -> ((Ptr CInt) -> (CInt -> (CInt -> (IO ()))))))

foreign import ccall safe "gtk_text_view_set_overwrite"
  gtk_text_view_set_overwrite :: ((Ptr TextView) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_text_view_get_overwrite"
  gtk_text_view_get_overwrite :: ((Ptr TextView) -> (IO CInt))

foreign import ccall safe "gtk_text_view_set_accepts_tab"
  gtk_text_view_set_accepts_tab :: ((Ptr TextView) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_text_view_get_accepts_tab"
  gtk_text_view_get_accepts_tab :: ((Ptr TextView) -> (IO CInt))

foreign import ccall safe "gtk_text_view_get_hadjustment"
  gtk_text_view_get_hadjustment :: ((Ptr TextView) -> (IO (Ptr Adjustment)))

foreign import ccall safe "gtk_text_view_get_vadjustment"
  gtk_text_view_get_vadjustment :: ((Ptr TextView) -> (IO (Ptr Adjustment)))

foreign import ccall safe "gtk_text_view_im_context_filter_keypress"
  gtk_text_view_im_context_filter_keypress :: ((Ptr TextView) -> ((Ptr ()) -> (IO CInt)))

foreign import ccall safe "gtk_text_view_reset_im_context"
  gtk_text_view_reset_im_context :: ((Ptr TextView) -> (IO ()))
