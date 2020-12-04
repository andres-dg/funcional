
{-# LINE 2 "./Graphics/UI/Gtk/Gdk/Display.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Display - a description of a keyboard/mouse/monitors combination
--
-- Author : Axel Simon
--
-- Created: 22 October 2009
--
-- Copyright (C) 2009 Axel Simon
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
-- Controls the keyboard\/mouse\/monitors combination.
--
-- * Module available since Gdk version 2.2
--
module Graphics.UI.Gtk.Gdk.Display (

-- * Detail
--
-- | 'Display' objects purpose are two fold:
--
-- * To grab\/ungrab keyboard focus and mouse pointer
--
-- * To manage and provide information about the 'Screen'(s) available for
-- this 'Display'
--
-- 'Display' objects are the GDK representation of the X Display which can
-- be described as /a workstation consisting of a keyboard a pointing device
-- (such as a mouse) and one or more screens/. It is used to open and keep
-- track of various 'Screen' objects currently instanciated by the application.
-- It is also used to grab and release the keyboard and the mouse pointer.

-- * Class Hierarchy
--
-- |
-- @
-- | 'GObject'
-- | +----Display
-- @


-- * Types
  Display,
  DisplayClass,
  castToDisplay, gTypeDisplay,
  toDisplay,

-- * Methods
  displayOpen,
  displayGetDefault,
  displayGetName,
  displayGetNScreens,
  displayGetScreen,
  displayGetDefaultScreen,
  displayPointerUngrab,
  displayKeyboardUngrab,
  displayPointerIsGrabbed,
  displayBeep,
  displaySync,

  displayFlush,

  displayClose,
  displayListDevices,
  displaySetDoubleClickTime,

  displaySetDoubleClickDistance,

  displayGetPointer,
  displayGetWindowAtPointer,

  displayWarpPointer,


  displaySupportsCursorColor,
  displaySupportsCursorAlpha,
  displayGetDefaultCursorSize,
  displayGetMaximalCursorSize,
  displayGetDefaultGroup,

  displaySupportsSelectionNotification,
  displayRequestSelectionNotification,
  displaySupportsClipboardPersistence,
  displayStoreClipboard,

  displaySupportsShapes,
  displaySupportsInputShapes,

  displaySupportsComposite,





-- * Signals
  displayClosed,


  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Flags
import System.Glib.GList
import Graphics.UI.Gtk.Types
{-# LINE 122 "./Graphics/UI/Gtk/Gdk/Display.chs" #-}
import Graphics.UI.Gtk.Signals
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.General.DNDTypes (SelectionTag, TargetTag, Atom(..))


{-# LINE 127 "./Graphics/UI/Gtk/Gdk/Display.chs" #-}


--------------------
-- Methods

-- | Opens a display.
--
displayOpen :: GlibString string
 => string -- ^ @displayName@ - the name of the display to open
 -> IO (Maybe Display)
               -- ^ returns a 'Display', or @Nothing@ if the display
               -- could not be opened.
displayOpen displayName =
  maybeNull (wrapNewGObject mkDisplay) $
  withUTFString displayName $ \displayNamePtr ->
  gdk_display_open
{-# LINE 143 "./Graphics/UI/Gtk/Gdk/Display.chs" #-}
    displayNamePtr

-- | Gets the default 'Display'. This is a convenience function for
-- @displayManagerGetDefaultDisplay displayManagerGet@.
--
displayGetDefault ::
    IO (Maybe Display)
               -- ^ returns a 'Display', or @Nothing@ if there is no
               -- default display.
displayGetDefault =
  maybeNull (makeNewGObject mkDisplay) $
  gdk_display_get_default
{-# LINE 155 "./Graphics/UI/Gtk/Gdk/Display.chs" #-}

-- | Gets the name of the display.
--
displayGetName :: GlibString string => Display
 -> IO string -- ^ returns a string representing the display name
displayGetName self =
  (\(Display arg1) -> withForeignPtr arg1 $ \argPtr1 ->gdk_display_get_name argPtr1)
{-# LINE 162 "./Graphics/UI/Gtk/Gdk/Display.chs" #-}
    self
  >>= peekUTFString

-- | Gets the number of screen managed by the @display@.
--
displayGetNScreens :: Display
 -> IO Int -- ^ returns number of screens.
displayGetNScreens self =
  liftM fromIntegral $
  (\(Display arg1) -> withForeignPtr arg1 $ \argPtr1 ->gdk_display_get_n_screens argPtr1)
{-# LINE 172 "./Graphics/UI/Gtk/Gdk/Display.chs" #-}
    self

-- | Returns a screen object for one of the screens of the display.
--
displayGetScreen :: Display
 -> Int -- ^ @screenNum@ - the screen number
 -> IO Screen -- ^ returns the 'Screen' object
displayGetScreen self screenNum =
  makeNewGObject mkScreen $
  (\(Display arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gdk_display_get_screen argPtr1 arg2)
{-# LINE 182 "./Graphics/UI/Gtk/Gdk/Display.chs" #-}
    self
    (fromIntegral screenNum)

-- | Get the default 'Screen' for @display@.
--
displayGetDefaultScreen :: Display
 -> IO Screen -- ^ returns the default 'Screen' object for @display@
displayGetDefaultScreen self =
  makeNewGObject mkScreen $
  (\(Display arg1) -> withForeignPtr arg1 $ \argPtr1 ->gdk_display_get_default_screen argPtr1)
{-# LINE 192 "./Graphics/UI/Gtk/Gdk/Display.chs" #-}
    self

-- | Release any pointer grab.
--
displayPointerUngrab :: Display
 -> TimeStamp -- ^ @time@ - a timestap (e.g. 'currentTime').
 -> IO ()
displayPointerUngrab self time =
  (\(Display arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gdk_display_pointer_ungrab argPtr1 arg2)
{-# LINE 201 "./Graphics/UI/Gtk/Gdk/Display.chs" #-}
    self
    (fromIntegral time)

-- | Release any keyboard grab
--
displayKeyboardUngrab :: Display
 -> TimeStamp -- ^ @time@ - a timestap (e.g 'currentTime').
 -> IO ()
displayKeyboardUngrab self time =
  (\(Display arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gdk_display_keyboard_ungrab argPtr1 arg2)
{-# LINE 211 "./Graphics/UI/Gtk/Gdk/Display.chs" #-}
    self
    (fromIntegral time)

-- | Test if the pointer is grabbed.
--
displayPointerIsGrabbed :: Display
 -> IO Bool -- ^ returns @True@ if an active X pointer grab is in effect
displayPointerIsGrabbed self =
  liftM toBool $
  (\(Display arg1) -> withForeignPtr arg1 $ \argPtr1 ->gdk_display_pointer_is_grabbed argPtr1)
{-# LINE 221 "./Graphics/UI/Gtk/Gdk/Display.chs" #-}
    self

-- | Emits a short beep on @display@
--
displayBeep :: Display -> IO ()
displayBeep self =
  (\(Display arg1) -> withForeignPtr arg1 $ \argPtr1 ->gdk_display_beep argPtr1)
{-# LINE 228 "./Graphics/UI/Gtk/Gdk/Display.chs" #-}
    self

-- | Flushes any requests queued for the windowing system and waits until all
-- requests have been handled. This is often used for making sure that the
-- display is synchronized with the current state of the program. Calling
-- 'displaySync' before 'errorTrapPop' makes sure that any errors generated
-- from earlier requests are handled before the error trap is removed.
--
-- This is most useful for X11. On windowing systems where requests are
-- handled synchronously, this function will do nothing.
--
displaySync :: Display -> IO ()
displaySync self =
  (\(Display arg1) -> withForeignPtr arg1 $ \argPtr1 ->gdk_display_sync argPtr1)
{-# LINE 242 "./Graphics/UI/Gtk/Gdk/Display.chs" #-}
    self


-- | Flushes any requests queued for the windowing system; this happens
-- automatically when the main loop blocks waiting for new events, but if your
-- application is drawing without returning control to the main loop, you may
-- need to call this function explicitely. A common case where this function
-- needs to be called is when an application is executing drawing commands from
-- a thread other than the thread where the main loop is running.
--
-- This is most useful for X11. On windowing systems where requests are
-- handled synchronously, this function will do nothing.
--
-- * Available since Gdk version 2.4
--
displayFlush :: Display -> IO ()
displayFlush self =
  (\(Display arg1) -> withForeignPtr arg1 $ \argPtr1 ->gdk_display_flush argPtr1)
{-# LINE 260 "./Graphics/UI/Gtk/Gdk/Display.chs" #-}
    self


-- | Closes the connection to the windowing system for the given display, and
-- cleans up associated resources.
--
displayClose :: Display -> IO ()
displayClose self =
  (\(Display arg1) -> withForeignPtr arg1 $ \argPtr1 ->gdk_display_close argPtr1)
{-# LINE 269 "./Graphics/UI/Gtk/Gdk/Display.chs" #-}
    self

-- | Returns the list of available input devices attached to @display@.
--
displayListDevices :: Display
 -> IO [Device] -- ^ returns a list of 'Device'
displayListDevices self =
  (\(Display arg1) -> withForeignPtr arg1 $ \argPtr1 ->gdk_display_list_devices argPtr1)
{-# LINE 277 "./Graphics/UI/Gtk/Gdk/Display.chs" #-}
    self
  >>= readGList
  >>= mapM (makeNewGObject mkDevice . return)


-- | Sets the double click time (two clicks within this time interval count as
-- a double click and result in an 'eventButton' where 'eventClick' is
-- 'DoubleClick'). Applications should /not/ set this, it is a global
-- user-configured setting.
--
displaySetDoubleClickTime :: Display
 -> Int -- ^ @msec@ - double click time in milliseconds (thousandths of a
        -- second)
 -> IO ()
displaySetDoubleClickTime self msec =
  (\(Display arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gdk_display_set_double_click_time argPtr1 arg2)
{-# LINE 293 "./Graphics/UI/Gtk/Gdk/Display.chs" #-}
    self
    (fromIntegral msec)


-- | Sets the double click distance (two clicks within this distance count as
-- a double click and result in an 'eventButton' where 'eventClick' is
-- 'DoubleClick'). See also 'displaySetDoubleClickTime'. Applications should
-- /not/ set this, it is a global user-configured setting.
--
-- * Available since Gdk version 2.4
--
displaySetDoubleClickDistance :: Display
 -> Int -- ^ @distance@ - distance in pixels
 -> IO ()
displaySetDoubleClickDistance self distance =
  (\(Display arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gdk_display_set_double_click_distance argPtr1 arg2)
{-# LINE 309 "./Graphics/UI/Gtk/Gdk/Display.chs" #-}
    self
    (fromIntegral distance)


-- | Gets the current location of the pointer and the current modifier mask
-- for a given display.
--
displayGetPointer :: Display
 -> IO (Screen, [Modifier], Int, Int)
  -- ^ @(s, m, x, y)@ - the screen @s@, the modifier mask @m@ and the @x@ and
  -- @y@ coordinates of the pointer
displayGetPointer self =
  alloca $ \sPtr ->
  alloca $ \xPtr ->
  alloca $ \yPtr ->
  alloca $ \mPtr ->
  (\(Display arg1) arg2 arg3 arg4 arg5 -> withForeignPtr arg1 $ \argPtr1 ->gdk_display_get_pointer argPtr1 arg2 arg3 arg4 arg5)
{-# LINE 326 "./Graphics/UI/Gtk/Gdk/Display.chs" #-}
    self
    (castPtr sPtr)
    xPtr
    yPtr
    mPtr >>
  makeNewGObject mkScreen (peek sPtr) >>= \s ->
  peek xPtr >>= \x ->
  peek yPtr >>= \y ->
  peek mPtr >>= \m ->
  return (s, toFlags (fromIntegral m), fromIntegral x, fromIntegral y)

-- | Obtains the window underneath the mouse pointer, returning the location
-- of the pointer in that window in @winX@, @winY@ for @screen@. Returns
-- @Nothing@ if
-- the window under the mouse pointer is not known to GDK (for example, belongs
-- to another application).
--
displayGetWindowAtPointer :: Display
 -> IO (Maybe (DrawWindow, Int, Int))
  -- ^ @(screen, winX, winY)@ returns the window under the mouse
  -- pointer, or @Nothing@. The @winX@ and @winY@ denote the pointer location
  -- relative to the window origin
displayGetWindowAtPointer self =
  alloca $ \winXPtr ->
  alloca $ \winYPtr -> do
  wPtr <- (\(Display arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gdk_display_get_window_at_pointer argPtr1 arg2 arg3)
{-# LINE 352 "./Graphics/UI/Gtk/Gdk/Display.chs" #-}
    self
    winXPtr
    winYPtr
  if wPtr==nullPtr then return Nothing else
    peek winXPtr >>= \winX ->
    peek winYPtr >>= \winY ->
    makeNewGObject mkDrawWindow (return wPtr) >>= \win ->
    return (Just (win, fromIntegral winX, fromIntegral winY))

{- not worth the trouble

-- | This function allows for hooking into the operation of getting the
-- current location of the pointer on a particular display. This is only useful
-- for such low-level tools as an event recorder. Applications should never
-- have any reason to use this facility.
--
displaySetPointerHooks :: Display
 -> {-const-GdkDisplayPointerHooks*-} -- ^ @newHooks@ - a table of pointers to
                                      -- functions for getting quantities
                                      -- related to the current pointer
                                      -- position, or {@NULL@, FIXME: this
                                      -- should probably be converted to a
                                      -- Maybe data type} to restore the
                                      -- default table.
 -> IO {-GdkDisplayPointerHooks*-} -- ^ returns the previous pointer hook
                                      -- table
displaySetPointerHooks self newHooks =
  {# call gdk_display_set_pointer_hooks #}
    self
    {-newHooks-}
-}


-- | Moves the pointer of @display@ to the point @x@,@y@ on the screen
-- @screen@, unless the pointer is confined to a window by a grab, in which
-- case it will be moved as far as allowed by the grab. Warping the pointer
-- creates events as if the user had moved the mouse instantaneously to the
-- destination.
--
-- Note that the pointer should normally be under the control of the user.
-- This function was added to cover some rare use cases like keyboard
-- navigation support for the color picker in the 'ColorSelectionDialog'.
--
-- * Available since Gdk version 2.8
--
displayWarpPointer :: Display
 -> Screen -- ^ @screen@ - the screen of @display@ to warp the pointer to
 -> Int -- ^ @x@ - the x coordinate of the destination
 -> Int -- ^ @y@ - the y coordinate of the destination
 -> IO ()
displayWarpPointer self screen x y =
  (\(Display arg1) (Screen arg2) arg3 arg4 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gdk_display_warp_pointer argPtr1 argPtr2 arg3 arg4)
{-# LINE 404 "./Graphics/UI/Gtk/Gdk/Display.chs" #-}
    self
    screen
    (fromIntegral x)
    (fromIntegral y)



-- | Returns @True@ if multicolored cursors are supported on @display@.
-- Otherwise, cursors have only a forground and a background color.
--
-- * Available since Gdk version 2.4
--
displaySupportsCursorColor :: Display
 -> IO Bool -- ^ returns whether cursors can have multiple colors.
displaySupportsCursorColor self =
  liftM toBool $
  (\(Display arg1) -> withForeignPtr arg1 $ \argPtr1 ->gdk_display_supports_cursor_color argPtr1)
{-# LINE 421 "./Graphics/UI/Gtk/Gdk/Display.chs" #-}
    self

-- | Returns @True@ if cursors can use an 8bit alpha channel on @display@.
-- Otherwise, cursors are restricted to bilevel alpha (i.e. a mask).
--
-- * Available since Gdk version 2.4
--
displaySupportsCursorAlpha :: Display
 -> IO Bool -- ^ returns whether cursors can have alpha channels.
displaySupportsCursorAlpha self =
  liftM toBool $
  (\(Display arg1) -> withForeignPtr arg1 $ \argPtr1 ->gdk_display_supports_cursor_alpha argPtr1)
{-# LINE 433 "./Graphics/UI/Gtk/Gdk/Display.chs" #-}
    self

-- | Returns the default size to use for cursors on @display@.
--
-- * Available since Gdk version 2.4
--
displayGetDefaultCursorSize :: Display
 -> IO Int -- ^ returns the default cursor size.
displayGetDefaultCursorSize self =
  liftM fromIntegral $
  (\(Display arg1) -> withForeignPtr arg1 $ \argPtr1 ->gdk_display_get_default_cursor_size argPtr1)
{-# LINE 444 "./Graphics/UI/Gtk/Gdk/Display.chs" #-}
    self

-- | Gets the maximal size to use for cursors on @display@.
--
-- * Available since Gdk version 2.4
--
displayGetMaximalCursorSize :: Display
 -> IO (Int, Int) -- ^ @(width, height)@
                  -- maximal @width@ and @height@ of the cursor
displayGetMaximalCursorSize self =
  alloca $ \widthPtr ->
  alloca $ \heightPtr ->
  (\(Display arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gdk_display_get_maximal_cursor_size argPtr1 arg2 arg3)
{-# LINE 457 "./Graphics/UI/Gtk/Gdk/Display.chs" #-}
    self
    widthPtr
    heightPtr >>
  peek widthPtr >>= \width ->
  peek heightPtr >>= \height ->
  return (fromIntegral width, fromIntegral height)

-- | Returns the default group leader window for all toplevel windows on
-- @display@. This window is implicitly created by GDK. See 'windowSetGroup'.
--
-- * Available since Gdk version 2.4
--
displayGetDefaultGroup :: Display
 -> IO DrawWindow -- ^ returns The default group leader window for @display@
displayGetDefaultGroup self =
  makeNewGObject mkDrawWindow $
  (\(Display arg1) -> withForeignPtr arg1 $ \argPtr1 ->gdk_display_get_default_group argPtr1)
{-# LINE 474 "./Graphics/UI/Gtk/Gdk/Display.chs" #-}
    self


-- | Returns whether 'EOwnerChange' events will be
-- sent when the owner of a selection changes.
--
-- * Available since Gdk version 2.6
--
displaySupportsSelectionNotification :: Display
 -> IO Bool -- ^ returns whether 'EOwnerChange'
            -- events will be sent.
displaySupportsSelectionNotification self =
  liftM toBool $
  (\(Display arg1) -> withForeignPtr arg1 $ \argPtr1 ->gdk_display_supports_selection_notification argPtr1)
{-# LINE 488 "./Graphics/UI/Gtk/Gdk/Display.chs" #-}
    self

-- | Request 'EOwnerChange' events for ownership
-- changes of the selection named by the given atom.
--
-- * Available since Gdk version 2.6
--
displayRequestSelectionNotification :: Display
 -> SelectionTag -- ^ @selection@ - the 'Atom' naming
                -- the selection for which ownership change notification is
                -- requested
 -> IO Bool -- ^ returns whether 'EOwnerChange'
                -- events will be sent.
displayRequestSelectionNotification self (Atom selection) =
  liftM toBool $
  (\(Display arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gdk_display_request_selection_notification argPtr1 arg2)
{-# LINE 504 "./Graphics/UI/Gtk/Gdk/Display.chs" #-}
    self
    selection

-- | Returns whether the speicifed display supports clipboard persistance;
-- i.e. if it's possible to store the clipboard data after an application has
-- quit. On X11 this checks if a clipboard daemon is running.
--
-- * Available since Gdk version 2.6
--
displaySupportsClipboardPersistence :: Display
 -> IO Bool -- ^ returns @True@ if the display supports clipboard persistance.
displaySupportsClipboardPersistence self =
  liftM toBool $
  (\(Display arg1) -> withForeignPtr arg1 $ \argPtr1 ->gdk_display_supports_clipboard_persistence argPtr1)
{-# LINE 518 "./Graphics/UI/Gtk/Gdk/Display.chs" #-}
    self

-- | Issues a request to the clipboard manager to store the clipboard data. On
-- X11, this is a special program that works according to the freedesktop
-- clipboard specification, available at
-- http:\/\/www.freedesktop.org\/Standards\/clipboard-manager-spec.
--
-- * Available since Gdk version 2.6
--
displayStoreClipboard :: Display
 -> DrawWindow -- ^ @clipboardWindow@ - a 'DrawWindow' belonging to
                       -- the clipboard owner
 -> Word32 -- ^ @time@ - a timestamp
 -> (Maybe [TargetTag]) -- ^ @targets@ - an array of targets that should be
                         -- saved, or @Nothing@ if all available
                         -- targets should be saved.
 -> IO ()
displayStoreClipboard self clipboardWindow time (Just targets) =
  withArrayLen (map (\(Atom a) -> a) targets) $ \nTargets tPtr ->
  (\(Display arg1) (DrawWindow arg2) arg3 arg4 arg5 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gdk_display_store_clipboard argPtr1 argPtr2 arg3 arg4 arg5)
{-# LINE 538 "./Graphics/UI/Gtk/Gdk/Display.chs" #-}
    self
    clipboardWindow
    (fromIntegral time)
    tPtr
    (fromIntegral nTargets)
displayStoreClipboard self clipboardWindow time Nothing =
  (\(Display arg1) (DrawWindow arg2) arg3 arg4 arg5 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gdk_display_store_clipboard argPtr1 argPtr2 arg3 arg4 arg5)
{-# LINE 545 "./Graphics/UI/Gtk/Gdk/Display.chs" #-}
    self
    clipboardWindow
    (fromIntegral time)
    nullPtr
    0


-- | Returns @True@ if 'windowShapeCombineMask' can be used to create shaped
-- windows on @display@.
--
-- * Available since Gdk version 2.10
--
displaySupportsShapes :: Display
 -> IO Bool -- ^ returns @True@ if shaped windows are supported
displaySupportsShapes self =
  liftM toBool $
  (\(Display arg1) -> withForeignPtr arg1 $ \argPtr1 ->gdk_display_supports_shapes argPtr1)
{-# LINE 562 "./Graphics/UI/Gtk/Gdk/Display.chs" #-}
    self

-- | Returns @True@ if 'windowInputShapeCombineMask' can be used to modify the
-- input shape of windows on @display@.
--
-- * Available since Gdk version 2.10
--
displaySupportsInputShapes :: Display
 -> IO Bool -- ^ returns @True@ if windows with modified input shape are
            -- supported
displaySupportsInputShapes self =
  liftM toBool $
  (\(Display arg1) -> withForeignPtr arg1 $ \argPtr1 ->gdk_display_supports_input_shapes argPtr1)
{-# LINE 575 "./Graphics/UI/Gtk/Gdk/Display.chs" #-}
    self


-- | Returns @True@ if 'windowSetComposited' can be used to redirect drawing
-- on the window using compositing.
--
-- Currently this only works on X11 with XComposite and XDamage extensions
-- available.
--
-- * Available since Gdk version 2.12
--
displaySupportsComposite :: Display
 -> IO Bool -- ^ returns @True@ if windows may be composited.
displaySupportsComposite self =
  liftM toBool $
  (\(Display arg1) -> withForeignPtr arg1 $ \argPtr1 ->gdk_display_supports_composite argPtr1)
{-# LINE 591 "./Graphics/UI/Gtk/Gdk/Display.chs" #-}
    self





--------------------
-- Signals

-- | The 'displayClosed' signal is emitted when the connection to the windowing
-- system for @display@ is closed.
--
displayClosed :: DisplayClass self => Signal self (Bool -> IO ())
displayClosed = Signal (connect_BOOL__NONE "closed")

foreign import ccall safe "gdk_display_open"
  gdk_display_open :: ((Ptr CChar) -> (IO (Ptr Display)))

foreign import ccall safe "gdk_display_get_default"
  gdk_display_get_default :: (IO (Ptr Display))

foreign import ccall safe "gdk_display_get_name"
  gdk_display_get_name :: ((Ptr Display) -> (IO (Ptr CChar)))

foreign import ccall safe "gdk_display_get_n_screens"
  gdk_display_get_n_screens :: ((Ptr Display) -> (IO CInt))

foreign import ccall safe "gdk_display_get_screen"
  gdk_display_get_screen :: ((Ptr Display) -> (CInt -> (IO (Ptr Screen))))

foreign import ccall safe "gdk_display_get_default_screen"
  gdk_display_get_default_screen :: ((Ptr Display) -> (IO (Ptr Screen)))

foreign import ccall safe "gdk_display_pointer_ungrab"
  gdk_display_pointer_ungrab :: ((Ptr Display) -> (CUInt -> (IO ())))

foreign import ccall safe "gdk_display_keyboard_ungrab"
  gdk_display_keyboard_ungrab :: ((Ptr Display) -> (CUInt -> (IO ())))

foreign import ccall safe "gdk_display_pointer_is_grabbed"
  gdk_display_pointer_is_grabbed :: ((Ptr Display) -> (IO CInt))

foreign import ccall safe "gdk_display_beep"
  gdk_display_beep :: ((Ptr Display) -> (IO ()))

foreign import ccall safe "gdk_display_sync"
  gdk_display_sync :: ((Ptr Display) -> (IO ()))

foreign import ccall safe "gdk_display_flush"
  gdk_display_flush :: ((Ptr Display) -> (IO ()))

foreign import ccall safe "gdk_display_close"
  gdk_display_close :: ((Ptr Display) -> (IO ()))

foreign import ccall safe "gdk_display_list_devices"
  gdk_display_list_devices :: ((Ptr Display) -> (IO (Ptr ())))

foreign import ccall safe "gdk_display_set_double_click_time"
  gdk_display_set_double_click_time :: ((Ptr Display) -> (CUInt -> (IO ())))

foreign import ccall safe "gdk_display_set_double_click_distance"
  gdk_display_set_double_click_distance :: ((Ptr Display) -> (CUInt -> (IO ())))

foreign import ccall safe "gdk_display_get_pointer"
  gdk_display_get_pointer :: ((Ptr Display) -> ((Ptr Screen) -> ((Ptr CInt) -> ((Ptr CInt) -> ((Ptr CInt) -> (IO ()))))))

foreign import ccall safe "gdk_display_get_window_at_pointer"
  gdk_display_get_window_at_pointer :: ((Ptr Display) -> ((Ptr CInt) -> ((Ptr CInt) -> (IO (Ptr DrawWindow)))))

foreign import ccall safe "gdk_display_warp_pointer"
  gdk_display_warp_pointer :: ((Ptr Display) -> ((Ptr Screen) -> (CInt -> (CInt -> (IO ())))))

foreign import ccall safe "gdk_display_supports_cursor_color"
  gdk_display_supports_cursor_color :: ((Ptr Display) -> (IO CInt))

foreign import ccall safe "gdk_display_supports_cursor_alpha"
  gdk_display_supports_cursor_alpha :: ((Ptr Display) -> (IO CInt))

foreign import ccall safe "gdk_display_get_default_cursor_size"
  gdk_display_get_default_cursor_size :: ((Ptr Display) -> (IO CUInt))

foreign import ccall safe "gdk_display_get_maximal_cursor_size"
  gdk_display_get_maximal_cursor_size :: ((Ptr Display) -> ((Ptr CUInt) -> ((Ptr CUInt) -> (IO ()))))

foreign import ccall safe "gdk_display_get_default_group"
  gdk_display_get_default_group :: ((Ptr Display) -> (IO (Ptr DrawWindow)))

foreign import ccall safe "gdk_display_supports_selection_notification"
  gdk_display_supports_selection_notification :: ((Ptr Display) -> (IO CInt))

foreign import ccall safe "gdk_display_request_selection_notification"
  gdk_display_request_selection_notification :: ((Ptr Display) -> ((Ptr ()) -> (IO CInt)))

foreign import ccall safe "gdk_display_supports_clipboard_persistence"
  gdk_display_supports_clipboard_persistence :: ((Ptr Display) -> (IO CInt))

foreign import ccall safe "gdk_display_store_clipboard"
  gdk_display_store_clipboard :: ((Ptr Display) -> ((Ptr DrawWindow) -> (CUInt -> ((Ptr (Ptr ())) -> (CInt -> (IO ()))))))

foreign import ccall safe "gdk_display_supports_shapes"
  gdk_display_supports_shapes :: ((Ptr Display) -> (IO CInt))

foreign import ccall safe "gdk_display_supports_input_shapes"
  gdk_display_supports_input_shapes :: ((Ptr Display) -> (IO CInt))

foreign import ccall safe "gdk_display_supports_composite"
  gdk_display_supports_composite :: ((Ptr Display) -> (IO CInt))
