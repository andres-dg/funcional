
{-# LINE 2 "./Graphics/UI/Gtk/Gdk/Cursor.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Cursor
--
-- Author : Bit Connor <bit@mutantlemon.com>
-- Andy Stewart <lazycat.manatee@gmail.com>
--
-- Created: 18 November 2007
--
-- Copyright (C) 2007 Bit Connor
-- Copyright (C) 2009 Andy Stewart
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
-- Cursors | Standard and pixmap cursors.
--
module Graphics.UI.Gtk.Gdk.Cursor (
-- * Types
  Cursor(..),

-- * Enums
  CursorType(..),

-- * Constructors
  cursorNew,

-- * Methods

  cursorNewFromPixmap,

  cursorNewFromPixbuf,
  cursorNewFromName,
  cursorNewForDisplay,
  cursorGetDisplay,
  cursorGetImage
  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.UTFString

import Graphics.UI.Gtk.General.Structs (Color)


import Graphics.UI.Gtk.Types hiding (Arrow)


{-# LINE 62 "./Graphics/UI/Gtk/Gdk/Cursor.chs" #-}

--------------------
-- Types
newtype Cursor = Cursor (ForeignPtr (Cursor))
{-# LINE 66 "./Graphics/UI/Gtk/Gdk/Cursor.chs" #-}

--------------------
-- Enums
-- | Cursor types.
data CursorType = XCursor
                | Arrow
                | BasedArrowDown
                | BasedArrowUp
                | Boat
                | Bogosity
                | BottomLeftCorner
                | BottomRightCorner
                | BottomSide
                | BottomTee
                | BoxSpiral
                | CenterPtr
                | Circle
                | Clock
                | CoffeeMug
                | Cross
                | CrossReverse
                | Crosshair
                | DiamondCross
                | Dot
                | Dotbox
                | DoubleArrow
                | DraftLarge
                | DraftSmall
                | DrapedBox
                | Exchange
                | Fleur
                | Gobbler
                | Gumby
                | Hand1
                | Hand2
                | Heart
                | Icon
                | IronCross
                | LeftPtr
                | LeftSide
                | LeftTee
                | Leftbutton
                | LlAngle
                | LrAngle
                | Man
                | Middlebutton
                | Mouse
                | Pencil
                | Pirate
                | Plus
                | QuestionArrow
                | RightPtr
                | RightSide
                | RightTee
                | Rightbutton
                | RtlLogo
                | Sailboat
                | SbDownArrow
                | SbHDoubleArrow
                | SbLeftArrow
                | SbRightArrow
                | SbUpArrow
                | SbVDoubleArrow
                | Shuttle
                | Sizing
                | Spider
                | Spraycan
                | Star
                | Target
                | Tcross
                | TopLeftArrow
                | TopLeftCorner
                | TopRightCorner
                | TopSide
                | TopTee
                | Trek
                | UlAngle
                | Umbrella
                | UrAngle
                | Watch
                | Xterm
                | LastCursor
                | BlankCursor
                | CursorIsPixmap
                deriving (Bounded,Eq,Show)
instance Enum CursorType where
  fromEnum XCursor = 0
  fromEnum Arrow = 2
  fromEnum BasedArrowDown = 4
  fromEnum BasedArrowUp = 6
  fromEnum Boat = 8
  fromEnum Bogosity = 10
  fromEnum BottomLeftCorner = 12
  fromEnum BottomRightCorner = 14
  fromEnum BottomSide = 16
  fromEnum BottomTee = 18
  fromEnum BoxSpiral = 20
  fromEnum CenterPtr = 22
  fromEnum Circle = 24
  fromEnum Clock = 26
  fromEnum CoffeeMug = 28
  fromEnum Cross = 30
  fromEnum CrossReverse = 32
  fromEnum Crosshair = 34
  fromEnum DiamondCross = 36
  fromEnum Dot = 38
  fromEnum Dotbox = 40
  fromEnum DoubleArrow = 42
  fromEnum DraftLarge = 44
  fromEnum DraftSmall = 46
  fromEnum DrapedBox = 48
  fromEnum Exchange = 50
  fromEnum Fleur = 52
  fromEnum Gobbler = 54
  fromEnum Gumby = 56
  fromEnum Hand1 = 58
  fromEnum Hand2 = 60
  fromEnum Heart = 62
  fromEnum Icon = 64
  fromEnum IronCross = 66
  fromEnum LeftPtr = 68
  fromEnum LeftSide = 70
  fromEnum LeftTee = 72
  fromEnum Leftbutton = 74
  fromEnum LlAngle = 76
  fromEnum LrAngle = 78
  fromEnum Man = 80
  fromEnum Middlebutton = 82
  fromEnum Mouse = 84
  fromEnum Pencil = 86
  fromEnum Pirate = 88
  fromEnum Plus = 90
  fromEnum QuestionArrow = 92
  fromEnum RightPtr = 94
  fromEnum RightSide = 96
  fromEnum RightTee = 98
  fromEnum Rightbutton = 100
  fromEnum RtlLogo = 102
  fromEnum Sailboat = 104
  fromEnum SbDownArrow = 106
  fromEnum SbHDoubleArrow = 108
  fromEnum SbLeftArrow = 110
  fromEnum SbRightArrow = 112
  fromEnum SbUpArrow = 114
  fromEnum SbVDoubleArrow = 116
  fromEnum Shuttle = 118
  fromEnum Sizing = 120
  fromEnum Spider = 122
  fromEnum Spraycan = 124
  fromEnum Star = 126
  fromEnum Target = 128
  fromEnum Tcross = 130
  fromEnum TopLeftArrow = 132
  fromEnum TopLeftCorner = 134
  fromEnum TopRightCorner = 136
  fromEnum TopSide = 138
  fromEnum TopTee = 140
  fromEnum Trek = 142
  fromEnum UlAngle = 144
  fromEnum Umbrella = 146
  fromEnum UrAngle = 148
  fromEnum Watch = 150
  fromEnum Xterm = 152
  fromEnum LastCursor = 153
  fromEnum BlankCursor = (-2)
  fromEnum CursorIsPixmap = (-1)

  toEnum 0 = XCursor
  toEnum 2 = Arrow
  toEnum 4 = BasedArrowDown
  toEnum 6 = BasedArrowUp
  toEnum 8 = Boat
  toEnum 10 = Bogosity
  toEnum 12 = BottomLeftCorner
  toEnum 14 = BottomRightCorner
  toEnum 16 = BottomSide
  toEnum 18 = BottomTee
  toEnum 20 = BoxSpiral
  toEnum 22 = CenterPtr
  toEnum 24 = Circle
  toEnum 26 = Clock
  toEnum 28 = CoffeeMug
  toEnum 30 = Cross
  toEnum 32 = CrossReverse
  toEnum 34 = Crosshair
  toEnum 36 = DiamondCross
  toEnum 38 = Dot
  toEnum 40 = Dotbox
  toEnum 42 = DoubleArrow
  toEnum 44 = DraftLarge
  toEnum 46 = DraftSmall
  toEnum 48 = DrapedBox
  toEnum 50 = Exchange
  toEnum 52 = Fleur
  toEnum 54 = Gobbler
  toEnum 56 = Gumby
  toEnum 58 = Hand1
  toEnum 60 = Hand2
  toEnum 62 = Heart
  toEnum 64 = Icon
  toEnum 66 = IronCross
  toEnum 68 = LeftPtr
  toEnum 70 = LeftSide
  toEnum 72 = LeftTee
  toEnum 74 = Leftbutton
  toEnum 76 = LlAngle
  toEnum 78 = LrAngle
  toEnum 80 = Man
  toEnum 82 = Middlebutton
  toEnum 84 = Mouse
  toEnum 86 = Pencil
  toEnum 88 = Pirate
  toEnum 90 = Plus
  toEnum 92 = QuestionArrow
  toEnum 94 = RightPtr
  toEnum 96 = RightSide
  toEnum 98 = RightTee
  toEnum 100 = Rightbutton
  toEnum 102 = RtlLogo
  toEnum 104 = Sailboat
  toEnum 106 = SbDownArrow
  toEnum 108 = SbHDoubleArrow
  toEnum 110 = SbLeftArrow
  toEnum 112 = SbRightArrow
  toEnum 114 = SbUpArrow
  toEnum 116 = SbVDoubleArrow
  toEnum 118 = Shuttle
  toEnum 120 = Sizing
  toEnum 122 = Spider
  toEnum 124 = Spraycan
  toEnum 126 = Star
  toEnum 128 = Target
  toEnum 130 = Tcross
  toEnum 132 = TopLeftArrow
  toEnum 134 = TopLeftCorner
  toEnum 136 = TopRightCorner
  toEnum 138 = TopSide
  toEnum 140 = TopTee
  toEnum 142 = Trek
  toEnum 144 = UlAngle
  toEnum 146 = Umbrella
  toEnum 148 = UrAngle
  toEnum 150 = Watch
  toEnum 152 = Xterm
  toEnum 153 = LastCursor
  toEnum (-2) = BlankCursor
  toEnum (-1) = CursorIsPixmap
  toEnum unmatched = error ("CursorType.toEnum: Cannot match " ++ show unmatched)

  succ XCursor = Arrow
  succ Arrow = BasedArrowDown
  succ BasedArrowDown = BasedArrowUp
  succ BasedArrowUp = Boat
  succ Boat = Bogosity
  succ Bogosity = BottomLeftCorner
  succ BottomLeftCorner = BottomRightCorner
  succ BottomRightCorner = BottomSide
  succ BottomSide = BottomTee
  succ BottomTee = BoxSpiral
  succ BoxSpiral = CenterPtr
  succ CenterPtr = Circle
  succ Circle = Clock
  succ Clock = CoffeeMug
  succ CoffeeMug = Cross
  succ Cross = CrossReverse
  succ CrossReverse = Crosshair
  succ Crosshair = DiamondCross
  succ DiamondCross = Dot
  succ Dot = Dotbox
  succ Dotbox = DoubleArrow
  succ DoubleArrow = DraftLarge
  succ DraftLarge = DraftSmall
  succ DraftSmall = DrapedBox
  succ DrapedBox = Exchange
  succ Exchange = Fleur
  succ Fleur = Gobbler
  succ Gobbler = Gumby
  succ Gumby = Hand1
  succ Hand1 = Hand2
  succ Hand2 = Heart
  succ Heart = Icon
  succ Icon = IronCross
  succ IronCross = LeftPtr
  succ LeftPtr = LeftSide
  succ LeftSide = LeftTee
  succ LeftTee = Leftbutton
  succ Leftbutton = LlAngle
  succ LlAngle = LrAngle
  succ LrAngle = Man
  succ Man = Middlebutton
  succ Middlebutton = Mouse
  succ Mouse = Pencil
  succ Pencil = Pirate
  succ Pirate = Plus
  succ Plus = QuestionArrow
  succ QuestionArrow = RightPtr
  succ RightPtr = RightSide
  succ RightSide = RightTee
  succ RightTee = Rightbutton
  succ Rightbutton = RtlLogo
  succ RtlLogo = Sailboat
  succ Sailboat = SbDownArrow
  succ SbDownArrow = SbHDoubleArrow
  succ SbHDoubleArrow = SbLeftArrow
  succ SbLeftArrow = SbRightArrow
  succ SbRightArrow = SbUpArrow
  succ SbUpArrow = SbVDoubleArrow
  succ SbVDoubleArrow = Shuttle
  succ Shuttle = Sizing
  succ Sizing = Spider
  succ Spider = Spraycan
  succ Spraycan = Star
  succ Star = Target
  succ Target = Tcross
  succ Tcross = TopLeftArrow
  succ TopLeftArrow = TopLeftCorner
  succ TopLeftCorner = TopRightCorner
  succ TopRightCorner = TopSide
  succ TopSide = TopTee
  succ TopTee = Trek
  succ Trek = UlAngle
  succ UlAngle = Umbrella
  succ Umbrella = UrAngle
  succ UrAngle = Watch
  succ Watch = Xterm
  succ Xterm = LastCursor
  succ LastCursor = BlankCursor
  succ BlankCursor = CursorIsPixmap
  succ _ = undefined

  pred Arrow = XCursor
  pred BasedArrowDown = Arrow
  pred BasedArrowUp = BasedArrowDown
  pred Boat = BasedArrowUp
  pred Bogosity = Boat
  pred BottomLeftCorner = Bogosity
  pred BottomRightCorner = BottomLeftCorner
  pred BottomSide = BottomRightCorner
  pred BottomTee = BottomSide
  pred BoxSpiral = BottomTee
  pred CenterPtr = BoxSpiral
  pred Circle = CenterPtr
  pred Clock = Circle
  pred CoffeeMug = Clock
  pred Cross = CoffeeMug
  pred CrossReverse = Cross
  pred Crosshair = CrossReverse
  pred DiamondCross = Crosshair
  pred Dot = DiamondCross
  pred Dotbox = Dot
  pred DoubleArrow = Dotbox
  pred DraftLarge = DoubleArrow
  pred DraftSmall = DraftLarge
  pred DrapedBox = DraftSmall
  pred Exchange = DrapedBox
  pred Fleur = Exchange
  pred Gobbler = Fleur
  pred Gumby = Gobbler
  pred Hand1 = Gumby
  pred Hand2 = Hand1
  pred Heart = Hand2
  pred Icon = Heart
  pred IronCross = Icon
  pred LeftPtr = IronCross
  pred LeftSide = LeftPtr
  pred LeftTee = LeftSide
  pred Leftbutton = LeftTee
  pred LlAngle = Leftbutton
  pred LrAngle = LlAngle
  pred Man = LrAngle
  pred Middlebutton = Man
  pred Mouse = Middlebutton
  pred Pencil = Mouse
  pred Pirate = Pencil
  pred Plus = Pirate
  pred QuestionArrow = Plus
  pred RightPtr = QuestionArrow
  pred RightSide = RightPtr
  pred RightTee = RightSide
  pred Rightbutton = RightTee
  pred RtlLogo = Rightbutton
  pred Sailboat = RtlLogo
  pred SbDownArrow = Sailboat
  pred SbHDoubleArrow = SbDownArrow
  pred SbLeftArrow = SbHDoubleArrow
  pred SbRightArrow = SbLeftArrow
  pred SbUpArrow = SbRightArrow
  pred SbVDoubleArrow = SbUpArrow
  pred Shuttle = SbVDoubleArrow
  pred Sizing = Shuttle
  pred Spider = Sizing
  pred Spraycan = Spider
  pred Star = Spraycan
  pred Target = Star
  pred Tcross = Target
  pred TopLeftArrow = Tcross
  pred TopLeftCorner = TopLeftArrow
  pred TopRightCorner = TopLeftCorner
  pred TopSide = TopRightCorner
  pred TopTee = TopSide
  pred Trek = TopTee
  pred UlAngle = Trek
  pred Umbrella = UlAngle
  pred UrAngle = Umbrella
  pred Watch = UrAngle
  pred Xterm = Watch
  pred LastCursor = Xterm
  pred BlankCursor = LastCursor
  pred CursorIsPixmap = BlankCursor
  pred _ = undefined

  enumFromTo x y | fromEnum x == fromEnum y = [ y ]
                 | otherwise = x : enumFromTo (succ x) y
  enumFrom x = enumFromTo x CursorIsPixmap
  enumFromThen _ _ =     error "Enum CursorType: enumFromThen not implemented"
  enumFromThenTo _ _ _ =     error "Enum CursorType: enumFromThenTo not implemented"

{-# LINE 71 "./Graphics/UI/Gtk/Gdk/Cursor.chs" #-}

--------------------
-- Utils
makeNewCursor :: Ptr Cursor -> IO Cursor
makeNewCursor rPtr = do
  cursor <- newForeignPtr rPtr cursor_unref
  return (Cursor cursor)

foreign import ccall unsafe "&gdk_cursor_unref"
  cursor_unref :: FinalizerPtr Cursor

--------------------
-- Constructors
-- | Creates a new cursor from the set of builtin cursors for the default display.
-- See 'cursorNewForDisplay'.
-- To make the cursor invisible, use 'BlankCursor'.
cursorNew ::
    CursorType -- ^ @cursorType@ cursor to create
 -> IO Cursor -- ^ return a new 'Cursor'
cursorNew cursorType = do
  cursorPtr <- gdk_cursor_new $fromIntegral (fromEnum cursorType)
  makeNewCursor cursorPtr

--------------------
-- Methods

-- | Creates a new cursor from a given pixmap and mask. Both the pixmap and
-- mask must have a depth of 1 (i.e. each pixel has only 2 values - on or off).
-- The standard cursor size is 16 by 16 pixels.
--
-- Removed in Gtk3.
cursorNewFromPixmap ::
     Pixmap -- ^ @source@ - the pixmap specifying the cursor.
  -> Pixmap -- ^ @mask@ - the pixmap specifying the mask, which must be the
            -- same size as source.
  -> Color -- ^ @fg@ - the foreground color, used for the bits in the source
            -- which are 1. The color does not have to be allocated first.
  -> Color -- ^ @bg@ - the background color, used for the bits in the source
            -- which are 0. The color does not have to be allocated first.
  -> Int -- ^ @x@ - the horizontal offset of the \'hotspot\' of the cursor.
  -> Int -- ^ @y@ - the vertical offset of the \'hotspot\' of the cursor.
  -> IO Cursor
cursorNewFromPixmap source mask fg bg x y =
  with fg $ \fgPtr ->
    with bg $ \bgPtr -> do
      rPtr <- (\(Pixmap arg1) (Pixmap arg2) arg3 arg4 arg5 arg6 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gdk_cursor_new_from_pixmap argPtr1 argPtr2 arg3 arg4 arg5 arg6) source mask (castPtr fgPtr) (castPtr bgPtr) (fromIntegral x) (fromIntegral y)
      makeNewCursor rPtr


-- | Creates a new cursor from a pixbuf.
-- Not all GDK backends support RGBA cursors. If they are not supported, a monochrome approximation will be displayed.
-- The functions 'displaySupportsCursorAlpha' and 'displaySupportsCursorColor' can be used to determine whether RGBA cursors are supported;
-- 'displayGetDefaultCursorSize' and 'displayGetMaximalCursorSize' give information about cursor sizes.
--
-- On the X backend, support for RGBA cursors requires a sufficently new version of the X Render extension.
--
cursorNewFromPixbuf ::
    Display -- ^ @display@ the 'Display' for which the cursor will be created
 -> Pixbuf -- ^ @pixbuf@ the 'Pixbuf' containing the cursor image
 -> Int -- ^ @x@ the horizontal offset of the 'hotspot' of the cursor.
 -> Int -- ^ @y@ the vertical offset of the 'hotspot' of the cursor.
 -> IO Cursor -- ^ return a new 'Cursor'.
cursorNewFromPixbuf display pixbuf x y = do
  cursorPtr <- (\(Display arg1) (Pixbuf arg2) arg3 arg4 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gdk_cursor_new_from_pixbuf argPtr1 argPtr2 arg3 arg4) display pixbuf (fromIntegral x) (fromIntegral y)
  makeNewCursor cursorPtr

-- | Creates a new cursor by looking up name in the current cursor theme.
cursorNewFromName :: GlibString string
 => Display -- ^ @display@ the 'Display' for which the cursor will be created
 -> string -- ^ @name@ the name of the cursor
 -> IO (Maybe Cursor) -- ^ return a new 'Cursor', or @Nothing@ if there is no cursor with the given name
cursorNewFromName display name =
    withUTFString name $ \namePtr -> do
      cursorPtr <- (\(Display arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gdk_cursor_new_from_name argPtr1 arg2) display namePtr
      if cursorPtr == nullPtr then return Nothing else liftM Just $ makeNewCursor cursorPtr

-- | Creates a new cursor from the set of builtin cursors.
cursorNewForDisplay ::
    Display -- ^ @display@ the 'Display' for which the cursor will be created
 -> CursorType -- ^ @cursorType@ cursor to create
 -> IO Cursor -- ^ return a new 'Cursor'
cursorNewForDisplay display cursorType = do
  cursorPtr <- (\(Display arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gdk_cursor_new_for_display argPtr1 arg2) display $fromIntegral (fromEnum cursorType)
  makeNewCursor cursorPtr

-- | Returns the display on which the GdkCursor is defined.
cursorGetDisplay ::
    Cursor -- ^ @cursor@ 'Cursor'
 -> IO Display -- ^ return the 'Display' associated to cursor
cursorGetDisplay cursor =
    makeNewGObject mkDisplay $ (\(Cursor arg1) -> withForeignPtr arg1 $ \argPtr1 ->gdk_cursor_get_display argPtr1) cursor

-- | Returns a 'Pixbuf' with the image used to display the cursor.
-- Note that depending on the capabilities of the windowing system and on the cursor, GDK may not be able to obtain the image data.
-- In this case, @Nothing@ is returned.
cursorGetImage ::
    Cursor -- ^ @cursor@ 'Cursor'
 -> IO (Maybe Pixbuf) -- ^ a 'Pixbuf' representing cursor, or @Nothing@
cursorGetImage cursor =
    maybeNull (makeNewGObject mkPixbuf) $ (\(Cursor arg1) -> withForeignPtr arg1 $ \argPtr1 ->gdk_cursor_get_image argPtr1) cursor

foreign import ccall safe "gdk_cursor_new"
  gdk_cursor_new :: (CInt -> (IO (Ptr Cursor)))

foreign import ccall unsafe "gdk_cursor_new_from_pixmap"
  gdk_cursor_new_from_pixmap :: ((Ptr Pixmap) -> ((Ptr Pixmap) -> ((Ptr ()) -> ((Ptr ()) -> (CInt -> (CInt -> (IO (Ptr Cursor))))))))

foreign import ccall safe "gdk_cursor_new_from_pixbuf"
  gdk_cursor_new_from_pixbuf :: ((Ptr Display) -> ((Ptr Pixbuf) -> (CInt -> (CInt -> (IO (Ptr Cursor))))))

foreign import ccall safe "gdk_cursor_new_from_name"
  gdk_cursor_new_from_name :: ((Ptr Display) -> ((Ptr CChar) -> (IO (Ptr Cursor))))

foreign import ccall safe "gdk_cursor_new_for_display"
  gdk_cursor_new_for_display :: ((Ptr Display) -> (CInt -> (IO (Ptr Cursor))))

foreign import ccall safe "gdk_cursor_get_display"
  gdk_cursor_get_display :: ((Ptr Cursor) -> (IO (Ptr Display)))

foreign import ccall safe "gdk_cursor_get_image"
  gdk_cursor_get_image :: ((Ptr Cursor) -> (IO (Ptr Pixbuf)))
