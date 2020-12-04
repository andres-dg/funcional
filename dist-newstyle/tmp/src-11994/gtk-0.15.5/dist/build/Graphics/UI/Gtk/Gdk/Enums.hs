
{-# LINE 2 "./Graphics/UI/Gtk/Gdk/Enums.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Enumerations
--
-- Author : Manuel M. T. Chakravarty, Axel Simon
--
-- Created: 13 Januar 1999
--
-- Copyright (C) 1999-2005 Manuel M. T. Chakravarty, Axel Simon
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
-- General enumeration types.
--
module Graphics.UI.Gtk.Gdk.Enums (
  CrossingMode(..),
  DragProtocol(..),
  DragAction(..),
  EventMask(..),
  Modifier(..),



  NotifyType(..),
  ScrollDirection(..),
  VisibilityState(..),
  WindowState(..),
  WindowEdge(..),
  WindowTypeHint(..),
  Gravity(..),
  GrabStatus(..),

  OwnerChange(..),


  ExtensionMode(..),
  CapStyle(..),
  Dither(..),
  Fill(..),
  Function(..),
  InputCondition(..),
  JoinStyle(..),
  LineStyle(..),
  SubwindowMode(..),

  ) where

import System.Glib.Flags (Flags)


{-# LINE 64 "./Graphics/UI/Gtk/Gdk/Enums.chs" #-}


-- | Specify the how the ends of a line is drawn.
--
-- Removed in Gtk3.
data CapStyle = CapNotLast
              | CapButt
              | CapRound
              | CapProjecting
              deriving (Enum,Eq,Show)

{-# LINE 70 "./Graphics/UI/Gtk/Gdk/Enums.chs" #-}


-- | How focus is crossing the widget.
--
data CrossingMode = CrossingNormal
                  | CrossingGrab
                  | CrossingUngrab
                  | CrossingGtkGrab
                  | CrossingGtkUngrab
                  | CrossingStateChanged
                  deriving (Enum,Eq,Show)

{-# LINE 75 "./Graphics/UI/Gtk/Gdk/Enums.chs" #-}

-- | Used in 'Graphics.UI.Gtk.Gdk.Drag.DragContext' to indicate the protocol according to which DND is done.
--
data DragProtocol = DragProtoMotif
                  | DragProtoXdnd
                  | DragProtoRootwin
                  | DragProtoNone
                  | DragProtoWin32Dropfiles
                  | DragProtoOle2
                  | DragProtoLocal
                  deriving (Enum,Eq,Bounded,Show)

{-# LINE 79 "./Graphics/UI/Gtk/Gdk/Enums.chs" #-}


-- | Used in 'Graphics.UI.Gtk.Genearl.Drag.DragContext' to indicate what the
-- destination should do with the dropped data.
--
-- * 'ActionDefault': Initialisation value, should not be used.
--
-- * 'ActionCopy': Copy the data.
--
-- * 'ActionMove': Move the data, i.e. first copy it, then delete it from the source.
--
-- * 'ActionLink': Add a link to the data. Note that this is only useful if source and
-- destination agree on what it means.
--
-- * 'ActionPrivate': Special action which tells the source that the destination will do
-- something that the source doesn't understand.
--
-- * 'ActionAsk': Ask the user what to do with the data.
--
data DragAction = ActionDefault
                | ActionCopy
                | ActionMove
                | ActionLink
                | ActionPrivate
                | ActionAsk
                deriving (Eq,Bounded,Show)
instance Enum DragAction where
  fromEnum ActionDefault = 1
  fromEnum ActionCopy = 2
  fromEnum ActionMove = 4
  fromEnum ActionLink = 8
  fromEnum ActionPrivate = 16
  fromEnum ActionAsk = 32

  toEnum 1 = ActionDefault
  toEnum 2 = ActionCopy
  toEnum 4 = ActionMove
  toEnum 8 = ActionLink
  toEnum 16 = ActionPrivate
  toEnum 32 = ActionAsk
  toEnum unmatched = error ("DragAction.toEnum: Cannot match " ++ show unmatched)

  succ ActionDefault = ActionCopy
  succ ActionCopy = ActionMove
  succ ActionMove = ActionLink
  succ ActionLink = ActionPrivate
  succ ActionPrivate = ActionAsk
  succ _ = undefined

  pred ActionCopy = ActionDefault
  pred ActionMove = ActionCopy
  pred ActionLink = ActionMove
  pred ActionPrivate = ActionLink
  pred ActionAsk = ActionPrivate
  pred _ = undefined

  enumFromTo x y | fromEnum x == fromEnum y = [ y ]
                 | otherwise = x : enumFromTo (succ x) y
  enumFrom x = enumFromTo x ActionAsk
  enumFromThen _ _ =     error "Enum DragAction: enumFromThen not implemented"
  enumFromThenTo _ _ _ =     error "Enum DragAction: enumFromThenTo not implemented"

{-# LINE 99 "./Graphics/UI/Gtk/Gdk/Enums.chs" #-}

instance Flags DragAction


-- | Specify how to dither colors onto the screen.
--
-- Removed in Gtk3.
data Dither = RgbDitherNone
            | RgbDitherNormal
            | RgbDitherMax
            deriving (Enum,Eq,Show)

{-# LINE 107 "./Graphics/UI/Gtk/Gdk/Enums.chs" #-}


-- | Specify which events a widget will emit signals on.
--
data EventMask = ExposureMask
               | PointerMotionMask
               | PointerMotionHintMask
               | ButtonMotionMask
               | Button1MotionMask
               | Button2MotionMask
               | Button3MotionMask
               | ButtonPressMask
               | ButtonReleaseMask
               | KeyPressMask
               | KeyReleaseMask
               | EnterNotifyMask
               | LeaveNotifyMask
               | FocusChangeMask
               | StructureMask
               | PropertyChangeMask
               | VisibilityNotifyMask
               | ProximityInMask
               | ProximityOutMask
               | SubstructureMask
               | ScrollMask
               | AllEventsMask
               deriving (Eq,Bounded,Show)
instance Enum EventMask where
  fromEnum ExposureMask = 2
  fromEnum PointerMotionMask = 4
  fromEnum PointerMotionHintMask = 8
  fromEnum ButtonMotionMask = 16
  fromEnum Button1MotionMask = 32
  fromEnum Button2MotionMask = 64
  fromEnum Button3MotionMask = 128
  fromEnum ButtonPressMask = 256
  fromEnum ButtonReleaseMask = 512
  fromEnum KeyPressMask = 1024
  fromEnum KeyReleaseMask = 2048
  fromEnum EnterNotifyMask = 4096
  fromEnum LeaveNotifyMask = 8192
  fromEnum FocusChangeMask = 16384
  fromEnum StructureMask = 32768
  fromEnum PropertyChangeMask = 65536
  fromEnum VisibilityNotifyMask = 131072
  fromEnum ProximityInMask = 262144
  fromEnum ProximityOutMask = 524288
  fromEnum SubstructureMask = 1048576
  fromEnum ScrollMask = 2097152
  fromEnum AllEventsMask = 4194302

  toEnum 2 = ExposureMask
  toEnum 4 = PointerMotionMask
  toEnum 8 = PointerMotionHintMask
  toEnum 16 = ButtonMotionMask
  toEnum 32 = Button1MotionMask
  toEnum 64 = Button2MotionMask
  toEnum 128 = Button3MotionMask
  toEnum 256 = ButtonPressMask
  toEnum 512 = ButtonReleaseMask
  toEnum 1024 = KeyPressMask
  toEnum 2048 = KeyReleaseMask
  toEnum 4096 = EnterNotifyMask
  toEnum 8192 = LeaveNotifyMask
  toEnum 16384 = FocusChangeMask
  toEnum 32768 = StructureMask
  toEnum 65536 = PropertyChangeMask
  toEnum 131072 = VisibilityNotifyMask
  toEnum 262144 = ProximityInMask
  toEnum 524288 = ProximityOutMask
  toEnum 1048576 = SubstructureMask
  toEnum 2097152 = ScrollMask
  toEnum 4194302 = AllEventsMask
  toEnum unmatched = error ("EventMask.toEnum: Cannot match " ++ show unmatched)

  succ ExposureMask = PointerMotionMask
  succ PointerMotionMask = PointerMotionHintMask
  succ PointerMotionHintMask = ButtonMotionMask
  succ ButtonMotionMask = Button1MotionMask
  succ Button1MotionMask = Button2MotionMask
  succ Button2MotionMask = Button3MotionMask
  succ Button3MotionMask = ButtonPressMask
  succ ButtonPressMask = ButtonReleaseMask
  succ ButtonReleaseMask = KeyPressMask
  succ KeyPressMask = KeyReleaseMask
  succ KeyReleaseMask = EnterNotifyMask
  succ EnterNotifyMask = LeaveNotifyMask
  succ LeaveNotifyMask = FocusChangeMask
  succ FocusChangeMask = StructureMask
  succ StructureMask = PropertyChangeMask
  succ PropertyChangeMask = VisibilityNotifyMask
  succ VisibilityNotifyMask = ProximityInMask
  succ ProximityInMask = ProximityOutMask
  succ ProximityOutMask = SubstructureMask
  succ SubstructureMask = ScrollMask
  succ ScrollMask = AllEventsMask
  succ _ = undefined

  pred PointerMotionMask = ExposureMask
  pred PointerMotionHintMask = PointerMotionMask
  pred ButtonMotionMask = PointerMotionHintMask
  pred Button1MotionMask = ButtonMotionMask
  pred Button2MotionMask = Button1MotionMask
  pred Button3MotionMask = Button2MotionMask
  pred ButtonPressMask = Button3MotionMask
  pred ButtonReleaseMask = ButtonPressMask
  pred KeyPressMask = ButtonReleaseMask
  pred KeyReleaseMask = KeyPressMask
  pred EnterNotifyMask = KeyReleaseMask
  pred LeaveNotifyMask = EnterNotifyMask
  pred FocusChangeMask = LeaveNotifyMask
  pred StructureMask = FocusChangeMask
  pred PropertyChangeMask = StructureMask
  pred VisibilityNotifyMask = PropertyChangeMask
  pred ProximityInMask = VisibilityNotifyMask
  pred ProximityOutMask = ProximityInMask
  pred SubstructureMask = ProximityOutMask
  pred ScrollMask = SubstructureMask
  pred AllEventsMask = ScrollMask
  pred _ = undefined

  enumFromTo x y | fromEnum x == fromEnum y = [ y ]
                 | otherwise = x : enumFromTo (succ x) y
  enumFrom x = enumFromTo x AllEventsMask
  enumFromThen _ _ =     error "Enum EventMask: enumFromThen not implemented"
  enumFromThenTo _ _ _ =     error "Enum EventMask: enumFromThenTo not implemented"

{-# LINE 112 "./Graphics/UI/Gtk/Gdk/Enums.chs" #-}

instance Flags EventMask

-- | Keyboard modifiers that are depressed when the user presses
-- a key or a mouse button.
--
-- * This data type is used to build lists of modifers that were active
-- during an event.
--
-- * The "Apple" key on Macintoshs is mapped to 'Alt2' and the 'Meta'
-- key (if available).
--
-- * Since Gtk 2.10, there are also 'Super', 'Hyper' and 'Meta' modifiers
-- which are simply generated from 'Alt' .. 'Compose' modifier keys,
-- depending on the mapping used by the windowing system. Due to one
-- key being mapped to e.g. 'Alt2' and 'Meta', you shouldn't pattern
-- match directly against a certain key but check whether a key is
-- in the list using the 'elem' function, say.
--

data Modifier = Shift
              | Lock
              | Control
              | Alt
              | Alt2
              | Alt3
              | Alt4
              | Alt5
              | Button1
              | Button2
              | Button3
              | Button4
              | Button5
              | Super
              | Hyper
              | Meta
              | Release
              | ModifierMask
              deriving (Bounded,Show,Eq)
instance Enum Modifier where
  fromEnum Shift = 1
  fromEnum Lock = 2
  fromEnum Control = 4
  fromEnum Alt = 8
  fromEnum Alt2 = 16
  fromEnum Alt3 = 32
  fromEnum Alt4 = 64
  fromEnum Alt5 = 128
  fromEnum Button1 = 256
  fromEnum Button2 = 512
  fromEnum Button3 = 1024
  fromEnum Button4 = 2048
  fromEnum Button5 = 4096
  fromEnum Super = 67108864
  fromEnum Hyper = 134217728
  fromEnum Meta = 268435456
  fromEnum Release = 1073741824
  fromEnum ModifierMask = 1543512063

  toEnum 1 = Shift
  toEnum 2 = Lock
  toEnum 4 = Control
  toEnum 8 = Alt
  toEnum 16 = Alt2
  toEnum 32 = Alt3
  toEnum 64 = Alt4
  toEnum 128 = Alt5
  toEnum 256 = Button1
  toEnum 512 = Button2
  toEnum 1024 = Button3
  toEnum 2048 = Button4
  toEnum 4096 = Button5
  toEnum 67108864 = Super
  toEnum 134217728 = Hyper
  toEnum 268435456 = Meta
  toEnum 1073741824 = Release
  toEnum 1543512063 = ModifierMask
  toEnum unmatched = error ("Modifier.toEnum: Cannot match " ++ show unmatched)

  succ Shift = Lock
  succ Lock = Control
  succ Control = Alt
  succ Alt = Alt2
  succ Alt2 = Alt3
  succ Alt3 = Alt4
  succ Alt4 = Alt5
  succ Alt5 = Button1
  succ Button1 = Button2
  succ Button2 = Button3
  succ Button3 = Button4
  succ Button4 = Button5
  succ Button5 = Super
  succ Super = Hyper
  succ Hyper = Meta
  succ Meta = Release
  succ Release = ModifierMask
  succ _ = undefined

  pred Lock = Shift
  pred Control = Lock
  pred Alt = Control
  pred Alt2 = Alt
  pred Alt3 = Alt2
  pred Alt4 = Alt3
  pred Alt5 = Alt4
  pred Button1 = Alt5
  pred Button2 = Button1
  pred Button3 = Button2
  pred Button4 = Button3
  pred Button5 = Button4
  pred Super = Button5
  pred Hyper = Super
  pred Meta = Hyper
  pred Release = Meta
  pred ModifierMask = Release
  pred _ = undefined

  enumFromTo x y | fromEnum x == fromEnum y = [ y ]
                 | otherwise = x : enumFromTo (succ x) y
  enumFrom x = enumFromTo x ModifierMask
  enumFromThen _ _ =     error "Enum Modifier: enumFromThen not implemented"
  enumFromThenTo _ _ _ =     error "Enum Modifier: enumFromThenTo not implemented"

{-# LINE 170 "./Graphics/UI/Gtk/Gdk/Enums.chs" #-}
instance Flags Modifier






-- | specify which input extension a widget desires
--
data ExtensionMode = ExtensionEventsNone
                   | ExtensionEventsAll
                   | ExtensionEventsCursor
                   deriving (Enum,Eq,Bounded,Show)

{-# LINE 180 "./Graphics/UI/Gtk/Gdk/Enums.chs" #-}

instance Flags ExtensionMode

-- | How objects are filled.
--
-- Removed in Gtk3.
data Fill = Solid
          | Tiled
          | Stippled
          | OpaqueStippled
          deriving (Enum,Eq,Show)

{-# LINE 187 "./Graphics/UI/Gtk/Gdk/Enums.chs" #-}

-- | Determine how bitmap operations are carried out.
--
-- Removed in Gtk3.
data Function = Copy
              | Invert
              | Xor
              | Clear
              | And
              | AndReverse
              | AndInvert
              | Noop
              | Or
              | Equiv
              | OrReverse
              | CopyInvert
              | OrInvert
              | Nand
              | Nor
              | Set
              deriving (Enum,Eq,Show)

{-# LINE 192 "./Graphics/UI/Gtk/Gdk/Enums.chs" #-}

-- | Specify on what file condition a callback should be
-- done.
--
-- Removed in Gtk3.
data InputCondition = InputRead
                    | InputWrite
                    | InputException
                    deriving (Eq,Bounded)
instance Enum InputCondition where
  fromEnum InputRead = 1
  fromEnum InputWrite = 2
  fromEnum InputException = 4

  toEnum 1 = InputRead
  toEnum 2 = InputWrite
  toEnum 4 = InputException
  toEnum unmatched = error ("InputCondition.toEnum: Cannot match " ++ show unmatched)

  succ InputRead = InputWrite
  succ InputWrite = InputException
  succ _ = undefined

  pred InputWrite = InputRead
  pred InputException = InputWrite
  pred _ = undefined

  enumFromTo x y | fromEnum x == fromEnum y = [ y ]
                 | otherwise = x : enumFromTo (succ x) y
  enumFrom x = enumFromTo x InputException
  enumFromThen _ _ =     error "Enum InputCondition: enumFromThen not implemented"
  enumFromThenTo _ _ _ =     error "Enum InputCondition: enumFromThenTo not implemented"

{-# LINE 198 "./Graphics/UI/Gtk/Gdk/Enums.chs" #-}

instance Flags InputCondition

-- | Determines how adjacent line ends are drawn.
--
-- Removed in Gtk3.
data JoinStyle = JoinMiter
               | JoinRound
               | JoinBevel
               deriving (Enum,Eq,Show)

{-# LINE 205 "./Graphics/UI/Gtk/Gdk/Enums.chs" #-}

-- | Determines if a line is solid or dashed.
--
-- Removed in Gtk3.
data LineStyle = LineSolid
               | LineOnOffDash
               | LineDoubleDash
               deriving (Enum,Eq,Show)

{-# LINE 210 "./Graphics/UI/Gtk/Gdk/Enums.chs" #-}

-- | Information on from what level of the widget hierarchy the mouse
-- cursor came.
--
-- ['NotifyAncestor'] The window is entered from an ancestor or left towards
-- an ancestor.
--
-- ['NotifyVirtual'] The pointer moves between an ancestor and an inferior
-- of the window.
--
-- ['NotifyInferior'] The window is entered from an inferior or left
-- towards an inferior.
--
-- ['NotifyNonlinear'] The window is entered from or left towards a
-- window which is neither an ancestor nor an inferior.
--
-- ['NotifyNonlinearVirtual'] The pointer moves between two windows which
-- are not ancestors of each other and the window is part of the ancestor
-- chain between one of these windows and their least common ancestor.
--
-- ['NotifyUnknown'] The level change does not fit into any of the other
-- categories or could not be determined.
--
data NotifyType = NotifyAncestor
                | NotifyVirtual
                | NotifyInferior
                | NotifyNonlinear
                | NotifyNonlinearVirtual
                | NotifyUnknown
                deriving (Eq,Show)
instance Enum NotifyType where
  fromEnum NotifyAncestor = 0
  fromEnum NotifyVirtual = 1
  fromEnum NotifyInferior = 2
  fromEnum NotifyNonlinear = 3
  fromEnum NotifyNonlinearVirtual = 4
  fromEnum NotifyUnknown = 5

  toEnum 0 = NotifyAncestor
  toEnum 1 = NotifyVirtual
  toEnum 2 = NotifyInferior
  toEnum 3 = NotifyNonlinear
  toEnum 4 = NotifyNonlinearVirtual
  toEnum 5 = NotifyUnknown
  toEnum unmatched = error ("NotifyType.toEnum: Cannot match " ++ show unmatched)

  succ NotifyAncestor = NotifyVirtual
  succ NotifyVirtual = NotifyInferior
  succ NotifyInferior = NotifyNonlinear
  succ NotifyNonlinear = NotifyNonlinearVirtual
  succ NotifyNonlinearVirtual = NotifyUnknown
  succ _ = undefined

  pred NotifyVirtual = NotifyAncestor
  pred NotifyInferior = NotifyVirtual
  pred NotifyNonlinear = NotifyInferior
  pred NotifyNonlinearVirtual = NotifyNonlinear
  pred NotifyUnknown = NotifyNonlinearVirtual
  pred _ = undefined

  enumFromTo x y | fromEnum x == fromEnum y = [ y ]
                 | otherwise = x : enumFromTo (succ x) y
  enumFrom x = enumFromTo x NotifyUnknown
  enumFromThen _ _ =     error "Enum NotifyType: enumFromThen not implemented"
  enumFromThenTo _ _ _ =     error "Enum NotifyType: enumFromThenTo not implemented"

{-# LINE 234 "./Graphics/UI/Gtk/Gdk/Enums.chs" #-}

-- | in which direction was scrolled?
--
data ScrollDirection = ScrollUp
                     | ScrollDown
                     | ScrollLeft
                     | ScrollRight
                     deriving (Enum,Eq,Show)

{-# LINE 238 "./Graphics/UI/Gtk/Gdk/Enums.chs" #-}



-- | Determine if child widget may be overdrawn.
--
-- Removed in Gtk3.
data SubwindowMode = ClipByChildren
                   | IncludeInferiors
                   deriving (Eq,Show)
instance Enum SubwindowMode where
  fromEnum ClipByChildren = 0
  fromEnum IncludeInferiors = 1

  toEnum 0 = ClipByChildren
  toEnum 1 = IncludeInferiors
  toEnum unmatched = error ("SubwindowMode.toEnum: Cannot match " ++ show unmatched)

  succ ClipByChildren = IncludeInferiors
  succ _ = undefined

  pred IncludeInferiors = ClipByChildren
  pred _ = undefined

  enumFromTo x y | fromEnum x == fromEnum y = [ y ]
                 | otherwise = x : enumFromTo (succ x) y
  enumFrom x = enumFromTo x IncludeInferiors
  enumFromThen _ _ =     error "Enum SubwindowMode: enumFromThen not implemented"
  enumFromThenTo _ _ _ =     error "Enum SubwindowMode: enumFromThenTo not implemented"

{-# LINE 245 "./Graphics/UI/Gtk/Gdk/Enums.chs" #-}


-- | visibility of a window
--
data VisibilityState = VisibilityUnobscured
                     | VisibilityPartialObscured
                     | VisibilityFullyObscured
                     deriving (Enum,Eq,Show)

{-# LINE 252 "./Graphics/UI/Gtk/Gdk/Enums.chs" #-}

-- | The state a @DrawWindow@ is in.
--
data WindowState = WindowStateWithdrawn
                 | WindowStateIconified
                 | WindowStateMaximized
                 | WindowStateSticky
                 | WindowStateFullscreen
                 | WindowStateAbove
                 | WindowStateBelow
                 deriving (Eq,Bounded,Show)
instance Enum WindowState where
  fromEnum WindowStateWithdrawn = 1
  fromEnum WindowStateIconified = 2
  fromEnum WindowStateMaximized = 4
  fromEnum WindowStateSticky = 8
  fromEnum WindowStateFullscreen = 16
  fromEnum WindowStateAbove = 32
  fromEnum WindowStateBelow = 64

  toEnum 1 = WindowStateWithdrawn
  toEnum 2 = WindowStateIconified
  toEnum 4 = WindowStateMaximized
  toEnum 8 = WindowStateSticky
  toEnum 16 = WindowStateFullscreen
  toEnum 32 = WindowStateAbove
  toEnum 64 = WindowStateBelow
  toEnum unmatched = error ("WindowState.toEnum: Cannot match " ++ show unmatched)

  succ WindowStateWithdrawn = WindowStateIconified
  succ WindowStateIconified = WindowStateMaximized
  succ WindowStateMaximized = WindowStateSticky
  succ WindowStateSticky = WindowStateFullscreen
  succ WindowStateFullscreen = WindowStateAbove
  succ WindowStateAbove = WindowStateBelow
  succ _ = undefined

  pred WindowStateIconified = WindowStateWithdrawn
  pred WindowStateMaximized = WindowStateIconified
  pred WindowStateSticky = WindowStateMaximized
  pred WindowStateFullscreen = WindowStateSticky
  pred WindowStateAbove = WindowStateFullscreen
  pred WindowStateBelow = WindowStateAbove
  pred _ = undefined

  enumFromTo x y | fromEnum x == fromEnum y = [ y ]
                 | otherwise = x : enumFromTo (succ x) y
  enumFrom x = enumFromTo x WindowStateBelow
  enumFromThen _ _ =     error "Enum WindowState: enumFromThen not implemented"
  enumFromThenTo _ _ _ =     error "Enum WindowState: enumFromThenTo not implemented"

{-# LINE 256 "./Graphics/UI/Gtk/Gdk/Enums.chs" #-}

instance Flags WindowState

-- | Determines a window edge or corner.
--
data WindowEdge = WindowEdgeNorthWest
                | WindowEdgeNorth
                | WindowEdgeNorthEast
                | WindowEdgeWest
                | WindowEdgeEast
                | WindowEdgeSouthWest
                | WindowEdgeSouth
                | WindowEdgeSouthEast
                deriving (Enum,Eq,Show)

{-# LINE 262 "./Graphics/UI/Gtk/Gdk/Enums.chs" #-}

-- | These are hints for the window manager that indicate what type of function
-- the window has. The window manager can use this when determining decoration
-- and behaviour of the window. The hint must be set before mapping the window.
--
-- See the extended window manager hints specification for more details about
-- window types.
--
data WindowTypeHint = WindowTypeHintNormal
                    | WindowTypeHintDialog
                    | WindowTypeHintMenu
                    | WindowTypeHintToolbar
                    | WindowTypeHintSplashscreen
                    | WindowTypeHintUtility
                    | WindowTypeHintDock
                    | WindowTypeHintDesktop
                    | WindowTypeHintDropdownMenu
                    | WindowTypeHintPopupMenu
                    | WindowTypeHintTooltip
                    | WindowTypeHintNotification
                    | WindowTypeHintCombo
                    | WindowTypeHintDnd
                    deriving (Enum,Eq,Show)

{-# LINE 271 "./Graphics/UI/Gtk/Gdk/Enums.chs" #-}

-- | Defines the reference point of a window and the meaning of coordinates
-- passed to 'Graphics.UI.Gtk.Windows.Window.windowMove'. See
-- 'Graphics.UI.Gtk.Windows.Window.windowMove' and the "implementation notes"
-- section of the extended window manager hints specification for more details.
--
data Gravity = GravityNorthWest
             | GravityNorth
             | GravityNorthEast
             | GravityWest
             | GravityCenter
             | GravityEast
             | GravitySouthWest
             | GravitySouth
             | GravitySouthEast
             | GravityStatic
             deriving (Eq,Show)
instance Enum Gravity where
  fromEnum GravityNorthWest = 1
  fromEnum GravityNorth = 2
  fromEnum GravityNorthEast = 3
  fromEnum GravityWest = 4
  fromEnum GravityCenter = 5
  fromEnum GravityEast = 6
  fromEnum GravitySouthWest = 7
  fromEnum GravitySouth = 8
  fromEnum GravitySouthEast = 9
  fromEnum GravityStatic = 10

  toEnum 1 = GravityNorthWest
  toEnum 2 = GravityNorth
  toEnum 3 = GravityNorthEast
  toEnum 4 = GravityWest
  toEnum 5 = GravityCenter
  toEnum 6 = GravityEast
  toEnum 7 = GravitySouthWest
  toEnum 8 = GravitySouth
  toEnum 9 = GravitySouthEast
  toEnum 10 = GravityStatic
  toEnum unmatched = error ("Gravity.toEnum: Cannot match " ++ show unmatched)

  succ GravityNorthWest = GravityNorth
  succ GravityNorth = GravityNorthEast
  succ GravityNorthEast = GravityWest
  succ GravityWest = GravityCenter
  succ GravityCenter = GravityEast
  succ GravityEast = GravitySouthWest
  succ GravitySouthWest = GravitySouth
  succ GravitySouth = GravitySouthEast
  succ GravitySouthEast = GravityStatic
  succ _ = undefined

  pred GravityNorth = GravityNorthWest
  pred GravityNorthEast = GravityNorth
  pred GravityWest = GravityNorthEast
  pred GravityCenter = GravityWest
  pred GravityEast = GravityCenter
  pred GravitySouthWest = GravityEast
  pred GravitySouth = GravitySouthWest
  pred GravitySouthEast = GravitySouth
  pred GravityStatic = GravitySouthEast
  pred _ = undefined

  enumFromTo x y | fromEnum x == fromEnum y = [ y ]
                 | otherwise = x : enumFromTo (succ x) y
  enumFrom x = enumFromTo x GravityStatic
  enumFromThen _ _ =     error "Enum Gravity: enumFromThen not implemented"
  enumFromThenTo _ _ _ =     error "Enum Gravity: enumFromThenTo not implemented"

{-# LINE 278 "./Graphics/UI/Gtk/Gdk/Enums.chs" #-}

-- | Returned by 'pointerGrab' and 'keyboardGrab' to indicate success or the
-- reason for the failure of the grab attempt.
--
-- [@GrabSuccess@] the resource was successfully grabbed.
--
-- [@GrabAlreadyGrabbed@] the resource is actively grabbed by another client.
--
-- [@GrabInvalidTime@] the resource was grabbed more recently than the
-- specified time.
--
-- [@GrabNotViewable@] the grab window or the confine_to window are not
-- viewable.
--
-- [@GrabFrozen@] the resource is frozen by an active grab of another client.
--
data GrabStatus = GrabSuccess
                | GrabAlreadyGrabbed
                | GrabInvalidTime
                | GrabNotViewable
                | GrabFrozen
                deriving (Eq,Show)
instance Enum GrabStatus where
  fromEnum GrabSuccess = 0
  fromEnum GrabAlreadyGrabbed = 1
  fromEnum GrabInvalidTime = 2
  fromEnum GrabNotViewable = 3
  fromEnum GrabFrozen = 4

  toEnum 0 = GrabSuccess
  toEnum 1 = GrabAlreadyGrabbed
  toEnum 2 = GrabInvalidTime
  toEnum 3 = GrabNotViewable
  toEnum 4 = GrabFrozen
  toEnum unmatched = error ("GrabStatus.toEnum: Cannot match " ++ show unmatched)

  succ GrabSuccess = GrabAlreadyGrabbed
  succ GrabAlreadyGrabbed = GrabInvalidTime
  succ GrabInvalidTime = GrabNotViewable
  succ GrabNotViewable = GrabFrozen
  succ _ = undefined

  pred GrabAlreadyGrabbed = GrabSuccess
  pred GrabInvalidTime = GrabAlreadyGrabbed
  pred GrabNotViewable = GrabInvalidTime
  pred GrabFrozen = GrabNotViewable
  pred _ = undefined

  enumFromTo x y | fromEnum x == fromEnum y = [ y ]
                 | otherwise = x : enumFromTo (succ x) y
  enumFrom x = enumFromTo x GrabFrozen
  enumFromThen _ _ =     error "Enum GrabStatus: enumFromThen not implemented"
  enumFromThenTo _ _ _ =     error "Enum GrabStatus: enumFromThenTo not implemented"

{-# LINE 295 "./Graphics/UI/Gtk/Gdk/Enums.chs" #-}


-- | Specifies why a selection ownership was changed.
--
-- [@OwnerChangeNewOwner@] some other application claimed the ownership
--
-- [@OwnerChangeDestroy@] the window was destroyed
--
-- [@OwnerChangeClose@] the client was closed
--
data OwnerChange = OwnerChangeNewOwner
                 | OwnerChangeDestroy
                 | OwnerChangeClose
                 deriving (Enum,Eq,Show)

{-# LINE 306 "./Graphics/UI/Gtk/Gdk/Enums.chs" #-}
