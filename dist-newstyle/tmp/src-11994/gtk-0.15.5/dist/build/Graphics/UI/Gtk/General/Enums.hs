
{-# LINE 2 "./Graphics/UI/Gtk/General/Enums.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Enumerations
--
-- Author : Axel Simon, Manuel Chakravarty
--
-- Created: 13 January 1999
--
-- Copyright (C) 1999..2005 Axel Simon, Manuel Chakravarty
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
module Graphics.UI.Gtk.General.Enums (
  AccelFlags(..),



  ArrowType(..),
  AttachOptions(..),



  MouseButton(..),
  ButtonBoxStyle(..),
  CalendarDisplayOptions(..),
  Click(..),
  CornerType(..),
  DeleteType(..),
  DestDefaults(..),

  DragResult(..),

  DirectionType(..),
  Justification(..),





  MatchType(..),


  MenuDirectionType(..),


  MetricType(..),


  MovementStep(..),
  Orientation(..),
  Packing(..), toPacking, fromPacking,
  PackType(..),
  PathPriorityType(..),
  PathType(..),
  PolicyType(..),
  PositionType(..),

  ProgressBarOrientation(..),

  ReliefStyle(..),
  ResizeMode(..),
  ScrollType(..),
  ScrollStep (..),
  SelectionMode(..),
  ShadowType(..),



  SortType(..),
  StateType(..),


  SubmenuDirection(..),
  SubmenuPlacement(..),


  SpinButtonUpdatePolicy(..),
  SpinType(..),
  TargetFlags(..),
  TextDirection(..),
  TextSearchFlags(..),
  TextWindowType(..),
  ToolbarStyle(..),
  TreeViewColumnSizing(..),
  --TroughType(..),

  UpdateType(..),
  Visibility(..),

  WindowPosition(..),
  WindowType(..),
  WrapMode(..),

  EntryIconPosition(..),


  AnchorType (..),





module Graphics.UI.Gtk.Gdk.Enums
  ) where

import System.Glib.Flags (Flags)
import Graphics.UI.Gtk.Gdk.Enums


{-# LINE 125 "./Graphics/UI/Gtk/General/Enums.chs" #-}


-- | State of an accelerator
--
data AccelFlags = AccelVisible
                | AccelLocked
                | AccelMask
                deriving (Bounded,Eq,Show)
instance Enum AccelFlags where
  fromEnum AccelVisible = 1
  fromEnum AccelLocked = 2
  fromEnum AccelMask = 7

  toEnum 1 = AccelVisible
  toEnum 2 = AccelLocked
  toEnum 7 = AccelMask
  toEnum unmatched = error ("AccelFlags.toEnum: Cannot match " ++ show unmatched)

  succ AccelVisible = AccelLocked
  succ AccelLocked = AccelMask
  succ _ = undefined

  pred AccelLocked = AccelVisible
  pred AccelMask = AccelLocked
  pred _ = undefined

  enumFromTo x y | fromEnum x == fromEnum y = [ y ]
                 | otherwise = x : enumFromTo (succ x) y
  enumFrom x = enumFromTo x AccelMask
  enumFromThen _ _ =     error "Enum AccelFlags: enumFromThen not implemented"
  enumFromThenTo _ _ _ =     error "Enum AccelFlags: enumFromThenTo not implemented"

{-# LINE 130 "./Graphics/UI/Gtk/General/Enums.chs" #-}

instance Flags AccelFlags







-- | Arrow directions for the arrow widget
--
data ArrowType = ArrowUp
               | ArrowDown
               | ArrowLeft
               | ArrowRight
               | ArrowNone
               deriving (Enum,Eq,Show)

{-# LINE 142 "./Graphics/UI/Gtk/General/Enums.chs" #-}

-- | Child widget attach options for table containers
--
data AttachOptions = Expand
                   | Shrink
                   | Fill
                   deriving (Bounded,Eq,Show)
instance Enum AttachOptions where
  fromEnum Expand = 1
  fromEnum Shrink = 2
  fromEnum Fill = 4

  toEnum 1 = Expand
  toEnum 2 = Shrink
  toEnum 4 = Fill
  toEnum unmatched = error ("AttachOptions.toEnum: Cannot match " ++ show unmatched)

  succ Expand = Shrink
  succ Shrink = Fill
  succ _ = undefined

  pred Shrink = Expand
  pred Fill = Shrink
  pred _ = undefined

  enumFromTo x y | fromEnum x == fromEnum y = [ y ]
                 | otherwise = x : enumFromTo (succ x) y
  enumFrom x = enumFromTo x Fill
  enumFromThen _ _ =     error "Enum AttachOptions: enumFromThen not implemented"
  enumFromThenTo _ _ _ =     error "Enum AttachOptions: enumFromThenTo not implemented"

{-# LINE 146 "./Graphics/UI/Gtk/General/Enums.chs" #-}

instance Flags AttachOptions
{-# LINE 159 "./Graphics/UI/Gtk/General/Enums.chs" #-}
-- | Mouse buttons.
--
data MouseButton = LeftButton
                 | MiddleButton
                 | RightButton
                 | OtherButton Int
                   deriving (Eq,Show)

instance Enum MouseButton where
  toEnum 1 = LeftButton
  toEnum 2 = MiddleButton
  toEnum 3 = RightButton
  toEnum n = OtherButton (fromIntegral n)
  fromEnum LeftButton = 1
  fromEnum MiddleButton = 2
  fromEnum RightButton = 3
  fromEnum (OtherButton n) = fromIntegral n

-- | Dictate the style that a ButtonBox uses to align it contents
--
data ButtonBoxStyle = ButtonboxDefaultStyle
                    | ButtonboxSpread
                    | ButtonboxEdge
                    | ButtonboxStart
                    | ButtonboxEnd
                    | ButtonboxCenter
                    deriving (Enum,Eq,Show)

{-# LINE 180 "./Graphics/UI/Gtk/General/Enums.chs" #-}

-- | Specify which items of a calendar should be displayed.
--
data CalendarDisplayOptions = CalendarShowHeading
                            | CalendarShowDayNames
                            | CalendarNoMonthChange
                            | CalendarShowWeekNumbers
                            | CalendarWeekStartMonday
                            | CalendarShowDetails
                            deriving (Bounded,Eq,Show)
instance Enum CalendarDisplayOptions where
  fromEnum CalendarShowHeading = 1
  fromEnum CalendarShowDayNames = 2
  fromEnum CalendarNoMonthChange = 4
  fromEnum CalendarShowWeekNumbers = 8
  fromEnum CalendarWeekStartMonday = 16
  fromEnum CalendarShowDetails = 32

  toEnum 1 = CalendarShowHeading
  toEnum 2 = CalendarShowDayNames
  toEnum 4 = CalendarNoMonthChange
  toEnum 8 = CalendarShowWeekNumbers
  toEnum 16 = CalendarWeekStartMonday
  toEnum 32 = CalendarShowDetails
  toEnum unmatched = error ("CalendarDisplayOptions.toEnum: Cannot match " ++ show unmatched)

  succ CalendarShowHeading = CalendarShowDayNames
  succ CalendarShowDayNames = CalendarNoMonthChange
  succ CalendarNoMonthChange = CalendarShowWeekNumbers
  succ CalendarShowWeekNumbers = CalendarWeekStartMonday
  succ CalendarWeekStartMonday = CalendarShowDetails
  succ _ = undefined

  pred CalendarShowDayNames = CalendarShowHeading
  pred CalendarNoMonthChange = CalendarShowDayNames
  pred CalendarShowWeekNumbers = CalendarNoMonthChange
  pred CalendarWeekStartMonday = CalendarShowWeekNumbers
  pred CalendarShowDetails = CalendarWeekStartMonday
  pred _ = undefined

  enumFromTo x y | fromEnum x == fromEnum y = [ y ]
                 | otherwise = x : enumFromTo (succ x) y
  enumFrom x = enumFromTo x CalendarShowDetails
  enumFromThen _ _ =     error "Enum CalendarDisplayOptions: enumFromThen not implemented"
  enumFromThenTo _ _ _ =     error "Enum CalendarDisplayOptions: enumFromThenTo not implemented"

{-# LINE 184 "./Graphics/UI/Gtk/General/Enums.chs" #-}

instance Flags CalendarDisplayOptions

-- | Type of mouse click
--
data Click = SingleClick
           | DoubleClick
           | TripleClick
           | ReleaseClick
  deriving (Eq,Show,Enum)

-- | Specifies in which corner a child widget should be placed
--
data CornerType = CornerTopLeft
                | CornerBottomLeft
                | CornerTopRight
                | CornerBottomRight
                deriving (Enum,Eq,Show)

{-# LINE 198 "./Graphics/UI/Gtk/General/Enums.chs" #-}

-- | Editing option
--
data DeleteType = DeleteChars
                | DeleteWordEnds
                | DeleteWords
                | DeleteDisplayLines
                | DeleteDisplayLineEnds
                | DeleteParagraphEnds
                | DeleteParagraphs
                | DeleteWhitespace
                deriving (Enum,Eq,Show)

{-# LINE 202 "./Graphics/UI/Gtk/General/Enums.chs" #-}

-- | The 'DestDefaults' enumeration specifies the various types of action that
-- will be taken on behalf of the user for a drag destination site.
--
-- * 'DestDefaultMotion': If set for a widget, GTK+, during a drag over this
-- widget will check if the drag matches this widget's list of possible
-- targets and actions. GTK+ will then call
-- 'Graphics.UI.Gtk.Gdk.Drag.dragStatus' as appropriate.
--
-- * 'DestDefaultHighlight': If set for a widget, GTK+ will draw a
-- highlight on this widget as long as a drag is over this widget and the
-- widget drag format and action are acceptable.
--
-- * 'DestDefaultDrop': If set for a widget, when a drop occurs, GTK+ will
-- will check if the drag matches this widget's list of possible targets and
-- actions. If so, GTK+ will call 'Graphics.UI.Gtk.Gdk.Drag.dragGetData' on
-- behalf of the widget. Whether or not the drop is successful, GTK+ will
-- call 'Graphics.UI.Gtk.Gdk.Drag.dragFinish'. If the action was a move,
-- then if the drag was successful, then @True@ will be passed for the
-- delete parameter to 'Graphics.UI.Gtk.Gdk.Drag.dragFinish'
--
-- * 'DestDefaultAll': If set, specifies that all default actions should be
-- taken.
--
data DestDefaults = DestDefaultMotion
                  | DestDefaultHighlight
                  | DestDefaultDrop
                  | DestDefaultAll
                  deriving (Bounded,Eq,Show)
instance Enum DestDefaults where
  fromEnum DestDefaultMotion = 1
  fromEnum DestDefaultHighlight = 2
  fromEnum DestDefaultDrop = 4
  fromEnum DestDefaultAll = 7

  toEnum 1 = DestDefaultMotion
  toEnum 2 = DestDefaultHighlight
  toEnum 4 = DestDefaultDrop
  toEnum 7 = DestDefaultAll
  toEnum unmatched = error ("DestDefaults.toEnum: Cannot match " ++ show unmatched)

  succ DestDefaultMotion = DestDefaultHighlight
  succ DestDefaultHighlight = DestDefaultDrop
  succ DestDefaultDrop = DestDefaultAll
  succ _ = undefined

  pred DestDefaultHighlight = DestDefaultMotion
  pred DestDefaultDrop = DestDefaultHighlight
  pred DestDefaultAll = DestDefaultDrop
  pred _ = undefined

  enumFromTo x y | fromEnum x == fromEnum y = [ y ]
                 | otherwise = x : enumFromTo (succ x) y
  enumFrom x = enumFromTo x DestDefaultAll
  enumFromThen _ _ =     error "Enum DestDefaults: enumFromThen not implemented"
  enumFromThenTo _ _ _ =     error "Enum DestDefaults: enumFromThenTo not implemented"

{-# LINE 227 "./Graphics/UI/Gtk/General/Enums.chs" #-}

instance Flags DestDefaults


-- | Gives an indication why a drag operation failed. The value can by
-- obtained by connecting to the 'dragFailed' signal.
--
-- * 'DragResultSuccess': The drag operation was successful
--
-- * 'DragResultNoTarget': No suitable drag target
--
-- * 'DragResultUserCancelled': The user cancelled the drag operation
--
-- * 'DragResultTimeoutExpired': The drag operation timed out
--
-- * 'DragResultGrabBroken': The pointer or keyboard grab used for the drag
-- operation was broken
--
-- * 'DragResultError': The drag operation failed due to some unspecified error
--
data DragResult = DragResultSuccess
                | DragResultNoTarget
                | DragResultUserCancelled
                | DragResultTimeoutExpired
                | DragResultGrabBroken
                | DragResultError
                deriving (Enum,Bounded,Eq,Show)

{-# LINE 248 "./Graphics/UI/Gtk/General/Enums.chs" #-}


-- | Editing direction
--
data DirectionType = DirTabForward
                   | DirTabBackward
                   | DirUp
                   | DirDown
                   | DirLeft
                   | DirRight
                   deriving (Enum,Eq,Show)

{-# LINE 253 "./Graphics/UI/Gtk/General/Enums.chs" #-}

-- | Justification for label and maybe other widgets (text?)
--
data Justification = JustifyLeft
                   | JustifyRight
                   | JustifyCenter
                   | JustifyFill
                   deriving (Enum,Eq,Show)

{-# LINE 257 "./Graphics/UI/Gtk/General/Enums.chs" #-}







-- | Some kind of string search options
--
-- Removed in Gtk3.
data MatchType = MatchAll
               | MatchAllTail
               | MatchHead
               | MatchTail
               | MatchExact
               | MatchLast
               deriving (Enum,Eq,Show)

{-# LINE 268 "./Graphics/UI/Gtk/General/Enums.chs" #-}



-- | From where was a menu item entered?
--
data MenuDirectionType = MenuDirParent
                       | MenuDirChild
                       | MenuDirNext
                       | MenuDirPrev
                       deriving (Enum,Eq,Show)

{-# LINE 274 "./Graphics/UI/Gtk/General/Enums.chs" #-}


-- | Units of measure
--
-- Removed in Gtk3.
data MetricType = Pixels
                | Inches
                | Centimeters
                deriving (Enum,Eq,Show)

{-# LINE 280 "./Graphics/UI/Gtk/General/Enums.chs" #-}


-- | Movement in text widget
--
data MovementStep = MovementLogicalPositions
                  | MovementVisualPositions
                  | MovementWords
                  | MovementDisplayLines
                  | MovementDisplayLineEnds
                  | MovementParagraphs
                  | MovementParagraphEnds
                  | MovementPages
                  | MovementBufferEnds
                  | MovementHorizontalPages
                  deriving (Enum,Eq,Show)

{-# LINE 285 "./Graphics/UI/Gtk/General/Enums.chs" #-}

-- | Orientation is good
--
data Orientation = OrientationHorizontal
                 | OrientationVertical
                 deriving (Enum,Eq,Show)

{-# LINE 289 "./Graphics/UI/Gtk/General/Enums.chs" #-}

-- | Packing parameters of a widget
--
-- * The 'Packing' parameter determines how the child behaves in the horizontal
-- or vertical way in an 'Graphics.UI.Gtk.Layout.HBox' or
-- 'Graphics.UI.Gtk.Layout.VBox', respectively. 'PackNatural'
-- means the child is as big as it requests. It will stay at the start or
-- end of a 'Graphics.UI.Gtk.Layout.Box' if there is more space available.
-- All children packed with 'PackRepel' will be padded on both sides with
-- additional space. 'PackGrow' will increase the size of a widget so that it
-- covers the available space. A menu bar, for instance, should always
-- stay at the top of a window and should only occupy as little space
-- as possible. Hence it should be packed at the start of a
-- 'Graphics.UI.Gtk.Layout.VBox' with
-- the packing option 'PackNatural'. The working area of a window
-- (e.g. the text area in an editor) should expand when the window is
-- resized. Here the packing option 'PackGrow' is the right choice and
-- it is irrelevant whether the main area is inserted at the start or
-- the end of a box. Finally 'PackRepel' is most useful in a window
-- where no widget can make use of excess space. Examples include a
-- dialog box without list boxes or text fields.
--
data Packing = PackRepel
             | PackGrow
             | PackNatural
             deriving (Enum,Eq,Show)

-- The conversions between our Packing type and Gtk's expand and fill
-- properties.
--
toPacking :: Bool -> Bool -> Packing
toPacking expand True = PackGrow
toPacking True fill = PackRepel
toPacking False fill = PackNatural

fromPacking :: Packing -> (Bool, Bool)
fromPacking PackGrow = (True,True)
fromPacking PackRepel = (True,False)
fromPacking PackNatural = (False,False)

-- | Packing of widgets at start or end in a box
--
data PackType = PackStart
              | PackEnd
              deriving (Enum,Eq,Show)

{-# LINE 332 "./Graphics/UI/Gtk/General/Enums.chs" #-}

-- | Priorities
--
data PathPriorityType = PathPrioLowest
                      | PathPrioGtk
                      | PathPrioApplication
                      | PathPrioTheme
                      | PathPrioRc
                      | PathPrioHighest
                      deriving (Eq,Show)
instance Enum PathPriorityType where
  fromEnum PathPrioLowest = 0
  fromEnum PathPrioGtk = 4
  fromEnum PathPrioApplication = 8
  fromEnum PathPrioTheme = 10
  fromEnum PathPrioRc = 12
  fromEnum PathPrioHighest = 15

  toEnum 0 = PathPrioLowest
  toEnum 4 = PathPrioGtk
  toEnum 8 = PathPrioApplication
  toEnum 10 = PathPrioTheme
  toEnum 12 = PathPrioRc
  toEnum 15 = PathPrioHighest
  toEnum unmatched = error ("PathPriorityType.toEnum: Cannot match " ++ show unmatched)

  succ PathPrioLowest = PathPrioGtk
  succ PathPrioGtk = PathPrioApplication
  succ PathPrioApplication = PathPrioTheme
  succ PathPrioTheme = PathPrioRc
  succ PathPrioRc = PathPrioHighest
  succ _ = undefined

  pred PathPrioGtk = PathPrioLowest
  pred PathPrioApplication = PathPrioGtk
  pred PathPrioTheme = PathPrioApplication
  pred PathPrioRc = PathPrioTheme
  pred PathPrioHighest = PathPrioRc
  pred _ = undefined

  enumFromTo x y | fromEnum x == fromEnum y = [ y ]
                 | otherwise = x : enumFromTo (succ x) y
  enumFrom x = enumFromTo x PathPrioHighest
  enumFromThen _ _ =     error "Enum PathPriorityType: enumFromThen not implemented"
  enumFromThenTo _ _ _ =     error "Enum PathPriorityType: enumFromThenTo not implemented"

{-# LINE 336 "./Graphics/UI/Gtk/General/Enums.chs" #-}

-- | Widget identification path
--
data PathType = PathWidget
              | PathWidgetClass
              | PathClass
              deriving (Enum,Eq,Show)

{-# LINE 340 "./Graphics/UI/Gtk/General/Enums.chs" #-}

-- | Scrollbar policy types (for scrolled windows)
--
data PolicyType = PolicyAlways
                | PolicyAutomatic
                | PolicyNever
                deriving (Enum,Eq,Show)

{-# LINE 344 "./Graphics/UI/Gtk/General/Enums.chs" #-}

-- | Position a scale's value is drawn relative to the
-- trough
--
data PositionType = PosLeft
                  | PosRight
                  | PosTop
                  | PosBottom
                  deriving (Enum,Eq,Show)

{-# LINE 349 "./Graphics/UI/Gtk/General/Enums.chs" #-}


-- | Is the ProgressBar horizontally or vertically
-- directed?
--
-- Removed in Gtk3.
data ProgressBarOrientation = ProgressLeftToRight
                            | ProgressRightToLeft
                            | ProgressBottomToTop
                            | ProgressTopToBottom
                            deriving (Enum,Eq,Show)

{-# LINE 356 "./Graphics/UI/Gtk/General/Enums.chs" #-}


-- | I don't have a clue.
--
data ReliefStyle = ReliefNormal
                 | ReliefHalf
                 | ReliefNone
                 deriving (Enum,Eq,Show)

{-# LINE 361 "./Graphics/UI/Gtk/General/Enums.chs" #-}

-- | Resize mode, for containers
--
-- * 'ResizeParent' Pass resize request to the parent
--
-- * 'ResizeQueue' Queue resizes on this widget
--
-- * 'ResizeImmediate' Perform the resizes now
--
data ResizeMode = ResizeParent
                | ResizeQueue
                | ResizeImmediate
                deriving (Enum,Eq,Show)

{-# LINE 371 "./Graphics/UI/Gtk/General/Enums.chs" #-}

-- | Scrolling type
--
data ScrollType = ScrollNone
                | ScrollJump
                | ScrollStepBackward
                | ScrollStepForward
                | ScrollPageBackward
                | ScrollPageForward
                | ScrollStepUp
                | ScrollStepDown
                | ScrollPageUp
                | ScrollPageDown
                | ScrollStepLeft
                | ScrollStepRight
                | ScrollPageLeft
                | ScrollPageRight
                | ScrollStart
                | ScrollEnd
                deriving (Enum,Eq,Show)

{-# LINE 375 "./Graphics/UI/Gtk/General/Enums.chs" #-}

-- | Scrolling step
--
data ScrollStep = ScrollSteps
                | ScrollPages
                | ScrollEnds
                | ScrollHorizontalSteps
                | ScrollHorizontalPages
                | ScrollHorizontalEnds
                deriving (Enum,Eq,Show)

{-# LINE 379 "./Graphics/UI/Gtk/General/Enums.chs" #-}

-- | Mode in which selections can be performed
--
-- * There is a deprecated entry SelectionExtended which should have the same
-- value as SelectionMultiple. C2HS chokes on that construct.
--
data SelectionMode = SelectionNone
                   | SelectionSingle
                   | SelectionBrowse
                   | SelectionMultiple
                   deriving (Enum,Eq,Show)
-- {#enum SelectionMode {underscoreToCase} deriving (Eq,Show)#}

-- | Shadow types
--
data ShadowType = ShadowNone
                | ShadowIn
                | ShadowOut
                | ShadowEtchedIn
                | ShadowEtchedOut
                deriving (Enum,Eq,Show)

{-# LINE 406 "./Graphics/UI/Gtk/General/Enums.chs" #-}
-- Sort a 'Graphics.UI.Gtk.ModelView.TreeViewColumn' in ascending or descending
-- order.
--
data SortType = SortAscending
              | SortDescending
              deriving (Enum,Eq,Show)

{-# LINE 410 "./Graphics/UI/Gtk/General/Enums.chs" #-}

-- | Widget states
--
data StateType = StateNormal
               | StateActive
               | StatePrelight
               | StateSelected
               | StateInsensitive
               deriving (Enum,Eq,Show)

{-# LINE 414 "./Graphics/UI/Gtk/General/Enums.chs" #-}



-- | Submenu direction policies
--
-- Removed in Gtk3.
data SubmenuDirection = DirectionLeft
                      | DirectionRight
                      deriving (Enum,Eq,Show)

{-# LINE 421 "./Graphics/UI/Gtk/General/Enums.chs" #-}

-- | Submenu placement policies
--
-- Removed in Gtk3.
data SubmenuPlacement = TopBottom
                      | LeftRight
                      deriving (Enum,Eq,Show)

{-# LINE 426 "./Graphics/UI/Gtk/General/Enums.chs" #-}



-- | Whether to clamp or ignore illegal values.
--
data SpinButtonUpdatePolicy = UpdateAlways
                            | UpdateIfValid
                            deriving (Enum,Eq,Show)

{-# LINE 432 "./Graphics/UI/Gtk/General/Enums.chs" #-}

-- | Spin a SpinButton with the following method.
--
data SpinType = SpinStepForward
              | SpinStepBackward
              | SpinPageForward
              | SpinPageBackward
              | SpinHome
              | SpinEnd
              | SpinUserDefined
              deriving (Enum,Eq,Show)

{-# LINE 436 "./Graphics/UI/Gtk/General/Enums.chs" #-}

-- | The 'TargetFlags' enumeration is used to specify constraints on an entry
-- in a 'Graphics.UI.Gtk.Gdk.Selection.TargetList'. These flags are only
-- used for drag and drop.
--
-- * If the 'TargetSameApp' flag is set, the target will only be selected for
-- drags within a single application.
--
-- * If the 'TargetSameWidget' flag is set, the target will only be selected
-- for drags within a single widget.
--
data TargetFlags = TargetSameApp
                 | TargetSameWidget
                 | TargetOtherApp
                 | TargetOtherWidget
                 deriving (Bounded,Eq,Show)
instance Enum TargetFlags where
  fromEnum TargetSameApp = 1
  fromEnum TargetSameWidget = 2
  fromEnum TargetOtherApp = 4
  fromEnum TargetOtherWidget = 8

  toEnum 1 = TargetSameApp
  toEnum 2 = TargetSameWidget
  toEnum 4 = TargetOtherApp
  toEnum 8 = TargetOtherWidget
  toEnum unmatched = error ("TargetFlags.toEnum: Cannot match " ++ show unmatched)

  succ TargetSameApp = TargetSameWidget
  succ TargetSameWidget = TargetOtherApp
  succ TargetOtherApp = TargetOtherWidget
  succ _ = undefined

  pred TargetSameWidget = TargetSameApp
  pred TargetOtherApp = TargetSameWidget
  pred TargetOtherWidget = TargetOtherApp
  pred _ = undefined

  enumFromTo x y | fromEnum x == fromEnum y = [ y ]
                 | otherwise = x : enumFromTo (succ x) y
  enumFrom x = enumFromTo x TargetOtherWidget
  enumFromThen _ _ =     error "Enum TargetFlags: enumFromThen not implemented"
  enumFromThenTo _ _ _ =     error "Enum TargetFlags: enumFromThenTo not implemented"

{-# LINE 448 "./Graphics/UI/Gtk/General/Enums.chs" #-}

instance Flags TargetFlags

-- | Is the text written from left to right or the exotic way?
--
data TextDirection = TextDirNone
                   | TextDirLtr
                   | TextDirRtl
                   deriving (Enum,Eq,Show)

{-# LINE 454 "./Graphics/UI/Gtk/General/Enums.chs" #-}

-- | Specify the way the search function for
-- 'Graphics.UI.Gtk.Multiline.TextBuffer' works.
--
data TextSearchFlags = TextSearchVisibleOnly
                     | TextSearchTextOnly
                     deriving (Bounded,Eq,Show)
instance Enum TextSearchFlags where
  fromEnum TextSearchVisibleOnly = 1
  fromEnum TextSearchTextOnly = 2

  toEnum 1 = TextSearchVisibleOnly
  toEnum 2 = TextSearchTextOnly
  toEnum unmatched = error ("TextSearchFlags.toEnum: Cannot match " ++ show unmatched)

  succ TextSearchVisibleOnly = TextSearchTextOnly
  succ _ = undefined

  pred TextSearchTextOnly = TextSearchVisibleOnly
  pred _ = undefined

  enumFromTo x y | fromEnum x == fromEnum y = [ y ]
                 | otherwise = x : enumFromTo (succ x) y
  enumFrom x = enumFromTo x TextSearchTextOnly
  enumFromThen _ _ =     error "Enum TextSearchFlags: enumFromThen not implemented"
  enumFromThenTo _ _ _ =     error "Enum TextSearchFlags: enumFromThenTo not implemented"

{-# LINE 459 "./Graphics/UI/Gtk/General/Enums.chs" #-}

instance Flags TextSearchFlags

-- | The window type for coordinate translation.
--
data TextWindowType = TextWindowPrivate
                    | TextWindowWidget
                    | TextWindowText
                    | TextWindowLeft
                    | TextWindowRight
                    | TextWindowTop
                    | TextWindowBottom
                    deriving (Enum,Eq,Show)

{-# LINE 465 "./Graphics/UI/Gtk/General/Enums.chs" #-}

-- | Where to place the toolbar?
--
data ToolbarStyle = ToolbarIcons
                  | ToolbarText
                  | ToolbarBoth
                  | ToolbarBothHoriz
                  deriving (Enum,Eq,Show)

{-# LINE 469 "./Graphics/UI/Gtk/General/Enums.chs" #-}

-- | Wether columns of a tree or list widget can be resized.
--
data TreeViewColumnSizing = TreeViewColumnGrowOnly
                          | TreeViewColumnAutosize
                          | TreeViewColumnFixed
                          deriving (Enum,Eq,Show)

{-# LINE 473 "./Graphics/UI/Gtk/General/Enums.chs" #-}

-- hm... text editing?
--{#enum TroughType {underscoreToCase} deriving (Eq,Show)#}


-- | Updating types for range widgets (determines when the
-- @\"connectToValueChanged\"@ signal is emitted by the widget)
--
-- Removed in Gtk3.
data UpdateType = UpdateContinuous
                | UpdateDiscontinuous
                | UpdateDelayed
                deriving (Enum,Eq,Show)

{-# LINE 483 "./Graphics/UI/Gtk/General/Enums.chs" #-}

-- | Visibility
--
-- Removed in Gtk3.
data Visibility = VisibilityNone
                | VisibilityPartial
                | VisibilityFull
                deriving (Enum,Eq,Show)

{-# LINE 488 "./Graphics/UI/Gtk/General/Enums.chs" #-}


-- | Window position types
--
data WindowPosition = WinPosNone
                    | WinPosCenter
                    | WinPosMouse
                    | WinPosCenterAlways
                    | WinPosCenterOnParent
                    deriving (Enum,Eq,Show)

{-# LINE 493 "./Graphics/UI/Gtk/General/Enums.chs" #-}

-- | Interaction of a window with window manager
--
data WindowType = WindowToplevel
                | WindowPopup
                deriving (Enum,Eq,Show)

{-# LINE 497 "./Graphics/UI/Gtk/General/Enums.chs" #-}

-- | Determine how lines are wrapped in a 'Graphics.UI.Gtk.Multiline.TextView'.
--
data WrapMode = WrapNone
              | WrapChar
              | WrapWord
              | WrapWordChar
              deriving (Enum,Eq,Show)

{-# LINE 501 "./Graphics/UI/Gtk/General/Enums.chs" #-}


-- | Specifies the side of the entry at which an icon is placed.
--
data EntryIconPosition = EntryIconPrimary
                       | EntryIconSecondary
                       deriving (Enum,Eq,Show)

{-# LINE 506 "./Graphics/UI/Gtk/General/Enums.chs" #-}



-- |
--
-- Removed in Gtk3.
data AnchorType = AnchorCenter
                | AnchorNorth
                | AnchorNorthWest
                | AnchorNorthEast
                | AnchorSouth
                | AnchorSouthWest
                | AnchorSouthEast
                | AnchorWest
                | AnchorEast
                | AnchorN
                | AnchorNw
                | AnchorNe
                | AnchorS
                | AnchorSw
                | AnchorSe
                | AnchorW
                | AnchorE
                deriving (Eq,Show)
instance Enum AnchorType where
  fromEnum AnchorCenter = 0
  fromEnum AnchorNorth = 1
  fromEnum AnchorNorthWest = 2
  fromEnum AnchorNorthEast = 3
  fromEnum AnchorSouth = 4
  fromEnum AnchorSouthWest = 5
  fromEnum AnchorSouthEast = 6
  fromEnum AnchorWest = 7
  fromEnum AnchorEast = 8
  fromEnum AnchorN = 1
  fromEnum AnchorNw = 2
  fromEnum AnchorNe = 3
  fromEnum AnchorS = 4
  fromEnum AnchorSw = 5
  fromEnum AnchorSe = 6
  fromEnum AnchorW = 7
  fromEnum AnchorE = 8

  toEnum 0 = AnchorCenter
  toEnum 1 = AnchorNorth
  toEnum 2 = AnchorNorthWest
  toEnum 3 = AnchorNorthEast
  toEnum 4 = AnchorSouth
  toEnum 5 = AnchorSouthWest
  toEnum 6 = AnchorSouthEast
  toEnum 7 = AnchorWest
  toEnum 8 = AnchorEast
  toEnum unmatched = error ("AnchorType.toEnum: Cannot match " ++ show unmatched)

  succ AnchorCenter = AnchorNorth
  succ AnchorNorth = AnchorNorthWest
  succ AnchorNorthWest = AnchorNorthEast
  succ AnchorNorthEast = AnchorSouth
  succ AnchorSouth = AnchorSouthWest
  succ AnchorSouthWest = AnchorSouthEast
  succ AnchorSouthEast = AnchorWest
  succ AnchorWest = AnchorEast
  succ AnchorEast = AnchorN
  succ AnchorN = AnchorNw
  succ AnchorNw = AnchorNe
  succ AnchorNe = AnchorS
  succ AnchorS = AnchorSw
  succ AnchorSw = AnchorSe
  succ AnchorSe = AnchorW
  succ AnchorW = AnchorE
  succ _ = undefined

  pred AnchorNorth = AnchorCenter
  pred AnchorNorthWest = AnchorNorth
  pred AnchorNorthEast = AnchorNorthWest
  pred AnchorSouth = AnchorNorthEast
  pred AnchorSouthWest = AnchorSouth
  pred AnchorSouthEast = AnchorSouthWest
  pred AnchorWest = AnchorSouthEast
  pred AnchorEast = AnchorWest
  pred AnchorN = AnchorEast
  pred AnchorNw = AnchorN
  pred AnchorNe = AnchorNw
  pred AnchorS = AnchorNe
  pred AnchorSw = AnchorS
  pred AnchorSe = AnchorSw
  pred AnchorW = AnchorSe
  pred AnchorE = AnchorW
  pred _ = undefined

  enumFromTo x y | fromEnum x == fromEnum y = [ y ]
                 | otherwise = x : enumFromTo (succ x) y
  enumFrom x = enumFromTo x AnchorE
  enumFromThen _ _ =     error "Enum AnchorType: enumFromThen not implemented"
  enumFromThenTo _ _ _ =     error "Enum AnchorType: enumFromThenTo not implemented"

{-# LINE 513 "./Graphics/UI/Gtk/General/Enums.chs" #-}
