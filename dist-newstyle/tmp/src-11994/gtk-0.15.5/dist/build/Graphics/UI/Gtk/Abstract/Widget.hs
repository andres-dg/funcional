
{-# LINE 2 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget Widget
--
-- Author : Axel Simon
--
-- Created: 27 April 2001
--
-- Copyright (C) 2001-2008 Axel Simon
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
--
-- |
-- Maintainer : gtk2hs-users@lists.sourceforge.net
-- Stability : provisional
-- Portability : portable (depends on GHC)
--
-- The base class for all widgets.
--
module Graphics.UI.Gtk.Abstract.Widget (

-- * Detail
--
-- | The base class for all widgets. While a widget cannot be created directly,
-- this module contains many useful methods common to all widgets. In
-- particular, these functions are needed to add functionality to
-- blank widgets such as 'DrawingArea' or 'Layout'.
--
-- 'Widget' introduces style properties - these are basically object
-- properties that are stored not on the object, but in the style object
-- associated to the widget. Style properties are set in resource files. This
-- mechanism is used for configuring such things as the location of the
-- scrollbar arrows through the theme, giving theme authors more control over
-- the look of applications without the need to write a theme engine in C.
--
-- Widgets receive events, that is, signals that indicate some low-level
-- user iteraction. The signal handlers for all these events have to
-- return @True@ if the signal has been dealt with and @False@ if other
-- signal handlers should be run.

-- * Class Hierarchy
--
-- |
-- @
-- | 'GObject'
-- | +----'Object'
-- | +----Widget
-- | +----/too many to list/
-- @

-- * Types
  Widget,
  WidgetClass,
  castToWidget, gTypeWidget,
  toWidget,
  EventMask(..),

  ExtensionMode(..),

  GType,
  KeyVal,

  Region,
  Bitmap,

  Requisition(..),
  Rectangle(..),
  Color,
  IconSize(..),
  StateType(..),
  TextDirection(..),
  AccelFlags(..),
  DirectionType(..),
  StockId,
  WidgetHelpType(..),
  Allocation,

-- * Methods
  widgetShow,
  widgetShowNow,
  widgetHide,
  widgetShowAll,

  widgetHideAll,

  widgetDestroy,



  widgetQueueDraw,
  widgetQueueResize,

  widgetQueueResizeNoRedraw,







  widgetSizeRequest,
  widgetGetChildRequisition,
  widgetSizeAllocate,



  widgetAddAccelerator,
  widgetRemoveAccelerator,
  widgetSetAccelPath,

  widgetCanActivateAccel,

  widgetActivate,
  widgetIntersect,
  widgetHasIntersection,
  widgetGetIsFocus,
  widgetGrabFocus,
  widgetGrabDefault,
  widgetSetName,
  widgetGetName,
  widgetSetSensitive,
  widgetSetSensitivity,
  widgetGetParentWindow,

  widgetGetDrawWindow,

  widgetDelEvents,
  widgetAddEvents,
  widgetGetEvents,
  widgetSetEvents,

  widgetSetExtensionEvents,
  widgetGetExtensionEvents,

  widgetGetToplevel,
  widgetGetAncestor,

  widgetGetColormap,
  widgetSetColormap,

  widgetGetPointer,
  widgetIsAncestor,
  widgetTranslateCoordinates,
  widgetSetStyle,
  widgetGetStyle,

  widgetPushColormap,
  widgetPopColormap,
  widgetSetDefaultColormap,
  widgetGetDefaultColormap,

  widgetGetDefaultStyle,
  widgetSetDirection,
  widgetGetDirection,
  widgetSetDefaultDirection,
  widgetGetDefaultDirection,

  widgetShapeCombineMask,

  widgetInputShapeCombineMask,
{-# LINE 178 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
  widgetGetSnapshot,


  widgetPath,
  widgetClassPath,
  widgetGetCompositeName,







  widgetModifyStyle,
  widgetGetModifierStyle,
  widgetModifyFg,
  widgetModifyBg,
  widgetModifyText,
  widgetModifyBase,
  widgetModifyFont,
  widgetRestoreFg,
  widgetRestoreBg,
  widgetRestoreText,
  widgetRestoreBase,
  widgetCreatePangoContext,
  widgetGetPangoContext,
  widgetCreateLayout,
  widgetRenderIcon,
  widgetQueueDrawArea,




  widgetResetShapes,

  widgetSetAppPaintable,
  widgetSetDoubleBuffered,
  widgetSetRedrawOnAllocate,
  widgetSetCompositeName,
  widgetMnemonicActivate,

  widgetSetScrollAdjustments,
  widgetRegionIntersect,

  widgetGetAccessible,
  widgetChildFocus,
  widgetGetChildVisible,
  widgetGetParent,
  widgetGetSettings,

  widgetGetClipboard,
  widgetGetDisplay,
  widgetGetRootWindow,
  widgetGetScreen,
  widgetHasScreen,

  widgetGetSizeRequest,
  widgetSetChildVisible,
  widgetSetSizeRequest,

  widgetSetNoShowAll,
  widgetGetNoShowAll,
  widgetListMnemonicLabels,
  widgetAddMnemonicLabel,
  widgetRemoveMnemonicLabel,


  widgetGetAction,

  widgetIsComposited,



  widgetErrorBell,
  widgetKeynavFailed,
  widgetGetTooltipMarkup,
  widgetSetTooltipMarkup,
  widgetGetTooltipText,
  widgetSetTooltipText,
  widgetGetTooltipWindow,
  widgetSetTooltipWindow,
  widgetGetHasTooltip,
  widgetSetHasTooltip,
  widgetTriggerTooltipQuery,


  widgetGetWindow,
{-# LINE 274 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
  widgetReparent,

  widgetGetCanFocus,
  widgetSetCanFocus,
  widgetGetAllocation,
{-# LINE 292 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
  widgetGetAppPaintable,
  widgetGetCanDefault,
  widgetSetCanDefault,
  widgetGetHasWindow,
  widgetSetHasWindow,
  widgetGetSensitive,
  widgetIsSensitive,
  widgetGetState,
  widgetGetVisible,
{-# LINE 311 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
  widgetGetHasDefault,
  widgetGetHasFocus,





  widgetHasGrab,
  widgetIsDrawable,
  widgetIsToplevel,
  widgetSetWindow,
  widgetSetReceivesDefault,
  widgetGetReceivesDefault,
{-# LINE 335 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
  widgetSetState,

  widgetGetSavedState,
  widgetGetSize,

  widgetEvent,
{-# LINE 351 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
-- * Attributes
  widgetName,
  widgetParent,
  widgetWidthRequest,
  widgetHeightRequest,
  widgetMarginLeft,
  widgetMarginRight,




  widgetMarginTop,
  widgetMarginBottom,
  widgetVisible,
  widgetOpacity,
  widgetSensitive,
  widgetAppPaintable,
  widgetCanFocus,
  widgetHasFocus,
  widgetIsFocus,
  widgetCanDefault,
  widgetHasDefault,
  widgetReceivesDefault,
  widgetCompositeChild,
  widgetStyle,
  widgetState,
  widgetEvents,

  widgetExtensionEvents,

  widgetExpand,
  widgetHExpand,
  widgetHExpandSet,
  widgetVExpand,
  widgetVExpandSet,
  widgetNoShowAll,
  widgetChildVisible,

  widgetColormap,

  widgetCompositeName,
  widgetDirection,
  widgetTooltipMarkup,
  widgetTooltipText,
  widgetHasTooltip,

  widgetHasRcStyle,
  widgetGetRealized,
  widgetGetMapped,
  widgetSetRealized,
  widgetSetMapped,





-- * Signals
  realize,
  unrealize,
  mapSignal,
  unmapSignal,
  sizeRequest,
  sizeAllocate,
  showSignal,
  hideSignal,
  focus,
  stateChanged,



  parentSet,
  hierarchyChanged,
  styleSet,
  directionChanged,
  grabNotify,
  popupMenuSignal,
  showHelp,
  accelClosuresChanged,
  screenChanged,
  queryTooltip,




-- * Events
  buttonPressEvent,
  buttonReleaseEvent,
  configureEvent,
  deleteEvent,
  destroyEvent,
  enterNotifyEvent,
  exposeEvent,
  focusInEvent,
  focusOutEvent,

  grabBrokenEvent,

  keyPressEvent,
  keyReleaseEvent,
  leaveNotifyEvent,
  mapEvent,
  motionNotifyEvent,
  noExposeEvent,
  proximityInEvent,
  proximityOutEvent,
  scrollEvent,
  unmapEvent,
  visibilityNotifyEvent,
  windowStateEvent,

-- * Deprecated

  onButtonPress,
  afterButtonPress,
  onButtonRelease,
  afterButtonRelease,
  onClient,
  afterClient,
  onConfigure,
  afterConfigure,
  onDelete,
  afterDelete,
  onDestroyEvent, -- you probably want onDestroy
  afterDestroyEvent,
  onDirectionChanged,
  afterDirectionChanged,
  onEnterNotify,
  afterEnterNotify,
  onLeaveNotify,
  afterLeaveNotify,
  onExpose,
  afterExpose,
  onExposeRect,
  afterExposeRect,
  onFocus,
  afterFocus,
  onFocusIn,
  afterFocusIn,
  onFocusOut,
  afterFocusOut,
  onGrabFocus,
  afterGrabFocus,
  onDestroy,
  afterDestroy,
  onHide,
  afterHide,
  onHierarchyChanged,
  afterHierarchyChanged,
  onKeyPress,
  afterKeyPress,
  onKeyRelease,
  afterKeyRelease,
  onMnemonicActivate,
  afterMnemonicActivate,
  onMotionNotify,
  afterMotionNotify,
  onParentSet,
  afterParentSet,
  onPopupMenu,
  afterPopupMenu,
  onProximityIn,
  afterProximityIn,
  onProximityOut,
  afterProximityOut,
  onRealize,
  afterRealize,
  onScroll,
  afterScroll,
  onShow,
  afterShow,
  onSizeAllocate,
  afterSizeAllocate,
  onSizeRequest,
  afterSizeRequest,
  onStateChanged,
  afterStateChanged,
  onUnmap,
  afterUnmap,
  onUnrealize,
  afterUnrealize,
  onVisibilityNotify,
  afterVisibilityNotify,
  onWindowState,
  afterWindowState

  ) where

import Control.Monad (liftM, unless)
import Data.Maybe (fromMaybe)
import Control.Monad.Reader (ask)
import Control.Monad.Trans (liftIO)
import System.Glib.FFI
import System.Glib.Flags (fromFlags, toFlags)




import System.Glib.UTFString
import System.Glib.Attributes
import System.Glib.Properties
import System.Glib.GType (GType)
import System.Glib.GList (fromGList)
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.UI.Gtk.General.DNDTypes (Atom (Atom), SelectionTag)
import Graphics.UI.Gtk.Types
{-# LINE 556 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
import Graphics.UI.Gtk.Signals
{-# LINE 557 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
import Graphics.UI.Gtk.Gdk.Enums (EventMask(..)

    , ExtensionMode(..)

    )
import Graphics.UI.Gtk.Gdk.Keys (KeyVal)

import Graphics.UI.Gtk.Gdk.Region (Region(..), makeNewRegion)
import Graphics.UI.Gtk.Gdk.Pixmap (Bitmap)

import Graphics.UI.Gtk.General.Structs (Allocation, Rectangle(..)
                                        ,Requisition(..), Color, IconSize(..)
                                        ,Point




                                        ,widgetGetSavedState
                                        ,widgetGetDrawWindow, widgetGetSize

                                        )

import Graphics.UI.Gtk.Gdk.Events (Event(..), marshalEvent, marshExposeRect)

import Graphics.UI.Gtk.Gdk.EventM (EventM,
  EventM,
  EAny,
  EKey,
  EButton,
  EScroll,
  EMotion,
  EExpose,
  EVisibility,
  ECrossing,
  EFocus,
  EConfigure,
  EProperty,
  EProximity,
  EWindowState,

  EGrabBroken,

  )
import Graphics.UI.Gtk.General.Enums (StateType(..), TextDirection(..),
                                         AccelFlags(..), DirectionType(..), Modifier






                                        )
import Graphics.Rendering.Pango.Types
{-# LINE 610 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
import Graphics.Rendering.Pango.BasicTypes (FontDescription(FontDescription),
                                         PangoLayout(PangoLayout), makeNewPangoString )
import Graphics.UI.Gtk.General.StockItems (StockId)
import Data.IORef ( newIORef )
import Control.Monad.Reader ( runReaderT )






{-# LINE 621 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}

--------------------
-- Methods

-- | Flags a widget to be displayed. Any widget that isn't shown will not
-- appear on the screen. If you want to show all the widgets in a container,
-- it's easier to call 'widgetShowAll' on the container, instead of
-- individually showing the widgets.
--
-- Remember that you have to show the containers containing a widget, in
-- addition to the widget itself, before it will appear onscreen.
--
-- When a toplevel container is shown, it is immediately realized and
-- mapped; other shown widgets are realized and mapped when their toplevel
-- container is realized and mapped.
--
widgetShow :: WidgetClass self => self -> IO ()
widgetShow self =
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_show argPtr1)
{-# LINE 640 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)

-- | Shows a widget. If the widget is an unmapped toplevel widget (i.e. a
-- 'Window' that has not yet been shown), enter the main loop and wait for the
-- window to actually be mapped. Be careful; because the main loop is running,
-- anything can happen during this function.
--
widgetShowNow :: WidgetClass self => self -> IO ()
widgetShowNow self =
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_show_now argPtr1)
{-# LINE 650 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)

-- | Reverses the effects of 'widgetShow', causing the widget to be hidden
-- (invisible to the user).
--
widgetHide :: WidgetClass self => self -> IO ()
widgetHide self =
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_hide argPtr1)
{-# LINE 658 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)

-- | Recursively shows a widget, and any child widgets (if the widget is a
-- container).
--
widgetShowAll :: WidgetClass self => self -> IO ()
widgetShowAll self =
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_show_all argPtr1)
{-# LINE 666 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)


-- | Recursively hides a widget and any child widgets.
--
-- Removed in Gtk3.
widgetHideAll :: WidgetClass self => self -> IO ()
widgetHideAll self =
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_hide_all argPtr1)
{-# LINE 675 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)


-- | Destroys a widget. Equivalent to
-- 'Graphics.UI.Gtk.Abstract.Object.objectDestroy'.
--
-- When a widget is destroyed it will be removed from the screen and
-- unrealized. When a widget is destroyed, it will break any references it
-- holds to other objects.If the widget is inside a container, the widget will
-- be removed from the container. The widget will be garbage collected
-- (finalized) time after your last reference to the widget dissapears.
--
-- In most cases, only toplevel widgets (windows) require explicit
-- destruction, because when you destroy a toplevel its children will be
-- destroyed as well.
--
widgetDestroy :: WidgetClass self => self -> IO ()
widgetDestroy self =
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_destroy argPtr1)
{-# LINE 694 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
{-# LINE 726 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
-- * Functions to be used with 'Graphics.UI.Gtk.Misc.DrawingArea' or
-- container implementations.

-- | Send a redraw request to a widget. Equivalent to calling
-- 'widgetQueueDrawArea' for the entire area of a widget.
--
widgetQueueDraw :: WidgetClass self => self -> IO ()
widgetQueueDraw self =
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_queue_draw argPtr1)
{-# LINE 735 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)

-- | This function is only for use in widget implementations. Flags a widget
-- to have its size renegotiated; should be called when a widget for some
-- reason has a new size request. For example, when you change the text in a
-- 'Graphics.UI.Gtk.Display.Label.Label',
-- 'Graphics.UI.Gtk.Display.Label.Label' queues a resize to ensure there's
-- enough space for the new text.
--
widgetQueueResize :: WidgetClass self => self -> IO ()
widgetQueueResize self =
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_queue_resize argPtr1)
{-# LINE 747 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)


-- | This function works like 'widgetQueueResize', except that the widget is
-- not invalidated.
--
-- * Available since Gtk+ version 2.4
--
widgetQueueResizeNoRedraw :: WidgetClass self => self -> IO ()
widgetQueueResizeNoRedraw self =
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_queue_resize_no_redraw argPtr1)
{-# LINE 758 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
{-# LINE 803 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
-- | This function is typically used when implementing a
-- 'Graphics.UI.Gtk.Abstract.Container.Container' subclass. Obtains the preferred size
-- of a widget. The container uses this information to arrange its child
-- widgets and decide what size allocations to give them with
-- 'widgetSizeAllocate'.
--
-- You can also call this function from an application, with some caveats.
-- Most notably, getting a size request requires the widget to be associated
-- with a screen, because font information may be needed. Multihead-aware
-- applications should keep this in mind.
--
-- Also remember that the size request is not necessarily the size a widget
-- will actually be allocated.
--
widgetSizeRequest :: WidgetClass self => self -> IO Requisition
widgetSizeRequest self = alloca $ \reqPtr -> do
  (\(Widget arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_size_request argPtr1 arg2) (toWidget self) (castPtr reqPtr)
  peek reqPtr

-- | This function is only for use in widget implementations. Obtains the
-- chached requisition information in the widget, unless someone has forced a
-- particular geometry on the widget (e.g. with 'widgetSetUsize'), in which
-- case it returns that geometry instead of the widget's requisition.
--
-- This function differs from 'widgetSizeRequest' in that it retrieves the
-- last size request value stored in the widget, while 'widgetSizeRequest'
-- actually emits the 'sizeRequest' signal on the widget to compute the size
-- request (which updates the widget's requisition information).
--
-- Since this function does not emit the 'sizeRequest' signal, it can only be
-- used when you know that the widget's requisition is up-to-date, that is,
-- 'widgetSizeRequest' has been called since the last time a resize was
-- queued. In general, only container implementations have this information;
-- applications should use 'widgetSizeRequest'.
--
widgetGetChildRequisition :: WidgetClass self => self -> IO Requisition
widgetGetChildRequisition self = alloca $ \reqPtr -> do
  (\(Widget arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_get_child_requisition argPtr1 arg2) (toWidget self) (castPtr reqPtr)
  peek reqPtr

-- | This function is only used by
-- 'Graphics.UI.Gtk.Abstract.Container.Container' subclasses, to assign a
-- size and position to their child widgets.
--
widgetSizeAllocate :: WidgetClass self => self
  -> Allocation -- ^ The @x@ and @y@ values of the rectangle determine the
                -- the position of the widget's area relative to its parent
                -- allocation.
  -> IO ()
widgetSizeAllocate self rect = with rect $ \rectPtr ->
  (\(Widget arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_size_allocate argPtr1 arg2) (toWidget self) (castPtr rectPtr)
{-# LINE 880 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
-- %hash c:1e14 d:53c5
-- | Installs an accelerator for this @widget@ in @accelGroup@ that causes
-- @accelSignal@ to be emitted if the accelerator is activated. The
-- @accelGroup@ needs to be added to the widget's toplevel via
-- 'windowAddAccelGroup', and the signal must be of type @G_RUN_ACTION@.
-- Accelerators added through this function are not user changeable during
-- runtime. If you want to support accelerators that can be changed by the
-- user, use 'accelMapAddEntry' and 'widgetSetAccelPath' or
-- 'menuItemSetAccelPath' instead.
--
widgetAddAccelerator :: (WidgetClass self, GlibString string) => self
 -> string -- ^ @accelSignal@ - widget signal to emit on accelerator
                   -- activation
 -> AccelGroup -- ^ @accelGroup@ - accel group for this widget, added to
                   -- its toplevel
 -> KeyVal -- ^ @accelKey@ - the key of the accelerator
 -> [Modifier] -- ^ @accelMods@ - modifier key combination of the
                   -- accelerator
 -> [AccelFlags] -- ^ @accelFlags@ - flag accelerators, e.g. 'AccelVisible'
 -> IO ()
widgetAddAccelerator self accelSignal accelGroup accelKey accelMods accelFlags =
  withUTFString accelSignal $ \accelSignalPtr ->
  (\(Widget arg1) arg2 (AccelGroup arg3) arg4 arg5 arg6 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg3 $ \argPtr3 ->gtk_widget_add_accelerator argPtr1 arg2 argPtr3 arg4 arg5 arg6)
{-# LINE 903 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
    accelSignalPtr
    accelGroup
    (fromIntegral accelKey)
    ((fromIntegral . fromFlags) accelMods)
    ((fromIntegral . fromFlags) accelFlags)

-- %hash c:3442 d:dfe8
-- | Removes an accelerator from @widget@, previously installed with
-- 'widgetAddAccelerator'.
--
widgetRemoveAccelerator :: WidgetClass self => self
 -> AccelGroup -- ^ @accelGroup@ - accel group for this widget
 -> KeyVal -- ^ @accelKey@ - the key of the accelerator
 -> [Modifier] -- ^ @accelMods@ - modifier key combination of the
                   -- accelerator
 -> IO Bool -- ^ returns whether an accelerator was installed and could
                   -- be removed
widgetRemoveAccelerator self accelGroup accelKey accelMods =
  liftM toBool $
  (\(Widget arg1) (AccelGroup arg2) arg3 arg4 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_widget_remove_accelerator argPtr1 argPtr2 arg3 arg4)
{-# LINE 924 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
    accelGroup
    (fromIntegral accelKey)
    ((fromIntegral . fromFlags) accelMods)

-- %hash c:f8d4 d:bd7f
-- | Given an accelerator group, @accelGroup@, and an accelerator path,
-- @accelPath@, sets up an accelerator in @accelGroup@ so whenever the key
-- binding that is defined for @accelPath@ is pressed, @widget@ will be
-- activated. This removes any accelerators (for any accelerator group)
-- installed by previous calls to 'widgetSetAccelPath'. Associating
-- accelerators with paths allows them to be modified by the user and the
-- modifications to be saved for future use. (See 'accelMapSave'.)
--
-- This function is a low level function that would most likely be used by a
-- menu creation system like 'ItemFactory'. If you use 'ItemFactory', setting
-- up accelerator paths will be done automatically.
--
-- Even when you you aren't using 'ItemFactory', if you only want to set up
-- accelerators on menu items 'menuItemSetAccelPath' provides a somewhat more
-- convenient interface.
--
widgetSetAccelPath :: (WidgetClass self, GlibString string) => self
 -> string -- ^ @accelPath@ - path used to look up the accelerator
 -> AccelGroup -- ^ @accelGroup@ - a 'AccelGroup'.
 -> IO ()
widgetSetAccelPath self accelPath accelGroup =
  withUTFString accelPath $ \accelPathPtr ->
  (\(Widget arg1) arg2 (AccelGroup arg3) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg3 $ \argPtr3 ->gtk_widget_set_accel_path argPtr1 arg2 argPtr3)
{-# LINE 953 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
    accelPathPtr
    accelGroup


-- %hash c:157e d:82ae
-- | Determines whether an accelerator that activates the signal identified by
-- @signalId@ can currently be activated. This is done by emitting the
-- 'canActivateAccel' signal on the widget the signal is attached to; if the
-- signal isn't overridden by a handler or in a derived widget, then the
-- default check is that the widget must be sensitive, and the widget and all
-- its ancestors mapped.
--
-- * Available since Gtk+ version 2.4
--
widgetCanActivateAccel :: WidgetClass self =>
 (ConnectId self) -- ^ @signalId@ - the ID of a signal installed on @widget@
 -> IO Bool -- ^ returns @True@ if the accelerator can be activated.
widgetCanActivateAccel (ConnectId signalId self) =
  liftM toBool $
  (\(Widget arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_can_activate_accel argPtr1 arg2)
{-# LINE 974 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
    (fromIntegral signalId)


-- | For widgets that can be \"activated\" (buttons, menu items, etc.) this
-- function activates them. Activation is what happens when you press Enter on
-- a widget during key navigation. If @widget@ isn't activatable, the function
-- returns @False@.
--
widgetActivate :: WidgetClass self => self
 -> IO Bool -- ^ returns @True@ if the widget was activatable
widgetActivate self =
  liftM toBool $
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_activate argPtr1)
{-# LINE 988 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)

-- | Computes the intersection of a widget's area and @area@, returning the
-- intersection, and returns @Nothing@ if there was no intersection.
--
widgetIntersect :: WidgetClass self => self
 -> Rectangle -- ^ @area@ - a rectangle
 -> IO (Maybe Rectangle) -- ^ returns the intersection or @Nothing@
widgetIntersect self area =
  with area $ \areaPtr ->
  alloca $ \intersectionPtr -> do
  hasIntersection <- (\(Widget arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_intersect argPtr1 arg2 arg3)
{-# LINE 1000 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
    (castPtr areaPtr)
    (castPtr intersectionPtr)
  if (toBool hasIntersection)
    then liftM Just $ peek intersectionPtr
    else return Nothing

-- | Check if the widget intersects with a given area.
--
widgetHasIntersection :: WidgetClass self => self
 -> Rectangle -- ^ @area@ - a rectangle
 -> IO Bool -- ^ returns @True@ if there was an intersection
widgetHasIntersection self area =
  liftM toBool $
  with area $ \areaPtr ->
  (\(Widget arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_intersect argPtr1 arg2 arg3)
{-# LINE 1016 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
    (castPtr areaPtr)
    (castPtr nullPtr)

-- %hash d:1cab
-- | Determines if the widget is the focus widget within its toplevel. (This
-- does not mean that the 'widgetHasFocus' attribute is necessarily set;
-- 'widgetHasFocus' will only be set if the toplevel widget additionally has
-- the global input focus.)
--
widgetGetIsFocus :: WidgetClass self => self
 -> IO Bool -- ^ returns @True@ if the widget is the focus widget.
widgetGetIsFocus self =
  liftM toBool $
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_is_focus argPtr1)
{-# LINE 1031 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)

-- %hash d:e1e
-- | Causes @widget@ to have the keyboard focus for the 'Window' it's inside.
-- @widget@ must be a focusable widget, such as a
-- 'Graphics.UI.Gtk.Entry.Entry'; something like
-- 'Graphics.UI.Gtk.Ornaments.Frame' won't work. (More precisely, it must have
-- the 'widgetCanFocus' flag set.)
--
widgetGrabFocus :: WidgetClass self => self -> IO ()
widgetGrabFocus self =
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_grab_focus argPtr1)
{-# LINE 1043 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)

-- %hash c:e5e9 d:412a
-- | Causes @widget@ to become the default widget. @widget@ must have the
-- 'canDefault' flag set. The default widget is
-- activated when the user presses Enter in a window. Default widgets must be
-- activatable, that is, 'widgetActivate' should affect them.
--
widgetGrabDefault :: WidgetClass self => self -> IO ()
widgetGrabDefault self =
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_grab_default argPtr1)
{-# LINE 1054 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)

-- %hash c:4f62 d:d05a
-- | Widgets can be named, which allows you to refer to them from a gtkrc
-- file. You can apply a style to widgets with a particular name in the gtkrc
-- file. See the documentation for gtkrc files.
--
-- Note that widget names are separated by periods in paths (see
-- 'widgetPath'), so names with embedded periods may cause confusion.
--
widgetSetName :: (WidgetClass self, GlibString string) => self
 -> string -- ^ @name@ - name for the widget
 -> IO ()
widgetSetName self name =
  withUTFString name $ \namePtr ->
  (\(Widget arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_set_name argPtr1 arg2)
{-# LINE 1070 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
    namePtr

-- | Retrieves the name of a widget. See 'widgetSetName' for the significance
-- of widget names.
--
widgetGetName :: (WidgetClass self, GlibString string) => self -> IO string
widgetGetName self =
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_get_name argPtr1)
{-# LINE 1079 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
  >>= peekUTFString

-- %hash c:25b1 d:f898
-- | Sets the sensitivity of a widget. A widget is sensitive if the user can
-- interact with it. Insensitive widgets are \"grayed out\" and the user can't
-- interact with them. Insensitive widgets are known as \"inactive\",
-- \"disabled\", or \"ghosted\" in some other toolkits.
--
widgetSetSensitive :: WidgetClass self => self
 -> Bool -- ^ @sensitive@ - @True@ to make the widget sensitive
 -> IO ()
widgetSetSensitive self sensitive =
  (\(Widget arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_set_sensitive argPtr1 arg2)
{-# LINE 1093 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
    (fromBool sensitive)

-- bad spelling backwards compatability definition
widgetSetSensitivity :: WidgetClass self => self -> Bool -> IO ()
widgetSetSensitivity = widgetSetSensitive

-- | Gets the widget's parent window.
--
widgetGetParentWindow :: WidgetClass self => self -> IO DrawWindow
widgetGetParentWindow self =
  makeNewGObject mkDrawWindow $
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_get_parent_window argPtr1)
{-# LINE 1106 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)

-- | Disable event signals.
--
-- * Remove events from the 'EventMask' of this widget. The event mask
-- determines which events a widget will receive. Events are signals
-- that return an 'Event' data type. On connecting to a such a signal,
-- the event mask is automatically adjusted so that he signal is emitted.
-- This function is useful to disable the reception of the signal. It
-- should be called whenever all signals receiving an 'Event'
-- have been disconnected.
--
widgetDelEvents :: WidgetClass self => self -> [EventMask] -> IO ()
widgetDelEvents self events = do
  mask <- (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_get_events argPtr1) (toWidget self)
  let mask' = mask .&. (complement (fromIntegral $ fromFlags events))
  (\(Widget arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_set_events argPtr1 arg2) (toWidget self) mask'

-- | Enable event signals.
--
-- * See 'widgetDelEvents'.
--
widgetAddEvents :: WidgetClass self => self -> [EventMask] -> IO ()
widgetAddEvents self [] = return ()
 -- special [] case to work around a GTK+ bug, see:
 -- http:
widgetAddEvents self events =
  (\(Widget arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_add_events argPtr1 arg2)
{-# LINE 1134 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
    (fromIntegral $ fromFlags events)

-- | Get enabled event signals.
--
-- * See 'widgetDelEvents'.
--
widgetGetEvents :: WidgetClass self => self -> IO [EventMask]
widgetGetEvents self =
  liftM (toFlags . fromIntegral) $
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_get_events argPtr1)
{-# LINE 1145 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)

-- %hash c:468a d:49a0
-- | Sets the event mask (see 'EventMask') for a widget. The event mask
-- determines which events a widget will receive. Keep in mind that different
-- widgets have different default event masks, and by changing the event mask
-- you may disrupt a widget's functionality, so be careful. This function must
-- be called while a widget is unrealized. Consider 'widgetAddEvents' for
-- widgets that are already realized, or if you want to preserve the existing
-- event mask. This function can't be used with 'NoWindow' widgets; to get
-- events on those widgets, place them inside a
-- 'Graphics.UI.Gtk.Misc.EventBox' and receive events on the event box.
--
widgetSetEvents :: WidgetClass self => self
 -> [EventMask] -- ^ @events@ - event mask
 -> IO ()
widgetSetEvents self events =
  (\(Widget arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_set_events argPtr1 arg2)
{-# LINE 1163 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
    (fromIntegral $ fromFlags events)


-- %hash c:4f2c d:781
-- | Sets the extension events mask to @mode@. See 'ExtensionMode' and
-- 'inputSetExtensionEvents'.
--
widgetSetExtensionEvents :: WidgetClass self => self
 -> [ExtensionMode]
 -> IO ()
widgetSetExtensionEvents self mode =
  (\(Widget arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_set_extension_events argPtr1 arg2)
{-# LINE 1176 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
    ((fromIntegral . fromFlags) mode)

-- %hash c:c824 d:e611
-- | Retrieves the extension events the widget will receive; see
-- 'widgetSetExtensionEvents'.
--
widgetGetExtensionEvents :: WidgetClass self => self
 -> IO [ExtensionMode]
widgetGetExtensionEvents self =
  liftM (toFlags . fromIntegral) $
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_get_extension_events argPtr1)
{-# LINE 1188 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)


-- %hash c:270b d:8877
-- | This function returns the topmost widget in the container hierarchy
-- @widget@ is a part of. If @widget@ has no parent widgets, it will be
-- returned as the topmost widget.
--
widgetGetToplevel :: WidgetClass self =>
    self -- ^ @widget@ - the widget in question
 -> IO Widget -- ^ returns the topmost ancestor of @widget@, or @widget@
              -- itself if there's no ancestor.
widgetGetToplevel self =
  makeNewObject mkWidget $
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_get_toplevel argPtr1)
{-# LINE 1203 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)

-- %hash c:17bc d:f8f9
-- | Gets the first ancestor of @widget@ with type @widgetType@. For example,
-- @widgetGetAncestor widget gTypeBox@ gets the first 'Box' that's
-- an ancestor of @widget@. See note about checking for a toplevel
-- 'Window' in the docs for 'widgetGetToplevel'.
--
-- Note that unlike 'widgetIsAncestor', 'widgetGetAncestor' considers
-- @widget@ to be an ancestor of itself.
--
widgetGetAncestor :: WidgetClass self => self
 -> GType -- ^ @widgetType@ - ancestor type
 -> IO (Maybe Widget) -- ^ returns the ancestor widget, or @Nothing@ if not found
widgetGetAncestor self widgetType = do
  ptr <- (\(Widget arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_get_ancestor argPtr1 arg2)
{-# LINE 1219 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
    widgetType
  if ptr==nullPtr then return Nothing else
    liftM Just $ makeNewObject mkWidget (return ptr)


-- %hash c:bd95 d:eb94
-- | Gets the colormap that will be used to render @widget@.
--
widgetGetColormap :: WidgetClass self => self
 -> IO Colormap -- ^ returns the colormap used by @widget@
widgetGetColormap self =
  makeNewGObject mkColormap $
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_get_colormap argPtr1)
{-# LINE 1233 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)

-- %hash c:cba1 d:ffeb
-- | Sets the colormap for the widget to the given value. Widget must not have
-- been previously realized. This probably should only be used from an 'init'
-- function (i.e. from the constructor for the widget).
--
widgetSetColormap :: WidgetClass self => self
 -> Colormap -- ^ @colormap@ - a colormap
 -> IO ()
widgetSetColormap self colormap =
  (\(Widget arg1) (Colormap arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_widget_set_colormap argPtr1 argPtr2)
{-# LINE 1245 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
    colormap


-- %hash c:3522 d:5637
-- | Obtains the location of the mouse pointer in widget coordinates. Widget
-- coordinates are a bit odd; for historical reasons, they are defined as
-- 'widgetGetParentWindow' coordinates for widgets that are not 'NoWindow' widgets,
-- and are relative to the widget's allocation's (x,y) for
-- widgets that are 'NoWindow' widgets.
--
widgetGetPointer :: WidgetClass self => self
 -> IO (Int, Int) -- ^ @(x, y)@ - X Y coordinate
widgetGetPointer self =
  alloca $ \xPtr ->
  alloca $ \yPtr ->
  (\(Widget arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_get_pointer argPtr1 arg2 arg3)
{-# LINE 1262 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
    xPtr
    yPtr
  >>
  peek xPtr >>= \x ->
  peek yPtr >>= \y ->
  return (fromIntegral x, fromIntegral y)

-- %hash c:499d
-- | Determines whether @widget@ is somewhere inside @ancestor@, possibly with
-- intermediate containers.
--
widgetIsAncestor :: (WidgetClass self, WidgetClass ancestor) =>
    self -- ^ @widget@ - the widget in question
 -> ancestor -- ^ @ancestor@ - another 'Widget'
 -> IO Bool -- ^ returns @True@ if @ancestor@ contains @widget@ as a child,
             -- grandchild, great grandchild, etc.
widgetIsAncestor self ancestor =
  liftM toBool $
  (\(Widget arg1) (Widget arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_widget_is_ancestor argPtr1 argPtr2)
{-# LINE 1282 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
    (toWidget ancestor)

-- %hash c:8661
-- | Translate coordinates relative to @srcWidget@'s allocation to coordinates
-- relative to @destWidget@'s allocations. In order to perform this operation,
-- both widgets must be realized, and must share a common toplevel.
--
widgetTranslateCoordinates :: (WidgetClass self, WidgetClass destWidget) =>
    self -- ^ @srcWidget@ - a 'Widget'
 -> destWidget -- ^ @destWidget@ - a 'Widget'
 -> Int -- ^ @srcX@ - X position relative to @srcWidget@
 -> Int -- ^ @srcY@ - Y position relative to @srcWidget@
 -> IO (Maybe (Int, Int)) -- ^ @Just (destX, destY)@ - X and Y position
                        -- relative to @destWidget@. Returns @Nothing@ if
                        -- either widget was not realized, or there was no
                        -- common ancestor.
widgetTranslateCoordinates self destWidget srcX srcY =
  alloca $ \destXPtr ->
  alloca $ \destYPtr -> do
  worked <- (\(Widget arg1) (Widget arg2) arg3 arg4 arg5 arg6 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_widget_translate_coordinates argPtr1 argPtr2 arg3 arg4 arg5 arg6)
{-# LINE 1303 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
    (toWidget destWidget)
    (fromIntegral srcX)
    (fromIntegral srcY)
    destXPtr
    destYPtr
  if (toBool worked)
    then do destX <- peek destXPtr
            destY <- peek destYPtr
            return (Just (fromIntegral destX, fromIntegral destY))
    else return Nothing

-- %hash c:596c d:b7e5
-- | Sets the 'Style' for a widget. You probably don't want
-- to use this function; it interacts badly with themes, because themes work by
-- replacing the 'Style'. Instead, use 'widgetModifyStyle'.
--
widgetSetStyle :: WidgetClass self => self
 -> Maybe Style -- ^ @style@ - a 'Style', or @Nothing@ to remove the effect of a previous
           -- 'widgetSetStyle' and go back to the default style
 -> IO ()
widgetSetStyle self style =
  (\(Widget arg1) (Style arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_widget_set_style argPtr1 argPtr2)
{-# LINE 1326 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
    (fromMaybe (Style nullForeignPtr) style)

-- | Retrieve the 'Style' associated with the widget.
--
widgetGetStyle :: WidgetClass widget => widget -> IO Style
widgetGetStyle widget = do
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_ensure_style argPtr1) (toWidget widget)
  makeNewGObject mkStyle $ (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_get_style argPtr1) (toWidget widget)


-- %hash c:d5ed d:dc10
-- | Pushes @cmap@ onto a global stack of colormaps; the topmost colormap on
-- the stack will be used to create all widgets. Remove @cmap@ with
-- 'widgetPopColormap'. There's little reason to use this function.
--
widgetPushColormap ::
    Colormap -- ^ @cmap@ - a 'Colormap'
 -> IO ()
widgetPushColormap cmap =
  (\(Colormap arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_push_colormap argPtr1)
{-# LINE 1347 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    cmap

-- %hash c:7300 d:2920
-- | Removes a colormap pushed with 'widgetPushColormap'.
--
widgetPopColormap :: IO ()
widgetPopColormap =
  gtk_widget_pop_colormap
{-# LINE 1355 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}

-- %hash c:1f73 d:590e
-- | Sets the default colormap to use when creating widgets.
-- 'widgetPushColormap' is a better function to use if you only want to affect
-- a few widgets, rather than all widgets.
--
widgetSetDefaultColormap ::
    Colormap -- ^ @colormap@ - a 'Colormap'
 -> IO ()
widgetSetDefaultColormap colormap =
  (\(Colormap arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_set_default_colormap argPtr1)
{-# LINE 1366 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    colormap


-- %hash c:e71b d:72c2
-- | Returns the default style used by all widgets initially.
--
widgetGetDefaultStyle ::
    IO Style -- ^ returns the default style. This 'Style' object is owned by
             -- Gtk and should not be modified.
widgetGetDefaultStyle =
  makeNewGObject mkStyle $
  gtk_widget_get_default_style
{-# LINE 1378 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}


-- %hash c:d731 d:52bf
-- | Obtains the default colormap used to create widgets.
--
widgetGetDefaultColormap ::
    IO Colormap -- ^ returns default widget colormap
widgetGetDefaultColormap =
  makeNewGObject mkColormap $
  gtk_widget_get_default_colormap
{-# LINE 1388 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}


-- | Sets the reading direction on a particular widget. This direction
-- controls the primary direction for widgets containing text, and also the
-- direction in which the children of a container are packed. The ability to
-- set the direction is present in order so that correct localization into
-- languages with right-to-left reading directions can be done. Generally,
-- applications will let the default reading direction present, except for
-- containers where the containers are arranged in an order that is explicitely
-- visual rather than logical (such as buttons for text justification).
--
-- If the direction is set to 'TextDirNone', then the value set by
-- 'widgetSetDefaultDirection' will be used.
--
widgetSetDirection :: WidgetClass self => self -> TextDirection -> IO ()
widgetSetDirection self dir =
  (\(Widget arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_set_direction argPtr1 arg2)
{-# LINE 1405 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
    ((fromIntegral . fromEnum) dir)

-- | Gets the reading direction for a particular widget. See
-- 'widgetSetDirection'.
--
widgetGetDirection :: WidgetClass self => self -> IO TextDirection
widgetGetDirection self =
  liftM (toEnum . fromIntegral) $
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_get_direction argPtr1)
{-# LINE 1415 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)

-- %hash c:ff9a
-- | Sets the default reading direction for widgets where the direction has
-- not been explicitly set by 'widgetSetDirection'.
--
widgetSetDefaultDirection ::
    TextDirection -- ^ @dir@ - the new default direction. This cannot be
                  -- 'TextDirNone'.
 -> IO ()
widgetSetDefaultDirection dir =
  gtk_widget_set_default_direction
{-# LINE 1427 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    ((fromIntegral . fromEnum) dir)

-- | Obtains the current default reading direction. See
-- 'widgetSetDefaultDirection'.
--
widgetGetDefaultDirection :: IO TextDirection
widgetGetDefaultDirection =
  liftM (toEnum . fromIntegral) $
  gtk_widget_get_default_direction
{-# LINE 1436 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}


-- %hash c:c7ba d:3a9c
-- | Sets a shape for this widget's 'DrawWindow'. This allows for transparent
-- windows etc., see 'windowShapeCombineMask' for more information.
--
widgetShapeCombineMask :: WidgetClass self => self
 -> Maybe Bitmap -- ^ @shapeMask@ - shape to be added, or @Nothint@ to remove an
            -- existing shape.
 -> Int -- ^ @offsetX@ - X position of shape mask with respect to @window@.
 -> Int -- ^ @offsetY@ - Y position of shape mask with respect to @window@.
 -> IO ()
widgetShapeCombineMask self shapeMask offsetX offsetY =
  case (fromMaybe (Pixmap nullForeignPtr) shapeMask) of
    Pixmap fPtr -> withForeignPtr fPtr $ \bitmapPtr ->
      (\(Widget arg1) arg2 arg3 arg4 -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_shape_combine_mask argPtr1 arg2 arg3 arg4)
{-# LINE 1452 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
        (toWidget self)
        (castPtr bitmapPtr)
        (fromIntegral offsetX)
        (fromIntegral offsetY)




-- %hash c:3c29 d:68e2
-- | Sets an input shape for this widget's GDK window. This allows for windows
-- which react to mouse click in a nonrectangular region, see
-- 'windowInputShapeCombineMask' for more information.
--
-- * Available since Gtk+ version 2.10
--
widgetInputShapeCombineMask :: WidgetClass self => self
 -> Maybe Bitmap -- ^ @shapeMask@ - shape to be added, or @Nothint@ to remove an
            -- existing shape.
 -> Int -- ^ @offsetX@ - X position of shape mask with respect to @window@.
 -> Int -- ^ @offsetY@ - Y position of shape mask with respect to @window@.
 -> IO ()
widgetInputShapeCombineMask self shapeMask offsetX offsetY =
  case (fromMaybe (Pixmap nullForeignPtr) shapeMask) of
    Pixmap fPtr -> withForeignPtr fPtr $ \bitmapPtr ->
      (\(Widget arg1) arg2 arg3 arg4 -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_input_shape_combine_mask argPtr1 arg2 arg3 arg4)
{-# LINE 1477 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
        (toWidget self)
        (castPtr bitmapPtr)
        (fromIntegral offsetX)
        (fromIntegral offsetY)
{-# LINE 1511 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
-- | Create a 'Pixmap' of the contents of the widget and its children.
--
-- Works even if the widget is obscured. The depth and visual of the resulting pixmap is dependent on
-- the widget being snapshot and likely differs from those of a target widget displaying the
-- pixmap. The function 'pixbufGetFromDrawable' can be used to convert the pixmap to a visual
-- independant representation.
--
-- The snapshot area used by this function is the widget's allocation plus any extra space occupied by
-- additional windows belonging to this widget (such as the arrows of a spin button). Thus, the
-- resulting snapshot pixmap is possibly larger than the allocation.
--
-- The resulting pixmap is shrunken to match the specified @clipRect@. The
-- (x,y) coordinates of @clipRect@ are interpreted widget relative. If width or height of @clipRect@ are
-- 0 or negative, the width or height of the resulting pixmap will be shrunken by the respective
-- amount. For instance a @clipRect@ { +5, +5, -10, -10 } will chop off 5 pixels at each side of the
-- snapshot pixmap. @clipRect@ will contain the exact widget-relative snapshot coordinates
-- upon return. A @clipRect@ of { -1, -1, 0, 0 } can be used to preserve the auto-grown snapshot area
-- and use @clipRect@ as a pure output parameter.
--
-- The returned pixmap can be 'Nothing', if the resulting @clipArea@ was empty.
widgetGetSnapshot :: WidgetClass self => self
                  -> Rectangle
                  -> IO (Maybe Pixmap) -- ^ returns 'Pixmap' snapshot of the widget
widgetGetSnapshot widget clipRect =
  maybeNull (wrapNewGObject mkPixmap) $
  with clipRect $ \ clipRectPtr ->
  (\(Widget arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_get_snapshot argPtr1 arg2)
{-# LINE 1538 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
     (toWidget widget)
     (castPtr clipRectPtr)



-- %hash c:7e36 d:616f
-- | Obtains the full path to @widget@. The path is simply the name of a
-- widget and all its parents in the container hierarchy, separated by periods.
-- The name of a widget comes from 'widgetGetName'. Paths are used to apply
-- styles to a widget in gtkrc configuration files. Widget names are the type
-- of the widget by default (e.g. \"GtkButton\") or can be set to an
-- application-specific value with 'widgetSetName'. By setting the name of a
-- widget, you allow users or theme authors to apply styles to that specific
-- widget in their gtkrc file. Also returns the path in reverse
-- order, i.e. starting with the widget's name instead of starting with the
-- name of the widget's outermost ancestor.
--
widgetPath :: (WidgetClass self, GlibString string) => self
 -> IO (Int, string, string) -- ^ @(pathLength, path, pathReversed)@ - length
                             -- of the path, path string and reverse path
                             -- string
widgetPath self =
  alloca $ \pathLengthPtr ->
  alloca $ \pathPtr ->
  alloca $ \pathReversedPtr ->
  (\(Widget arg1) arg2 arg3 arg4 -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_path argPtr1 arg2 arg3 arg4)
{-# LINE 1564 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
    pathLengthPtr
    pathPtr
    pathReversedPtr
  >>
  peek pathLengthPtr >>= \pathLength ->
  peek pathPtr >>= readUTFString >>= \path ->
  peek pathReversedPtr >>= readUTFString >>= \pathReversed ->
  return (fromIntegral pathLength, path, pathReversed)

-- %hash c:d4a6
-- | Same as 'widgetPath', but always uses the name of a widget's type, never
-- uses a custom name set with 'widgetSetName'.
--
widgetClassPath :: (WidgetClass self, GlibString string) => self
 -> IO (Int, string, string) -- ^ @(pathLength, path, pathReversed)@ - length
                             -- of the path, path string and reverse path
                             -- string
widgetClassPath self =
  alloca $ \pathLengthPtr ->
  alloca $ \pathPtr ->
  alloca $ \pathReversedPtr ->
  (\(Widget arg1) arg2 arg3 arg4 -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_class_path argPtr1 arg2 arg3 arg4)
{-# LINE 1587 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
    pathLengthPtr
    pathPtr
    pathReversedPtr
  >>
  peek pathLengthPtr >>= \pathLength ->
  peek pathPtr >>= readUTFString >>= \path ->
  peek pathReversedPtr >>= readUTFString >>= \pathReversed ->
  return (fromIntegral pathLength, path, pathReversed)

-- %hash c:769e
-- | Obtains the composite name of a widget.
--
widgetGetCompositeName :: (WidgetClass self, GlibString string) => self
 -> IO (Maybe string) -- ^ returns the composite name of @widget@, or
                      -- @Nothing@ if @widget@ is not a composite child.
widgetGetCompositeName self =
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_get_composite_name argPtr1)
{-# LINE 1605 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
  >>= maybePeek peekUTFString
{-# LINE 1707 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
-- | Modifies style values on the widget. Modifications made using this
-- technique take precedence over style values set via an RC file, however,
-- they will be overriden if a style is explicitely set on the widget using
-- 'widgetSetStyle'. The 'RcStyle' structure is designed so each field can
-- either be set or unset, so it is possible, using this function, to modify
-- some style values and leave the others unchanged.
--
-- Note that modifications made with this function are not cumulative with
-- previous calls to 'widgetModifyStyle' or with such functions as
-- 'widgetModifyFg'. If you wish to retain previous values, you must first call
-- 'widgetGetModifierStyle', make your modifications to the returned style,
-- then call 'widgetModifyStyle' with that style. On the other hand, if you
-- first call 'widgetModifyStyle', subsequent calls to such functions
-- 'widgetModifyFg' will have a cumulative effect with the initial
-- modifications.
--
widgetModifyStyle :: (WidgetClass self, RcStyleClass style) => self
 -> style -- ^ @style@ - the 'RcStyle' holding the style modifications
 -> IO ()
widgetModifyStyle self style =
  (\(Widget arg1) (RcStyle arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_widget_modify_style argPtr1 argPtr2)
{-# LINE 1728 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
    (toRcStyle style)

-- | Returns the current modifier style for the widget. (As set by
-- 'widgetModifyStyle'.) If no style has previously set, a new 'RcStyle' will
-- be created with all values unset, and set as the modifier style for the
-- widget. If you make changes to this rc style, you must call
-- 'widgetModifyStyle', passing in the returned rc style, to make sure that
-- your changes take effect.
--
-- Caution: passing the style back to 'widgetModifyStyle' will normally end
-- up destroying it, because 'widgetModifyStyle' copies the passed-in style and
-- sets the copy as the new modifier style, thus dropping any reference to the
-- old modifier styl e. Add a reference to the modifier style if you want to
-- keep it alive.
--
widgetGetModifierStyle :: WidgetClass self => self -> IO RcStyle
widgetGetModifierStyle self =
  makeNewGObject mkRcStyle $
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_get_modifier_style argPtr1)
{-# LINE 1748 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)

-- %hash c:5550
-- | Sets the foreground color for a widget in a particular state. All other
-- style values are left untouched. See also 'widgetModifyStyle'.
--
widgetModifyFg :: WidgetClass self => self
 -> StateType -- ^ @state@ - the state for which to set the foreground color.
 -> Color -- ^ @color@ - the color to assign (does not need to be
              -- allocated)
 -> IO ()
widgetModifyFg self state color =
  with color $ \colorPtr ->
  (\(Widget arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_modify_fg argPtr1 arg2 arg3)
{-# LINE 1762 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
    ((fromIntegral . fromEnum) state)
    (castPtr colorPtr)

-- | Restores the foreground color for a widget in a particular state. This
-- undoes the effects of previous calls to `widgetModifyFg'.
--
widgetRestoreFg :: WidgetClass self => self
 -> StateType -- ^ @state@ - the state for which to restore the foreground color.
 -> IO ()
widgetRestoreFg self state =
  (\(Widget arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_modify_fg argPtr1 arg2 arg3)
{-# LINE 1774 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
    ((fromIntegral . fromEnum) state)
    nullPtr

-- %hash c:2c5
-- | Sets the background color for a widget in a particular state. All other
-- style values are left untouched. See also 'widgetModifyStyle'.
--
-- Note that \"no window\" widgets (which have the 'NoWindow' flag set) draw
-- on their parent container's window and thus may not draw any background
-- themselves. This is the case for e.g. 'Label'. To modify the background of
-- such widgets, you have to set the background color on their parent; if you
-- want to set the background of a rectangular area around a label, try placing
-- the label in a 'EventBox' widget and setting the background color on that.
--
widgetModifyBg :: WidgetClass self => self
 -> StateType -- ^ @state@ - the state for which to set the background color.
 -> Color -- ^ @color@ - the color to assign (does not need to be
              -- allocated).
 -> IO ()
widgetModifyBg self state color =
  with color $ \colorPtr ->
  (\(Widget arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_modify_bg argPtr1 arg2 arg3)
{-# LINE 1797 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
    ((fromIntegral . fromEnum) state)
    (castPtr colorPtr)

-- | Restores the background color for a widget in a particular state. This
-- undoes the effects of previous calls to `widgetModifyBg'.
--
widgetRestoreBg :: WidgetClass self => self
 -> StateType -- ^ @state@ - the state for which to restore the background color.
 -> IO ()
widgetRestoreBg self state =
  (\(Widget arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_modify_bg argPtr1 arg2 arg3)
{-# LINE 1809 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
    ((fromIntegral . fromEnum) state)
    nullPtr

-- %hash c:d2ba
-- | Sets the text color for a widget in a particular state. All other style
-- values are left untouched. The text color is the foreground color used along
-- with the base color (see 'widgetModifyBase') for widgets such as 'Entry' and
-- 'TextView'. See also 'widgetModifyStyle'.
--
widgetModifyText :: WidgetClass self => self
 -> StateType -- ^ @state@ - the state for which to set the text color.
 -> Color -- ^ @color@ - the color to assign (does not need to be
              -- allocated).
 -> IO ()
widgetModifyText self state color =
  with color $ \colorPtr ->
  (\(Widget arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_modify_text argPtr1 arg2 arg3)
{-# LINE 1827 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
    ((fromIntegral . fromEnum) state)
    (castPtr colorPtr)

-- | Restores the text color for a widget in a particular state. This
-- undoes the effects of previous calls to `widgetModifyText'.
--
widgetRestoreText :: WidgetClass self => self
 -> StateType -- ^ @state@ - the state for which to restore the text color.
 -> IO ()
widgetRestoreText self state =
  (\(Widget arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_modify_text argPtr1 arg2 arg3)
{-# LINE 1839 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
    ((fromIntegral . fromEnum) state)
    nullPtr

-- %hash c:ac08
-- | Sets the base color for a widget in a particular state. All other style
-- values are left untouched. The base color is the background color used along
-- with the text color (see 'widgetModifyText') for widgets such as 'Entry' and
-- 'TextView'. See also 'widgetModifyStyle'.
--
-- Note that \"no window\" widgets (which have the 'NoWindow' flag set) draw
-- on their parent container's window and thus may not draw any background
-- themselves. This is the case for e.g. 'Label'. To modify the background of
-- such widgets, you have to set the base color on their parent; if you want to
-- set the background of a rectangular area around a label, try placing the
-- label in a 'EventBox' widget and setting the base color on that.
--
widgetModifyBase :: WidgetClass self => self
 -> StateType -- ^ @state@ - the state for which to set the base color.
 -> Color -- ^ @color@ - the color to assign (does not need to be
              -- allocated).
 -> IO ()
widgetModifyBase self state color =
  with color $ \colorPtr ->
  (\(Widget arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_modify_base argPtr1 arg2 arg3)
{-# LINE 1864 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
    ((fromIntegral . fromEnum) state)
    (castPtr colorPtr)

-- | Restores the base color for a widget in a particular state. This undoes
-- the effects of previous calls to widgetModifyBase.
--
widgetRestoreBase :: WidgetClass self => self
 -> StateType -- ^ @state@ - the state for which to restore the base color.
 -> IO ()
widgetRestoreBase self state =
  (\(Widget arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_modify_base argPtr1 arg2 arg3)
{-# LINE 1876 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
    ((fromIntegral . fromEnum) state)
    nullPtr


-- %hash c:38d7
-- | Sets the font to use for a widget. All other style values are left
-- untouched. See also 'widgetModifyStyle'.
--
widgetModifyFont :: WidgetClass self => self
 -> Maybe FontDescription -- ^ @fontDesc@ - the font description to use, or
                          -- @Nothing@ to undo the effect of previous calls to
                          -- 'widgetModifyFont'.
 -> IO ()
widgetModifyFont self fontDesc =
  (\(Widget arg1) (FontDescription arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_widget_modify_font argPtr1 argPtr2)
{-# LINE 1892 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
    (fromMaybe (FontDescription nullForeignPtr) fontDesc)

-- | Creates a new 'PangoContext' with the appropriate colormap, font description,
-- and base direction for drawing text for this widget. See also
-- 'widgetGetPangoContext'.
--
widgetCreatePangoContext :: WidgetClass self => self
 -> IO PangoContext -- ^ returns the new 'PangoContext'
widgetCreatePangoContext self =
  wrapNewGObject mkPangoContext $
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_create_pango_context argPtr1)
{-# LINE 1904 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)

-- | Gets a 'PangoContext' with the appropriate font description and base
-- direction for this widget. Unlike the context returned by
-- 'widgetCreatePangoContext', this context is owned by the widget (it can be
-- used until the screen for the widget changes or the widget is removed from
-- its toplevel), and will be updated to match any changes to the widget's
-- attributes.
--
-- If you create and keep a 'PangoLayout' using this context, you must deal
-- with changes to the context by calling
-- 'layoutContextChanged' on the layout
-- in response to the 'onStyleChanged' and 'onDirectionChanged' signals for the
-- widget.
--
widgetGetPangoContext :: WidgetClass self => self
 -> IO PangoContext -- ^ returns the 'PangoContext' for the widget.
widgetGetPangoContext self =
  makeNewGObject mkPangoContext $
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_get_pango_context argPtr1)
{-# LINE 1924 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)

-- | Prepare text for display.
--
-- The 'PangoLayout' represents the rendered text. It can be shown on screen
-- by calling 'drawLayout'.
--
-- The returned 'PangoLayout' shares the same font information ('PangoContext') as this
-- widget. If this information changes, the 'PangoLayout' should change. The
-- following code ensures that the displayed text always reflects the widget's
-- settings:
--
-- > l <- widgetCreateLayout w "My Text."
-- > let update = do
-- > layoutContextChanged l
-- > -- update the Drawables which show this layout
-- > w `onDirectionChanged` update
-- > w `onStyleChanged` update
--
widgetCreateLayout :: (WidgetClass self, GlibString string) => self
 -> string -- ^ @text@ - text to set on the layout
 -> IO PangoLayout
widgetCreateLayout self text = do
  pl <- wrapNewGObject mkPangoLayoutRaw $
    withUTFString text $ \textPtr ->
    (\(Widget arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_create_pango_layout argPtr1 arg2)
{-# LINE 1950 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
      (toWidget self)
      textPtr
  ps <- makeNewPangoString text
  psRef <- newIORef ps
  return (PangoLayout psRef pl)

-- %hash c:cee d:1d29
-- | A convenience function that uses the theme engine and RC file settings
-- for @widget@ to look up the stock icon and render it to a
-- 'Graphics.UI.Gtk.Gdk.Pixbuf.Pixbuf'.
-- The icon should be one of the stock id constants such as
-- 'Graphics.UI.Gtk.General.StockItems.stockOpen'. @size@ should be a
-- size such as 'Graphics.UI.Gtk.General.IconFactory.IconSizeMenu'.
-- @detail@ should be a string that identifies the
-- widget or code doing the rendering, so that theme engines can special-case
-- rendering for that widget or code.
--
-- The pixels in the returned 'Graphics.UI.Gtk.Gdk.Pixbuf.Pixbuf' are
-- shared with the rest of the
-- application and should not be modified.
--
widgetRenderIcon :: (WidgetClass self, GlibString string) => self
 -> string -- ^ @stockId@ - a stock ID
 -> IconSize -- ^ @size@ - a stock size
 -> string -- ^ @detail@ - render detail to pass to theme engine
 -> IO (Maybe Pixbuf) -- ^ returns a new pixbuf, or @Nothing@ if the stock ID
                      -- wasn't known
widgetRenderIcon self stockId size detail =
  maybeNull (wrapNewGObject mkPixbuf) $
  withUTFString detail $ \detailPtr ->
  withUTFString stockId $ \stockIdPtr ->
  (\(Widget arg1) arg2 arg3 arg4 -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_render_icon argPtr1 arg2 arg3 arg4)
{-# LINE 1982 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
    stockIdPtr
    ((fromIntegral . fromEnum) size)
    detailPtr

-- %hash c:62f d:1863
-- | Invalidates the rectangular area of @widget@ defined by @x@, @y@, @width@
-- and @height@ by calling
-- 'Graphics.UI.Gtk.Gdk.DrawWindow.drawWindowInvalidateRect' on the widget's
-- 'Graphics.UI.Gtk.Gdk.DrawWindow.DrawWindow' and all its child windows. Once
-- the main loop becomes idle (after the current batch of events has been
-- processed, roughly), the window will receive expose events for the union of
-- all regions that have been invalidated.
--
-- Normally you would only use this function in widget implementations. In
-- particular, you might use it, or
-- 'Graphics.UI.Gtk.Gdk.DrawWindow.drawWindowInvalidateRect' directly, to
-- schedule a redraw of a 'Graphics.UI.Gtk.Gdk.DrawWindow.DrawingArea' or some
-- portion thereof.
--
-- Frequently you can just call
-- 'Graphics.UI.Gtk.Gdk.DrawWindow.windowInvalidateRect' or
-- 'Graphics.UI.Gtk.Gdk.DrawWindow.windowInvalidateRegion' instead of this
-- function. Those functions will invalidate only a single window, instead of
-- the widget and all its children.
--
-- The advantage of adding to the invalidated region compared to simply
-- drawing immediately is efficiency; using an invalid region ensures that you
-- only have to redraw one time.
--
widgetQueueDrawArea :: WidgetClass self => self
 -> Int -- ^ @x@ - x coordinate of upper-left corner of rectangle to redraw
 -> Int -- ^ @y@ - y coordinate of upper-left corner of rectangle to redraw
 -> Int -- ^ @width@ - width of region to draw
 -> Int -- ^ @height@ - height of region to draw
 -> IO ()
widgetQueueDrawArea self x y width height =
  (\(Widget arg1) arg2 arg3 arg4 arg5 -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_queue_draw_area argPtr1 arg2 arg3 arg4 arg5)
{-# LINE 2020 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
    (fromIntegral x)
    (fromIntegral y)
    (fromIntegral width)
    (fromIntegral height)
{-# LINE 2047 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
-- %hash c:5ffb d:3e1a
-- | Recursively resets the shape on this widget and its descendants.
--
widgetResetShapes :: WidgetClass self => self -> IO ()
widgetResetShapes self =
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_reset_shapes argPtr1)
{-# LINE 2053 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)


-- | Sets whether the application intends to draw on the widget in response
-- to an 'onExpose' signal.
--
-- * This is a hint to the widget and does not affect the behavior of the
-- GTK+ core; many widgets ignore this flag entirely. For widgets that do
-- pay attention to the flag, such as 'EventBox' and 'Window', the effect
-- is to suppress default themed drawing of the widget's background.
-- (Children of the widget will still be drawn.) The application is then
-- entirely responsible for drawing the widget background.
--
widgetSetAppPaintable :: WidgetClass self => self
 -> Bool -- ^ @appPaintable@ - @True@ if the application will paint on the
          -- widget
 -> IO ()
widgetSetAppPaintable self appPaintable =
  (\(Widget arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_set_app_paintable argPtr1 arg2)
{-# LINE 2072 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
    (fromBool appPaintable)

-- %hash c:89b2 d:e14d
-- | Widgets are double buffered by default; you can use this function to turn
-- off the buffering. \"Double buffered\" simply means that
-- 'Graphics.UI.Gtk.Gdk.DrawWindow.drawWindowBeginPaintRegion' and
-- 'Graphics.UI.Gtk.Gdk.DrawWindow.drawWindowEndPaint' are called automatically
-- around expose events sent to the widget.
-- 'Graphics.UI.Gtk.Gdk.DrawWindow.drawWindowBeginPaintRegion' diverts all
-- drawing to a widget's window to an offscreen buffer, and
-- 'Graphics.UI.Gtk.Gdk.DrawWindow.drawWindowEndPaint'
-- draws the buffer to the screen. The result is that users see the window
-- update in one smooth step, and don't see individual graphics primitives
-- being rendered.
--
-- In very simple terms, double buffered widgets don't flicker, so you would
-- only use this function to turn off double buffering if you had special needs
-- and really knew what you were doing.
--
-- Note: if you turn off double-buffering, you have to handle expose events,
-- since even the clearing to the background color or pixmap will not happen
-- automatically (as it is done in
-- 'Graphics.UI.Gtk.Gdk.DrawWindow.drawWindowBeginPaint').
--
widgetSetDoubleBuffered :: WidgetClass self => self
 -> Bool -- ^ @doubleBuffered@ - @True@ to double-buffer a widget
 -> IO ()
widgetSetDoubleBuffered self doubleBuffered =
  (\(Widget arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_set_double_buffered argPtr1 arg2)
{-# LINE 2102 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
    (fromBool doubleBuffered)

-- %hash c:d61 d:ac24
-- | Sets whether the entire widget is queued for drawing when its size
-- allocation changes. By default, this setting is @True@ and the entire widget
-- is redrawn on every size change. If your widget leaves the upper left
-- unchanged when made bigger, turning this setting on will improve
-- performance.
--
-- Note that for \"no window\" widgets setting this flag to @False@ turns off
-- all allocation on resizing: the widget will not even redraw if its position
-- changes; this is to allow containers that don't draw anything to avoid
-- excess invalidations. If you set this flag on a \"no window\" widget that
-- /does/ draw its window, you are responsible for invalidating both
-- the old and new allocation of the widget when the widget is moved and
-- responsible for invalidating regions newly when the widget increases size.
--
widgetSetRedrawOnAllocate :: WidgetClass self => self
 -> Bool -- ^ @redrawOnAllocate@ - if @True@, the entire widget will be
          -- redrawn when it is allocated to a new size. Otherwise, only the
          -- new portion of the widget will be redrawn.
 -> IO ()
widgetSetRedrawOnAllocate self redrawOnAllocate =
  (\(Widget arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_set_redraw_on_allocate argPtr1 arg2)
{-# LINE 2127 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
    (fromBool redrawOnAllocate)

-- | Sets a widgets composite name. A child widget of a container is
-- composite if it serves as an internal widget and, thus, is not
-- added by the user.
--
widgetSetCompositeName :: (WidgetClass self, GlibString string) => self
 -> string -- ^ @name@ - the name to set.
 -> IO ()
widgetSetCompositeName self name =
  withUTFString name $ \namePtr ->
  (\(Widget arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_set_composite_name argPtr1 arg2)
{-# LINE 2140 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
    namePtr

-- | Emits the “mnemonic-activate” signal.
--
-- The default handler for this signal activates the widget if groupCycling
-- is @False@, and just grabs the focus if @groupCycling@ is @True@.
widgetMnemonicActivate :: WidgetClass self => self
 -> Bool
 -> IO Bool
widgetMnemonicActivate self groupCycling =
  liftM toBool $
  (\(Widget arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_mnemonic_activate argPtr1 arg2)
{-# LINE 2153 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
    (fromBool groupCycling)


-- %hash c:5c58 d:6895
-- | For widgets that support scrolling, sets the scroll adjustments and
-- returns @True@. For widgets that don't support scrolling, does nothing and
-- returns @False@. Widgets that don't support scrolling can be scrolled by
-- placing them in a 'Viewport', which does support scrolling.
--
-- Removed in Gtk3.
widgetSetScrollAdjustments :: WidgetClass self => self
 -> Maybe Adjustment -- ^ @hadjustment@ - an adjustment for horizontal scrolling, or
               -- @Nothing@
 -> Maybe Adjustment -- ^ @vadjustment@ - an adjustment for vertical scrolling, or
               -- @Nothing@
 -> IO Bool -- ^ returns @True@ if the widget supports scrolling
widgetSetScrollAdjustments self hadjustment vadjustment =
  liftM toBool $
  (\(Widget arg1) (Adjustment arg2) (Adjustment arg3) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->withForeignPtr arg3 $ \argPtr3 ->gtk_widget_set_scroll_adjustments argPtr1 argPtr2 argPtr3)
{-# LINE 2173 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
    (fromMaybe (Adjustment nullForeignPtr) hadjustment)
    (fromMaybe (Adjustment nullForeignPtr) vadjustment)




-- | Computes the intersection of a widget's area and @region@, returning
-- the intersection. The result may be empty, use
-- 'Graphics.UI.Gtk.Gdk.Region.regionEmpty' to check.
--
widgetRegionIntersect :: WidgetClass self => self
 -> Region -- ^ @region@ - a 'Region' in the same coordinate system as the
              -- widget's allocation. That is, relative to the widget's
              -- 'DrawWindow' for 'NoWindow' widgets; relative to the parent
              -- 'DrawWindow' of the widget's 'DrawWindow' for widgets with
              -- their own 'DrawWindow'.
 -> IO Region -- ^ returns A region holding the intersection of the widget and
              -- @region@. The coordinates of the return value are relative to
              -- the widget's 'DrawWindow', if it has one, otherwise
              -- it is relative to the parent's 'DrawWindow'.
widgetRegionIntersect self region = do
  intersectionPtr <- (\(Widget arg1) (Region arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_widget_region_intersect argPtr1 argPtr2)
{-# LINE 2196 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
    region
  makeNewRegion intersectionPtr


-- %hash c:3c94 d:cdb6
-- | Returns the accessible object that describes the widget to an assistive
-- technology.
--
-- If no accessibility library is loaded (i.e. no ATK implementation library
-- is loaded via GTK_MODULES or via another application library, such as
-- libgnome), then this 'Object' instance may be a no-op. Likewise, if no
-- class-specific 'Object' implementation is available for the widget instance
-- in question, it will inherit an 'Object' implementation from the first
-- ancestor class for which such an implementation is defined.
--
-- The documentation of the ATK library contains more information about
-- accessible objects and their uses.
--
-- Returns a GObject in Gtk3.
widgetGetAccessible :: WidgetClass self => self

 -> IO Object -- ^ returns the 'Object' associated with @widget@



widgetGetAccessible self =

  makeNewGObject mkObject $



  liftM castPtr $
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_get_accessible argPtr1)
{-# LINE 2230 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)

-- %hash c:713d d:c4fc
-- | This function is used by custom widget implementations; if you\'re
-- writing an app, you\'d use 'widgetGrabFocus' to move the focus to a
-- particular widget, and 'containerSetFocusChain' to change the focus tab
-- order. So you may want to investigate those functions instead.
--
-- The \"focus\" default handler for a widget should return @True@ if moving
-- in @direction@ left the focus on a focusable location inside that widget,
-- and @False@ if moving in @direction@ moved the focus outside the widget. If
-- returning @True@, widgets normally call 'widgetGrabFocus' to place the focus
-- accordingly; if returning @False@, they don't modify the current focus
-- location.
--
widgetChildFocus :: WidgetClass self => self
 -> DirectionType -- ^ @direction@ - direction of focus movement
 -> IO Bool -- ^ returns @True@ if focus ended up inside @widget@
widgetChildFocus self direction =
  liftM toBool $
  (\(Widget arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_child_focus argPtr1 arg2)
{-# LINE 2251 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
    ((fromIntegral . fromEnum) direction)

-- %hash c:de20 d:5300
-- | Gets the value set with 'widgetSetChildVisible'. If you feel a need to
-- use this function, your code probably needs reorganization.
--
-- This function is only useful for container implementations and never
-- should be called by an application.
--
widgetGetChildVisible :: WidgetClass self => self
 -> IO Bool -- ^ returns @True@ if the widget is mapped with the parent.
widgetGetChildVisible self =
  liftM toBool $
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_get_child_visible argPtr1)
{-# LINE 2266 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)

-- %hash c:9320 d:367
-- | Returns the parent container of @widget@.
--
-- * Returns the parent container of @widget@ if it has one.
--
widgetGetParent :: WidgetClass self => self
 -> IO (Maybe Widget)
widgetGetParent self = do
  parentPtr <- (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_get_parent argPtr1) (toWidget self)
  if parentPtr==nullPtr then return Nothing else
    liftM Just $ makeNewObject mkWidget (return parentPtr)

-- %hash c:85e3 d:a962
-- | Gets the settings object holding the settings (global property settings,
-- RC file information, etc) used for this widget.
--
-- Note that this function can only be called when the 'Widget' is attached
-- to a toplevel, since the settings object is specific to a particular
-- 'Screen'.
--
widgetGetSettings :: WidgetClass self => self
 -> IO Settings -- ^ returns the relevant 'Settings' object
widgetGetSettings self =
  makeNewGObject mkSettings $
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_get_settings argPtr1)
{-# LINE 2293 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)



-- | Returns the clipboard object for the given selection to
-- be used with widget. widget must have a 'Display'
-- associated with it, so must be attached to a toplevel
-- window.
widgetGetClipboard :: WidgetClass self => self
                   -> SelectionTag -- ^ @selection@ a 'Atom' which identifies the clipboard
                                       -- to use. 'selectionClipboard' gives the
                                       -- default clipboard. Another common value
                                       -- is 'selectionPrimary', which gives
                                       -- the primary X selection.
                   -> IO Clipboard -- ^ returns the appropriate clipboard object. If no
                                   -- clipboard already exists, a new one will
                                   -- be created.
widgetGetClipboard self (Atom tagPtr) =
  makeNewGObject mkClipboard $
  (\(Widget arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_get_clipboard argPtr1 arg2)
{-# LINE 2313 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
    tagPtr

-- %hash c:45ed d:52ef
-- | Get the 'Display' for the toplevel window associated with this widget.
-- This function can only be called after the widget has been added to a widget
-- hierarchy with a 'Window' at the top.
--
-- In general, you should only create display specific resources when a
-- widget has been realized, and you should free those resources when the
-- widget is unrealized.
--
-- * Available since Gtk+ version 2.2
--
widgetGetDisplay :: WidgetClass self => self
 -> IO Display -- ^ returns the 'Display' for the toplevel for this widget.
widgetGetDisplay self =
  makeNewGObject mkDisplay $
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_get_display argPtr1)
{-# LINE 2332 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)

-- %hash c:8e4e d:252b
-- | Get the root window where this widget is located. This function can only
-- be called after the widget has been added to a widget heirarchy with
-- 'Window' at the top.
--
-- The root window is useful for such purposes as creating a popup
-- 'DrawWindow' associated with the window. In general, you should only create
-- display specific resources when a widget has been realized, and you should
-- free those resources when the widget is unrealized.
--
-- * Available since Gtk+ version 2.2
--
widgetGetRootWindow :: WidgetClass self => self
 -> IO DrawWindow -- ^ returns the 'DrawWindow' root window for the toplevel
                  -- for this widget.
widgetGetRootWindow self =
  makeNewGObject mkDrawWindow $
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_get_root_window argPtr1)
{-# LINE 2352 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)

-- %hash c:b929 d:67f0
-- | Get the 'Screen' from the toplevel window associated with this widget.
-- This function can only be called after the widget has been added to a widget
-- hierarchy with a 'Window' at the top.
--
-- In general, you should only create screen specific resources when a
-- widget has been realized, and you should free those resources when the
-- widget is unrealized.
--
-- * Available since Gtk+ version 2.2
--
widgetGetScreen :: WidgetClass self => self
 -> IO Screen -- ^ returns the 'Screen' for the toplevel for this widget.
widgetGetScreen self =
  makeNewGObject mkScreen $
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_get_screen argPtr1)
{-# LINE 2370 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)

-- %hash c:4fab d:aae2
-- | Checks whether there is a 'Screen' is associated with this widget. All
-- toplevel widgets have an associated screen, and all widgets added into a
-- heirarchy with a toplevel window at the top.
--
-- * Available since Gtk+ version 2.2
--
widgetHasScreen :: WidgetClass self => self
 -> IO Bool -- ^ returns @True@ if there is a 'Screen' associcated with the
            -- widget.
widgetHasScreen self =
  liftM toBool $
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_has_screen argPtr1)
{-# LINE 2385 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)


-- %hash c:dabc d:8275
-- | Gets the size request that was explicitly set for the widget using
-- 'widgetSetSizeRequest'. A value of -1 for @width@ or @height@
-- indicates that that dimension has not been set explicitly and the natural
-- requisition of the widget will be used intead. See 'widgetSetSizeRequest'.
-- To get the size a widget will actually use, call 'widgetSizeRequest' instead
-- of this function.
--
widgetGetSizeRequest :: WidgetClass self => self
 -> IO (Int, Int) -- ^ @(width, height)@
widgetGetSizeRequest self =
  alloca $ \widthPtr ->
  alloca $ \heightPtr -> do
  (\(Widget arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_get_size_request argPtr1 arg2 arg3)
{-# LINE 2402 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
    widthPtr
    heightPtr
  width <- peek widthPtr
  height <- peek heightPtr
  return (fromIntegral width, fromIntegral height)

-- %hash c:546d d:3c7f
-- | Sets whether @widget@ should be mapped along with its when its parent is
-- mapped and @widget@ has been shown with 'widgetShow'.
--
-- The child visibility can be set for widget before it is added to a
-- container with 'widgetSetParent', to avoid mapping children unnecessary
-- before immediately unmapping them. However it will be reset to its default
-- state of @True@ when the widget is removed from a container.
--
-- Note that changing the child visibility of a widget does not queue a
-- resize on the widget. Most of the time, the size of a widget is computed
-- from all visible children, whether or not they are mapped. If this is not
-- the case, the container can queue a resize itself.
--
-- This function is only useful for container implementations and never
-- should be called by an application.
--
widgetSetChildVisible :: WidgetClass self => self
 -> Bool -- ^ @isVisible@ - if @True@, @widget@ should be mapped along with
         -- its parent.
 -> IO ()
widgetSetChildVisible self isVisible =
  (\(Widget arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_set_child_visible argPtr1 arg2)
{-# LINE 2432 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
    (fromBool isVisible)

-- | Sets the minimum size of a widget; that is, the widget's size request
-- will be @width@ by @height@. You can use this function to force a widget to
-- be either larger or smaller than it normally would be.
--
-- In most cases, 'Graphics.UI.Gtk.Windows.Window.windowSetDefaultSize'
-- is a better choice for toplevel
-- windows than this function; setting the default size will still allow users
-- to shrink the window. Setting the size request will force them to leave the
-- window at least as large as the size request. When dealing with window
-- sizes, 'Graphics.UI.Gtk.Windows.Window.windowSetGeometryHints' can be a
-- useful function as well.
--
-- Note the inherent danger of setting any fixed size - themes, translations
-- into other languages, different fonts, and user action can all change the
-- appropriate size for a given widget. So, it's basically impossible to
-- hardcode a size that will always be correct.
--
-- The size request of a widget is the smallest size a widget can accept
-- while still functioning well and drawing itself correctly. However in some
-- strange cases a widget may be allocated less than its requested size, and in
-- many cases a widget may be allocated more space than it requested.
--
-- If the size request in a given direction is -1 (unset), then the
-- \"natural\" size request of the widget will be used instead.
--
-- Widgets can't actually be allocated a size less than 1 by 1, but you can
-- pass 0,0 to this function to mean \"as small as possible.\"
--
widgetSetSizeRequest :: WidgetClass self => self
 -> Int -- ^ @width@ - width @widget@ should request, or -1 to unset
 -> Int -- ^ @height@ - height @widget@ should request, or -1 to unset
 -> IO ()
widgetSetSizeRequest self width height =
  (\(Widget arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_set_size_request argPtr1 arg2 arg3)
{-# LINE 2469 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
    (fromIntegral width)
    (fromIntegral height)


-- %hash c:83c3 d:e6f1
-- | Sets the 'noShowAll' property, which determines whether calls to
-- 'widgetShowAll' and 'widgetHideAll' will affect this widget.
--
-- This is mostly for use in constructing widget hierarchies with externally
-- controlled visibility, see 'UIManager'.
--
-- * Available since Gtk+ version 2.4
--
widgetSetNoShowAll :: WidgetClass self => self
 -> Bool -- ^ @noShowAll@ - the new value for the 'noShowAll' property
 -> IO ()
widgetSetNoShowAll self noShowAll =
  (\(Widget arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_set_no_show_all argPtr1 arg2)
{-# LINE 2488 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
    (fromBool noShowAll)

-- %hash c:218d d:e07e
-- | Returns the current value of the 'noShowAll' property, which
-- determines whether calls to 'widgetShowAll' and 'widgetHideAll' will affect
-- this widget.
--
-- * Available since Gtk+ version 2.4
--
widgetGetNoShowAll :: WidgetClass self => self
 -> IO Bool -- ^ returns the current value of the \"no_show_all\" property.
widgetGetNoShowAll self =
  liftM toBool $
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_get_no_show_all argPtr1)
{-# LINE 2503 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)

-- %hash c:205b d:c518
-- | Returns a list of the widgets, normally labels, for which
-- this widget is a the target of a mnemonic (see for example,
-- 'labelSetMnemonicWidget').
--
-- * Available since Gtk+ version 2.4
--
widgetListMnemonicLabels :: WidgetClass self => self
 -> IO [Widget] -- ^ returns the list of mnemonic labels
widgetListMnemonicLabels self =
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_list_mnemonic_labels argPtr1)
{-# LINE 2516 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
  >>= fromGList
  >>= mapM (makeNewGObject mkWidget . return)

-- %hash c:eb76 d:28a2
-- | Adds a widget to the list of mnemonic labels for this widget. (See
-- 'widgetListMnemonicLabels'). Note the list of mnemonic labels for the widget
-- is cleared when the widget is destroyed, so the caller must make sure to
-- update its internal state at this point as well, by using a connection to
-- the 'destroy' signal or a weak notifier.
--
-- * Available since Gtk+ version 2.4
--
widgetAddMnemonicLabel :: (WidgetClass self, WidgetClass label) => self
 -> label -- ^ @label@ - a 'Widget' that acts as a mnemonic label for
          -- @widget@.
 -> IO ()
widgetAddMnemonicLabel self label =
  (\(Widget arg1) (Widget arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_widget_add_mnemonic_label argPtr1 argPtr2)
{-# LINE 2535 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
    (toWidget label)

-- %hash c:7831 d:d10b
-- | Removes a widget from the list of mnemonic labels for this widget. (See
-- 'widgetListMnemonicLabels'). The widget must have previously been added to
-- the list with 'widgetAddMnemonicLabel'.
--
-- * Available since Gtk+ version 2.4
--
widgetRemoveMnemonicLabel :: (WidgetClass self, WidgetClass label) => self
 -> label -- ^ @label@ - a 'Widget' that was previously set as a mnemnic label
          -- for @widget@ with 'widgetAddMnemonicLabel'.
 -> IO ()
widgetRemoveMnemonicLabel self label =
  (\(Widget arg1) (Widget arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_widget_remove_mnemonic_label argPtr1 argPtr2)
{-# LINE 2551 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
    (toWidget label)



-- %hash c:5c70 d:cbf9
-- | Returns the 'Action' that @widget@ is a proxy for. See also
-- 'actionGetProxies'.
--
-- * Available since Gtk+ version 2.10
--
-- Removed in Gtk3.
widgetGetAction :: WidgetClass self => self
 -> IO (Maybe Action)
   -- ^ returns the action that a widget is a proxy for, or
   -- @Nothing@, if it is not attached to an action.
widgetGetAction self = do
  ptr <- (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_get_action argPtr1) (toWidget self)
  if ptr==nullPtr then return Nothing else liftM Just $
    makeNewGObject mkAction (return ptr)


-- %hash c:7ea0 d:2560
-- | Whether @widget@ can rely on having its alpha channel drawn correctly. On
-- X11 this function returns whether a compositing manager is running for
-- @widget@'s screen
--
-- * Available since Gtk+ version 2.10
--
widgetIsComposited :: WidgetClass self => self
 -> IO Bool -- ^ returns @True@ if the widget can rely on its alpha channel
            -- being drawn correctly.
widgetIsComposited self =
  liftM toBool $
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_is_composited argPtr1)
{-# LINE 2586 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)




-- | Notifies the user about an input-related error on this widget.
-- If the "gtk-error-bell" setting is @True@, it calls 'drawWindowBeep',
-- otherwise it does nothing.
--
-- Note that the effect of 'drawWindow_beep' can be configured in many
-- ways, depending on the windowing backend and the desktop environment
-- or window manager that is used.
widgetErrorBell :: WidgetClass self => self
 -> IO ()
widgetErrorBell self =
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_error_bell argPtr1)
{-# LINE 2602 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)

-- | This function should be called whenever keyboard navigation within
-- a single widget hits a boundary. The function emits the "keynav-failed"
-- signal on the widget and its return value should be interpreted in a
-- way similar to the return value of 'widgetChildFocus':
--
-- When @True@ is returned, stay in the widget, the failed keyboard
-- navigation is Ok and/or there is nowhere we can/should move the
-- focus to.
--
-- When @False@ is returned, the caller should continue with keyboard
-- navigation outside the widget, e.g. by calling 'widgetChildFocus' on
-- the widget’s toplevel.
--
-- The default ::keynav-failed handler returns @True@ for 'DirTabForward'
-- and 'DirTabBackward'. For the other values of 'DirectionType' it
-- returns @False@.
--
-- Whenever the default handler returns @True@, it also calls
-- 'widgetErrorBell' to notify the user of the failed keyboard
-- navigation.
--
-- A use case for providing an own implementation of ::keynav-failed
-- (either by connecting to it or by overriding it) would be a row of
-- 'Entry' widgets where the user should be able to navigate the entire
-- row with the cursor keys, as e.g. known from user interfaces that
-- require entering license keys.
widgetKeynavFailed :: WidgetClass self => self
 -> DirectionType -- ^ @direction@ - direction of focus movement
 -> IO Bool -- ^ returns @True@ if stopping keyboard navigation is
                  -- fine, @False@ if the emitting widget should try to handle
                  -- the keyboard navigation attempt in its parent container(s).
widgetKeynavFailed self direction =
  liftM toBool $
  (\(Widget arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_keynav_failed argPtr1 arg2)
{-# LINE 2638 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
    ((fromIntegral . fromEnum) direction)

-- | Gets the contents of the tooltip for widget.
widgetGetTooltipMarkup :: (WidgetClass self, GlibString markup) => self
 -> IO (Maybe markup) -- Returns the tooltip text, or Nothing.
widgetGetTooltipMarkup self =
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_get_tooltip_markup argPtr1)
{-# LINE 2646 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
  >>= maybePeek peekUTFString

-- | Sets @markup@ as the contents of the tooltip, which is marked up with the
-- Pango text markup language.
--
-- This function will take care of setting "has-tooltip" to True and of the
-- default handler for the "query-tooltip" signal.
--
-- See also the "tooltip-markup" property and 'tooltipSetMarkup'.
widgetSetTooltipMarkup :: (WidgetClass self, GlibString markup) => self
  -> Maybe markup -- ^ the contents of the tooltip for widget, or @Nothing@.
  -> IO ()
widgetSetTooltipMarkup self markup =
  maybeWith withUTFString markup $ \ markupPtr ->
  (\(Widget arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_set_tooltip_markup argPtr1 arg2)
{-# LINE 2662 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
    markupPtr

-- | Gets the contents of the tooltip for widget.
widgetGetTooltipText :: (WidgetClass self, GlibString text) => self
 -> IO (Maybe text) -- Returns the tooltip text, or Nothing.
widgetGetTooltipText self =
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_get_tooltip_text argPtr1)
{-# LINE 2670 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
  >>= maybePeek peekUTFString

-- | Sets @text@ as the contents of the tooltip. This function will take care
-- of setting "has-tooltip" to @True@ and of the default handler for the
-- "query-tooltip" signal.
--
-- See also the "tooltip-text" property and 'tooltipSetText'.
widgetSetTooltipText :: (WidgetClass widget, GlibString text) => widget
  -> Maybe text -- ^ the contents of the tooltip for widget, or @Nothing@.
  -> IO ()
widgetSetTooltipText widget text =
  maybeWith withUTFString text $ \ textPtr ->
  (\(Widget arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_set_tooltip_text argPtr1 arg2)
{-# LINE 2684 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget widget)
    textPtr

-- | Returns the 'Window' of the current tooltip. This can be the 'Window' created by default, or the
-- custom tooltip window set using 'widgetSetTooltipWindow'.
--
-- * Available since Gtk+ version 2.12
--
widgetGetTooltipWindow :: WidgetClass self => self
 -> IO Window -- ^ returns The 'Window' of the current tooltip
widgetGetTooltipWindow self =
  makeNewObject mkWindow $
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_get_tooltip_window argPtr1)
{-# LINE 2697 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)

-- | Replaces the default, usually yellow, window used for displaying tooltips with @customWindow@. GTK+
-- will take care of showing and hiding @customWindow@ at the right moment, to behave likewise as the
-- default tooltip window. If @customWindow@ is 'Nothing', the default tooltip window will be used.
--
-- If the custom window should have the default theming it needs to have the name 'gtk-tooltip', see
-- 'widgetSetName'.
--
-- * Available since Gtk+ version 2.12
--
widgetSetTooltipWindow :: (WidgetClass self, WindowClass customWindow) => self
 -> Maybe customWindow -- ^ @customWindow@ a 'Window', or 'Nothing'. allow-none.
 -> IO ()
widgetSetTooltipWindow self customWindow =
  (\(Widget arg1) (Window arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_widget_set_tooltip_window argPtr1 argPtr2)
{-# LINE 2713 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
    (maybe (Window nullForeignPtr) toWindow customWindow)

-- | Returns the current value of the has-tooltip property.
-- See 'widgetHasTooltip' for more information.
widgetGetHasTooltip :: WidgetClass widget => widget
 -> IO Bool -- ^ current value of 'widgetHasTooltip' on @widget@.
widgetGetHasTooltip widget =
  liftM toBool $
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_get_has_tooltip argPtr1)
{-# LINE 2723 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget widget)

-- | Sets the has-tooltip property on @widget@ to @hasTooltip@.
-- See 'widgetHasTooltip' for more information.
widgetSetHasTooltip :: WidgetClass widget => widget
 -> Bool -- ^ @hasTooltip@ whether or not @widget@ has a tooltip.
 -> IO ()
widgetSetHasTooltip widget hasTooltip =
  (\(Widget arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_set_has_tooltip argPtr1 arg2)
{-# LINE 2732 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget widget)
    (fromBool hasTooltip)

-- | Triggers a tooltip query on the display where the toplevel of @widget@ is
-- located. See 'tooltipTriggerTooltipQuery' for more information.
--
-- * Available since Gtk+ version 2.12
--
widgetTriggerTooltipQuery :: WidgetClass self => self -> IO ()
widgetTriggerTooltipQuery self =
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_trigger_tooltip_query argPtr1)
{-# LINE 2743 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)



-- | Returns the widget's window if it is realized, Nothing otherwise
--
-- * Available since Gtk+ version 2.14
--
widgetGetWindow :: WidgetClass self => self -> IO (Maybe DrawWindow)
widgetGetWindow self =
  maybeNull (makeNewGObject mkDrawWindow) $
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_get_window argPtr1)
{-# LINE 2755 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
{-# LINE 2821 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
-- | Moves a widget from one 'Container' to another.
--
widgetReparent :: (WidgetClass self, WidgetClass newParent) => self
 -> newParent -- ^ @newParent@ - a 'Container' to move the widget into
 -> IO ()
widgetReparent self newParent =
  (\(Widget arg1) (Widget arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_widget_reparent argPtr1 argPtr2)
{-# LINE 2828 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)
    (toWidget newParent)


-- | Set if this widget can receive keyboard input.
--
-- * To use the 'keyPress' event, the widget must be allowed
-- to get the input focus. Once it has the input focus all keyboard
-- input is directed to this widget.
--
widgetSetCanFocus :: WidgetClass self => self -> Bool -> IO ()
widgetSetCanFocus = objectSetPropertyBool "can_focus"

-- | Check if this widget can receive keyboard input.
--
widgetGetCanFocus :: WidgetClass self => self -> IO Bool
widgetGetCanFocus = objectGetPropertyBool "can_focus"

-- | Retrieves the widget's allocation.
--
-- * Available since Gtk+ version 2.18
--
widgetGetAllocation :: WidgetClass self => self -> IO Allocation
widgetGetAllocation widget =
  alloca $ \ allocationPtr -> do
     (\(Widget arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_get_allocation argPtr1 arg2) (toWidget widget) (castPtr allocationPtr)
     peek allocationPtr
{-# LINE 2916 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
-- | Determines whether the application intends to draw on the widget in an
-- "draw" handler.
-- See 'widgetSetAppPaintable'.
widgetGetAppPaintable :: WidgetClass widget => widget
 -> IO Bool -- ^ Returns @True@ if the @widget@ is app paintable.
widgetGetAppPaintable widget =
  liftM toBool $
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_get_app_paintable argPtr1)
{-# LINE 2924 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget widget)

-- | Determines whether @widget@ can be a default widget.
-- See 'widgetSetCanDefault'.
widgetGetCanDefault :: WidgetClass widget => widget
 -> IO Bool -- ^ Returns @True@ if @widget@ can be a default widget, @False@ otherwise.
widgetGetCanDefault widget =
  liftM toBool $
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_get_can_default argPtr1)
{-# LINE 2933 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget widget)

-- | Specifies whether @widget@ can be a default widget.
-- See 'widgetGrabDefault' for details about the meaning of "default".
widgetSetCanDefault :: WidgetClass widget => widget
 -> Bool -- ^ @canDefault@ whether or not @widget@ can be a default widget.
 -> IO ()
widgetSetCanDefault widget canDefault =
  (\(Widget arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_set_can_default argPtr1 arg2)
{-# LINE 2942 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget widget)
    (fromBool canDefault)

-- | Determines whether @widget@ has a 'DrawWindow' of its own. See 'widgetSetHasWindow'.
widgetGetHasWindow :: WidgetClass widget => widget
 -> IO Bool -- ^ Returns @True@ if @widget@ has a window, @False@ otherwise.
widgetGetHasWindow widget =
  liftM toBool $
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_get_has_window argPtr1)
{-# LINE 2951 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget widget)

-- | Specifies whether @widget@ has a 'DrawWindow' of its own. Note that all
-- realized widgets have a non-NULL "window" pointer ('widgetGetWindow' never
-- returns a NULL window when a widget is realized), but for many of them it's
-- actually the 'DrawWindow' of one of its parent widgets. Widgets that do not
-- create a window for themselves in "realize" must announce this by calling
-- this function with @hasWindow@ = @False@.
--
-- This function should only be called by widget implementations, and they
-- should call it in their @init()@ function.
widgetSetHasWindow :: WidgetClass widget => widget
 -> Bool -- ^ @hasWindow@ whether or not @widget@ has a window.
 -> IO ()
widgetSetHasWindow widget hasWindow =
  (\(Widget arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_set_has_window argPtr1 arg2)
{-# LINE 2967 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget widget)
    (fromBool hasWindow)

-- | Returns the @widget@’s sensitivity (in the sense of returning the value
-- that has been set using 'widgetSetSensitive').
--
-- The effective sensitivity of a widget is however determined by both its own
-- and its parent widget’s sensitivity. See 'widgetIsSensitive'.
widgetGetSensitive :: WidgetClass widget => widget
 -> IO Bool -- ^ Returns @True@ if the widget is sensitive.
widgetGetSensitive widget =
  liftM toBool $
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_get_sensitive argPtr1)
{-# LINE 2980 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget widget)

-- | Returns the widget’s effective sensitivity, which means it is sensitive
-- itself and also its parent widget is sensitive.
widgetIsSensitive :: WidgetClass widget => widget
 -> IO Bool -- ^ Returns @True@ if the widget is effectively sensitive.
widgetIsSensitive widget =
  liftM toBool $
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_is_sensitive argPtr1)
{-# LINE 2989 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget widget)

-- | Retrieve the current state of the widget.
--
-- * The state refers to different modes of user interaction, see
-- 'StateType' for more information.
--
widgetGetState :: WidgetClass self => self -> IO StateType
widgetGetState widget =
  liftM (toEnum . fromIntegral) $
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_get_state argPtr1)
{-# LINE 3000 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget widget)

-- | Determines whether the widget is visible. If you want to take into
-- account whether the widget’s parent is also marked as visible, use
-- 'widgetIsVisible' instead.
--
-- This function does not check if the widget is obscured in any way.
-- See 'widgetSetVisible'.
widgetGetVisible :: WidgetClass widget => widget
 -> IO Bool -- ^ Returns @True@ if the widget is visible.
widgetGetVisible widget =
  liftM toBool $
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_get_visible argPtr1)
{-# LINE 3013 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget widget)
{-# LINE 3077 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
-- | Determines whether @widget@ is the current default widget within its
-- toplevel. See 'widgetSetCanDefault'.
widgetGetHasDefault :: WidgetClass widget => widget
 -> IO Bool -- ^ Returns @True@ if @widget@ is the current default widget within its toplevel, @False@ otherwise.
widgetGetHasDefault widget =
  liftM toBool $
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_has_default argPtr1)
{-# LINE 3084 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget widget)

-- | Determines if the @widget@ has the global input focus.
-- See 'widgetIsFocus' for the difference between having the global input
-- focus, and only having the focus within a toplevel.
widgetGetHasFocus :: WidgetClass widget => widget
 -> IO Bool -- ^ Returns @True@ if @widget@ has the global input focus.
widgetGetHasFocus widget =
  liftM toBool $
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_has_focus argPtr1)
{-# LINE 3094 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget widget)
{-# LINE 3114 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
-- | Determines whether the widget is currently grabbing events, so it is the
-- only widget receiving input events (keyboard and mouse).
--
-- See also 'grabAdd'.
widgetHasGrab :: WidgetClass widget => widget
 -> IO Bool -- ^ Returns @True@ if the widget is in the grab_widgets stack
widgetHasGrab widget =
  liftM toBool $
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_has_grab argPtr1)
{-# LINE 3123 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget widget)

-- | Determines whether @widget@ can be drawn to. A widget can be drawn to if
-- it is mapped and visible.
widgetIsDrawable :: WidgetClass widget => widget
 -> IO Bool -- ^ Returns @True@ if @widget@ is drawable, @False@ otherwise
widgetIsDrawable widget =
  liftM toBool $
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_is_drawable argPtr1)
{-# LINE 3132 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget widget)

-- | Determines whether @widget@ is a toplevel widget.
--
-- Currently only 'Window' and 'Invisible' (and out-of-process 'Plugs') are
-- toplevel widgets. Toplevel widgets have no parent widget.
widgetIsToplevel :: WidgetClass widget => widget
 -> IO Bool -- ^ Returns @True@ if @widget@ is a toplevel, @False@ otherwise
widgetIsToplevel widget =
  liftM toBool $
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_is_toplevel argPtr1)
{-# LINE 3143 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget widget)

-- | Sets a widget’s window. This function should only be used in a widget’s
-- “realize” implementation. The window passed is usually either new window
-- created with 'drawWindowNew', or the window of its parent widget as
-- returned by 'widgetGetParentWindow'.
--
-- Widgets must indicate whether they will create their own 'DrawWindow' by
-- calling 'widgetSetHasWindow'. This is usually done in the widget’s init()
-- function.
--
-- Note that this function does not add any reference to window.
widgetSetWindow :: (WidgetClass widget, DrawWindowClass window) => widget
 -> window
 -> IO ()
widgetSetWindow widget window =
  (\(Widget arg1) (DrawWindow arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_widget_set_window argPtr1 argPtr2)
{-# LINE 3160 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget widget)
    (toDrawWindow window)

-- | Specifies whether @widget@ will be treated as the default widget within
-- its toplevel when it has the focus, even if another widget is the default.
--
-- See 'widgetGrabDefault' for details about the meaning of “default”.
widgetSetReceivesDefault :: WidgetClass widget => widget
 -> Bool -- ^ @receivesDefault@ whether or not widget can be a default widget.
 -> IO ()
widgetSetReceivesDefault widget receivesDefault =
  (\(Widget arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_set_receives_default argPtr1 arg2)
{-# LINE 3172 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget widget)
    (fromBool receivesDefault)

-- | Determines whether @widget@ is always treated as the default widget
-- within its toplevel when it has the focus, even if another widget is the
-- default.
--
-- See 'widgetSetReceivesDefault'.
widgetGetReceivesDefault :: WidgetClass widget => widget
 -> IO Bool -- ^ Returns @True@ if @widget@ acts as the default widget when focussed, @False@ otherwise
widgetGetReceivesDefault widget =
  liftM toBool $
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_get_receives_default argPtr1)
{-# LINE 3185 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget widget)
{-# LINE 3241 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
-- | This function is for use in widget implementations. Sets the state of a
-- widget (insensitive, prelighted, etc.) Usually you should set the state
-- using wrapper functions such as 'widgetSetSensitive'.
--
widgetSetState :: WidgetClass self => self -> StateType -> IO ()
widgetSetState widget state =
  (\(Widget arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_set_state argPtr1 arg2)
{-# LINE 3248 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget widget)
    ((fromIntegral . fromEnum) state)

-- | Rarely-used function. This function is used to emit the event signals on a widget (those signals
-- should never be emitted without using this function to do so). If you want to synthesize an event
-- though, don't use this function; instead, use 'mainDoEvent' so the event will behave as if it
-- were in the event queue. Don't synthesize expose events; instead, use 'windowInvalidateRect'
-- to invalidate a region of the window.
widgetEvent :: WidgetClass self => self -> EventM t Bool
widgetEvent widget = do
  ptr <- ask
  liftIO $ liftM toBool $ (\(Widget arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_event argPtr1 arg2) (toWidget widget) (castPtr ptr)

--------------------
-- Attributes

-- %hash c:6f7f d:9384
-- | The name of the widget.
--
-- Default value: @Nothing@
--
widgetName :: (WidgetClass self, GlibString string) => Attr self (Maybe string)
widgetName = newAttrFromMaybeStringProperty "name"

widgetMarginLeft :: WidgetClass self => Attr self Int
widgetMarginLeft = newAttrFromIntProperty "margin-left"

widgetMarginRight :: WidgetClass self => Attr self Int
widgetMarginRight = newAttrFromIntProperty "margin-right"
{-# LINE 3286 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
widgetMarginTop :: WidgetClass self => Attr self Int
widgetMarginTop = newAttrFromIntProperty "margin-top"

widgetMarginBottom :: WidgetClass self => Attr self Int
widgetMarginBottom = newAttrFromIntProperty "margin-bottom"

-- %hash c:1533 d:3213
-- | The parent widget of this widget. Must be a Container widget.
--
widgetParent :: (WidgetClass self, ContainerClass container) => ReadWriteAttr self (Maybe Container) (Maybe container)
widgetParent = newAttrFromMaybeObjectProperty "parent" gTypeContainer

-- %hash c:2b4c d:3c31
-- | Override for width request of the widget, or -1 if natural request should
-- be used.
--
-- Allowed values: >= -1
--
-- Default value: -1
--
widgetWidthRequest :: WidgetClass self => Attr self Int
widgetWidthRequest = newAttrFromIntProperty "width-request"

-- %hash c:fa97 d:172a
-- | Override for height request of the widget, or -1 if natural request
-- should be used.
--
-- Allowed values: >= -1
--
-- Default value: -1
--
widgetHeightRequest :: WidgetClass self => Attr self Int
widgetHeightRequest = newAttrFromIntProperty "height-request"

-- %hash c:70d0 d:e8e2
-- | Whether the widget is visible.
--
-- Default value: @False@
--
widgetVisible :: WidgetClass self => Attr self Bool
widgetVisible = newAttrFromBoolProperty "visible"

-- | The opacity of the widget
--
-- Default value: @1.0@
--
widgetOpacity :: WidgetClass self => Attr self Double
widgetOpacity = newAttrFromDoubleProperty "opacity"

-- %hash c:4dd4 d:594e
-- | Whether the widget responds to input.
--
-- Default value: @True@
--
widgetSensitive :: WidgetClass self => Attr self Bool
widgetSensitive = newAttrFromBoolProperty "sensitive"

-- %hash c:7506 d:1dde
-- | Whether the application will paint directly on the widget.
--
-- Default value: @False@
--
widgetAppPaintable :: WidgetClass self => Attr self Bool
widgetAppPaintable = newAttrFromBoolProperty "app-paintable"

-- %hash c:6289 d:72ab
-- | Whether the widget can accept the input focus.
--
-- Default value: @False@
--
widgetCanFocus :: WidgetClass self => Attr self Bool
widgetCanFocus = newAttrFromBoolProperty "can-focus"

-- %hash c:8e7 d:2645
-- | Whether the widget has the input focus.
--
-- Default value: @False@
--
widgetHasFocus :: WidgetClass self => Attr self Bool
widgetHasFocus = newAttrFromBoolProperty "has-focus"

-- %hash c:7547 d:1d78
-- | Whether the widget is the focus widget within the toplevel.
--
-- Default value: @False@
--
widgetIsFocus :: WidgetClass self => Attr self Bool
widgetIsFocus = newAttrFromBoolProperty "is-focus"

-- %hash c:f2d8 d:1cbb
-- | Whether the widget can be the default widget.
--
-- Default value: @False@
--
widgetCanDefault :: WidgetClass self => Attr self Bool
widgetCanDefault = newAttrFromBoolProperty "can-default"

-- %hash c:836 d:4cbe
-- | Whether the widget is the default widget.
--
-- Default value: @False@
--
widgetHasDefault :: WidgetClass self => Attr self Bool
widgetHasDefault = newAttrFromBoolProperty "has-default"

-- %hash c:f964 d:b62f
-- | If @True@, the widget will receive the default action when it is focused.
--
-- Default value: @False@
--
widgetReceivesDefault :: WidgetClass self => Attr self Bool
widgetReceivesDefault = newAttrFromBoolProperty "receives-default"

-- %hash c:2ca6 d:cad8
-- | Whether the widget is part of a composite widget.
--
-- Default value: @False@
--
widgetCompositeChild :: WidgetClass self => ReadAttr self Bool
widgetCompositeChild = readAttrFromBoolProperty "composite-child"

-- %hash c:4f01 d:bd3
-- | The style of the widget, which contains information about how it will
-- look (colors etc).
--
widgetStyle :: WidgetClass self => Attr self Style
widgetStyle = newAttrFromObjectProperty "style" gTypeStyle

-- | The current visual user interaction state of the widget (insensitive,
-- prelighted, selected etc). See 'StateType' for more information.
--
widgetState :: WidgetClass self => Attr self StateType
widgetState = newAttr
  widgetGetState
  widgetSetState

-- %hash c:e2a4 d:9296
-- | The event mask that decides what kind of GdkEvents this widget gets.
--
-- Default value: 'StructureMask'
--
widgetEvents :: WidgetClass self => Attr self [EventMask]
widgetEvents = newAttrFromFlagsProperty "events"
                 gdk_event_mask_get_type
{-# LINE 3430 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}


-- %hash c:ba80
-- | The mask that decides what kind of extension events this widget gets.
--
-- Default value: 'ExtensionEventsNone'
--
-- Removed in Gtk3.
widgetExtensionEvents :: WidgetClass self => Attr self [ExtensionMode]
widgetExtensionEvents = newAttr
  widgetGetExtensionEvents
  widgetSetExtensionEvents


-- | Whether to expand in both directions. Setting this sets both 'widgetHExpand' and 'widgetVExpand'
--
-- Default value: @False@
--
widgetExpand :: WidgetClass self => Attr self Bool
widgetExpand = newAttrFromBoolProperty "expand"

-- | Whether to expand horizontally. See 'widgetSetHExpand'
--
-- Default value: @False@
--
widgetHExpand :: WidgetClass self => Attr self Bool
widgetHExpand = newAttrFromBoolProperty "hexpand"

-- | Whether to use the “hexpand” property. See 'widgetGetHExpandSet'.
--
-- Default value: @False@
--
widgetHExpandSet :: WidgetClass self => Attr self Bool
widgetHExpandSet = newAttrFromBoolProperty "hexpand-set"

-- | Whether to expand vertically. See 'widgetSetVExpand'.
--
-- Default value: @False@
--
widgetVExpand :: WidgetClass self => Attr self Bool
widgetVExpand = newAttrFromBoolProperty "vexpand"

-- | Whether to use the “vexpand” property. See 'widgetGetVExpandSet'.
--
-- Default value: @False@
--
widgetVExpandSet :: WidgetClass self => Attr self Bool
widgetVExpandSet = newAttrFromBoolProperty "vexpand-set"

-- %hash c:1605 d:48ea
-- | Whether 'widgetShowAll' should not affect this widget.
--
-- Default value: @False@
--
widgetNoShowAll :: WidgetClass self => Attr self Bool
widgetNoShowAll = newAttrFromBoolProperty "no-show-all"

-- %hash c:cd8d d:59b2
-- | \'childVisible\' property. See 'widgetGetChildVisible' and
-- 'widgetSetChildVisible'
--
widgetChildVisible :: WidgetClass self => Attr self Bool
widgetChildVisible = newAttr
  widgetGetChildVisible
  widgetSetChildVisible


-- %hash c:a20a d:646f
-- | \'colormap\' property. See 'widgetGetColormap' and 'widgetSetColormap'
--
-- Removed in Gtk3.
widgetColormap :: WidgetClass self => Attr self Colormap
widgetColormap = newAttr
  widgetGetColormap
  widgetSetColormap


-- %hash c:a7fd d:55b8
-- | \'compositeName\' property. See 'widgetGetCompositeName' and
-- 'widgetSetCompositeName'
--
widgetCompositeName :: (WidgetClass self, GlibString string) => ReadWriteAttr self (Maybe string) string
widgetCompositeName = newAttr
  widgetGetCompositeName
  widgetSetCompositeName

-- %hash c:6c03 d:ce3b
-- | \'direction\' property. See 'widgetGetDirection' and 'widgetSetDirection'
--
widgetDirection :: WidgetClass self => Attr self TextDirection
widgetDirection = newAttr
  widgetGetDirection
  widgetSetDirection

-- | Sets the text of tooltip to be the given string, which is marked up with the Pango text markup
-- language. Also see 'tooltipSetMarkup'.
--
-- This is a convenience property which will take care of getting the tooltip shown if the given string
-- is not \"\": 'hasTooltip' will automatically be set to 'True' and there will be taken care of
-- 'queryTooltip' in the default signal handler.
--
-- Default value: \"\"
--
-- * Available since Gtk+ version 2.12
--
widgetTooltipMarkup :: (WidgetClass self, GlibString markup) => Attr self (Maybe markup)
widgetTooltipMarkup = newAttrFromMaybeStringProperty "tooltip-markup"

-- | Sets the text of tooltip to be the given string.
--
-- Also see 'tooltipSetText'.
--
-- This is a convenience property which will take care of getting the tooltip shown if the given string
-- is not \"\": 'hasTooltip' will automatically be set to 'True' and there will be taken care of
-- 'queryTooltip' in the default signal handler.
--
-- Default value: \"\"
--
-- * Available since Gtk+ version 2.12
--
widgetTooltipText :: (WidgetClass self, GlibString string) => Attr self (Maybe string)
widgetTooltipText = newAttrFromMaybeStringProperty "tooltip-text"

-- | Enables or disables the emission of 'queryTooltip' on widget. A value of 'True' indicates that widget
-- can have a tooltip, in this case the widget will be queried using 'queryTooltip' to determine
-- whether it will provide a tooltip or not.
--
-- Note that setting this property to 'True' for the first time will change the event masks of the
-- 'Windows' of this widget to include leave-notify and motion-notify events. This cannot and will not
-- be undone when the property is set to 'False' again.
--
-- Default value: 'False'
--
-- * Available since Gtk+ version 2.12
--
widgetHasTooltip :: WidgetClass self => Attr self Bool
widgetHasTooltip = newAttrFromBoolProperty "has-tooltip"


-- | Determines if the widget style has been looked up through the rc mechanism.
widgetHasRcStyle :: WidgetClass self => self
                 -> IO Bool -- ^ returns 'True' if the widget has been looked up through the rc mechanism, 'False' otherwise.
widgetHasRcStyle self =
  liftM toBool $
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_has_rc_style argPtr1)
{-# LINE 3575 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)

-- | Determines whether widget is realized.
widgetGetRealized :: WidgetClass self => self
                  -> IO Bool -- ^ returns 'True' if widget is realized, 'False' otherwise
widgetGetRealized self =
  liftM toBool $
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_get_realized argPtr1)
{-# LINE 3583 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)

-- | Whether the widget is mapped.
widgetGetMapped :: WidgetClass self => self
                -> IO Bool -- ^ returns 'True' if the widget is mapped, 'False' otherwise.
widgetGetMapped self =
  liftM toBool $
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_get_mapped argPtr1)
{-# LINE 3591 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget self)

-- | Marks the @widget@ as being realized. This function must only be called
-- after all 'DrawWindows' for the widget have been created and registered.
--
-- This function should only ever be called in a derived widget's “realize”
-- or “unrealize” implementation.
widgetSetRealized :: WidgetClass widget => widget
 -> Bool -- ^ @realized@ @True@ to mark the widget realized.
 -> IO ()
widgetSetRealized widget realized =
  (\(Widget arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_set_realized argPtr1 arg2)
{-# LINE 3603 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget widget)
    (fromBool realized)

-- | Marks the @widget@ as being realized.
--
-- This function should only ever be called in a derived widget's “map” or
-- “unmap” implementation.
widgetSetMapped :: WidgetClass widget => widget
 -> Bool -- ^ @mapped@ @True@ to mark the widget as mapped.
 -> IO ()
widgetSetMapped widget mapped =
  (\(Widget arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_set_mapped argPtr1 arg2)
{-# LINE 3615 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
    (toWidget widget)
    (fromBool mapped)
{-# LINE 3678 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
--------------------
-- Signals


-- %hash c:4cf5 d:af3f
-- | The widget appears on screen.
--
mapSignal :: WidgetClass self => Signal self (IO ())
mapSignal = Signal (connect_NONE__NONE "map")

-- %hash c:e33e d:af3f
-- | The widget disappears from the screen.
--
unmapSignal :: WidgetClass self => Signal self (IO ())
unmapSignal = Signal (connect_NONE__NONE "unmap")

-- %hash c:1f7f d:af3f
-- | The widget should allocate any resources needed, in particular, the
-- widget's 'DrawWindow' is created. If you connect to this signal and
-- you rely on some of these resources to be present, you have to use
-- 'System.Glib.Signals.after'.
--
realize :: WidgetClass self => Signal self (IO ())
realize = Signal (connect_NONE__NONE "realize")

-- %hash c:7948 d:af3f
-- | The widget should deallocate any resources. This signal is emitted before
-- the widget is destroyed.
--
unrealize :: WidgetClass self => Signal self (IO ())
unrealize = Signal (connect_NONE__NONE "unrealize")

-- %hash c:9f6f d:af3f
-- | Query the widget for the size it likes to
-- have.
--
-- * A parent container emits this signal to its child to query the needed
-- height and width of the child. There is not guarantee that the widget
-- will actually get this area.
--
sizeRequest :: WidgetClass self => Signal self (IO Requisition)
sizeRequest = Signal (\after w fun ->
  connect_PTR__NONE "size-request" after w
    (\rqPtr -> fun >>= \req -> unless (rqPtr==nullPtr) $ poke rqPtr req))

-- %hash c:8ec5 d:af3f
-- | Inform widget about the size it has.
--
-- * After querying a widget for the size it wants to have (through emitting
-- the @\"sizeRequest\"@ signal) a container will emit this signal to
-- inform the widget about the real size it should occupy.
--
sizeAllocate :: WidgetClass self => Signal self (Allocation -> IO ())
sizeAllocate = Signal (connect_BOXED__NONE "size-allocate" peek)

-- %hash c:ae3e d:af3f
-- | The widget is shown.
--
showSignal :: WidgetClass self => Signal self (IO ())
showSignal = Signal (connect_NONE__NONE "show")

-- %hash c:f589 d:af3f
-- | The widget is hidden.
--
hideSignal :: WidgetClass self => Signal self (IO ())
hideSignal = Signal (connect_NONE__NONE "hide")

-- %hash c:a285 d:af3f
-- | The widget gains focus via the given user action.
--
focus :: WidgetClass self => Signal self (DirectionType -> IO Bool)
focus = Signal (connect_ENUM__BOOL "focus")

-- %hash c:78ae d:af3f
-- | The state of the widget (input focus, insensitive, etc.) has changed.
--
stateChanged :: WidgetClass self => Signal self (StateType -> IO ())
stateChanged = Signal (connect_ENUM__NONE "state-changed")
{-# LINE 3776 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
-- %hash c:bef2 d:1d66
-- | The 'parentSet' signal is emitted when a new parent has been set on a
-- widget. The parameter is the new parent.
--
parentSet :: WidgetClass self => Signal self (Maybe Widget -> IO ())
parentSet = Signal (connect_MOBJECT__NONE "parent-set")

-- %hash c:7e2b d:4049
-- | Emitted when there is a change in the hierarchy to which a widget belong.
-- More precisely, a widget is anchored when its toplevel ancestor is a
-- 'Window'. This signal is emitted when a widget changes from un-anchored to
-- anchored or vice-versa.
--
hierarchyChanged :: WidgetClass self => Signal self (Maybe Widget -> IO ())
hierarchyChanged = Signal (connect_MOBJECT__NONE "hierarchy-changed")

-- %hash c:5894 d:ba10
-- | The 'styleSet' signal is emitted when a new style has been set on a
-- widget. Note that style-modifying functions like 'widgetModifyBase' also
-- cause this signal to be emitted.
--
styleSet :: WidgetClass self => Signal self (Style -> IO ())
styleSet = Signal (connect_OBJECT__NONE "style-set")

-- %hash c:6bb1 d:af3f
-- | The default direction of text writing has changed.
--
directionChanged :: WidgetClass self => Signal self (TextDirection -> IO ())
directionChanged = Signal (connect_ENUM__NONE "direction-changed")

-- %hash c:c28c d:d116
-- | The 'grabNotify' signal is emitted when a widget becomes shadowed by a
-- Gtk+ grab (not a pointer or keyboard grab) on another widget, or when it
-- becomes unshadowed due to a grab being removed.
--
-- A widget is shadowed by a 'grabAdd' when the topmost grab widget in the
-- grab stack of its window group is not its ancestor.
--
grabNotify :: WidgetClass self => Signal self (Bool -> IO ())
grabNotify = Signal (connect_BOOL__NONE "grab-notify")

-- %hash c:e06c d:a681
-- | This signal gets emitted whenever a widget should pop up a
-- context-sensitive menu. This usually happens through the standard key
-- binding mechanism; by pressing a certain key while a widget is focused, the
-- user can cause the widget to pop up a menu. For example, the 'Entry' widget
-- creates a menu with clipboard commands.
--
popupMenuSignal :: WidgetClass self => Signal self (IO Bool)
popupMenuSignal = Signal (connect_NONE__BOOL "popup-menu")

-- | Specify what kind of help the user wants.
data WidgetHelpType = WidgetHelpTooltip
                    | WidgetHelpWhatsThis
                    deriving (Enum,Eq,Show)

{-# LINE 3829 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}

-- %hash c:b18e d:af3f
-- | Tell the widget to show an explanatory help text. Should return @True@
-- if help has been shown.
--
showHelp :: WidgetClass self => Signal self (WidgetHelpType -> IO Bool)
showHelp = Signal (connect_ENUM__BOOL "show-help")

-- %hash c:6a8f d:af3f
-- | The set of keyboard accelerators has changed.
--
accelClosuresChanged :: WidgetClass self => Signal self (IO ())
accelClosuresChanged = Signal (connect_NONE__NONE "accel-closures-changed")

-- %hash c:5ca d:af3f
-- | The widget moved to a new screen.
--
screenChanged :: WidgetClass self => Signal self (Screen -> IO ())
screenChanged = Signal (connect_OBJECT__NONE "screen-changed")

-- | Emitted when 'hasTooltip' is 'True' and the 'gtkTooltipTimeout' has expired with the cursor
-- hovering "above" widget; or emitted when widget got focus in keyboard mode.
--
-- Using the given coordinates, the signal handler should determine whether a tooltip should be shown
-- for widget. If this is the case 'True' should be returned, 'False' otherwise.
-- Note if widget got focus in keyboard mode, 'Point' is 'Nothing'.
--
-- The signal handler is free to manipulate tooltip with the therefore destined function calls.
--
-- * Available since Gtk+ version 2.12
--
queryTooltip :: WidgetClass self => Signal self (Widget -> Maybe Point -> Tooltip -> IO Bool)
queryTooltip =
  Signal (\after model user ->
           connect_OBJECT_INT_INT_BOOL_OBJECT__BOOL "query-tooltip"
             after model (\widget x y keyb tooltip ->
                              user widget (if keyb then Nothing else Just (x, y)) tooltip))
{-# LINE 3874 "./Graphics/UI/Gtk/Abstract/Widget.chs" #-}
-- * Events
--
-- An event is a signal that indicates that some low-level interaction like a
-- button or key press, mouse movement, etc. has occurred. In particular,
-- events relate to operations on 'DrawWindow's which are a concept of the
-- underlying OS rather than the logical widget concept. Some widgets have no
-- window and use their parent to receive these events. Widgets normally
-- synthesize more sophistiacted signals from events. For instance, the
-- 'focusIn' and a 'focusOut' signal indicate that the widget gains or looses
-- the input focus. From these events a 'focus' signal is synthesized that
-- indicates what maneuver lead to the input focus change (i.e. a tab or
-- shift-tab key press).
--
-- For applications it is often sufficient to connect to the high-level
-- signals rather than the low-level events. Only in cases where a custom
-- widget is built based on the 'DarwingArea' skeleton, the functionality of
-- such an application-specific widget needs to be implemented using events.
--
-- Every event is passed an 'Event' structure that contains the data of the
-- event. The return value should be @True@ if the handler has dealt with the
-- event and @False@ if the event should be propagated further. For instance,
-- if a key press event that isn't meaningful in the widget, the handler can
-- return @False@ such that the key is handled by the other widgets (the main
-- menu, for instance).
--


-- Because there are so many similar signals (those that take an Event and
-- return a Bool) we will abstract out the skeleton. As some of these events
-- are emitted at a high rate often a bit has to be set to enable emission.


eventM :: WidgetClass w => SignalName -> [EventMask] ->
  ConnectAfter -> w -> (EventM t Bool) -> IO (ConnectId w)
eventM name eMask after obj fun = do
  id <- connect_PTR__BOOL name after obj (runReaderT fun)
  widgetAddEvents obj eMask
  return id

-- %hash c:6cc d:af3f
-- | A mouse button has been depressed while the mouse pointer was within the
-- widget area. Sets the widget's 'ButtonPressMask' flag.
--
buttonPressEvent :: WidgetClass self => Signal self (EventM EButton Bool)
buttonPressEvent = Signal (eventM "button_press_event" [ButtonPressMask])

-- %hash c:62e8 d:af3f
-- | A mouse button has been released. Sets the widget's 'ButtonReleaseMask'
-- flag.
--
buttonReleaseEvent :: WidgetClass self => Signal self (EventM EButton Bool)
buttonReleaseEvent = Signal (eventM "button_release_event" [ButtonReleaseMask])

-- %hash c:23e5 d:af3f
-- | The scroll wheel of the mouse has been used. Sets the widget's
-- 'ScrollMask' flag.
--
scrollEvent :: WidgetClass self => Signal self (EventM EScroll Bool)
scrollEvent = Signal (eventM "scroll_event" [ScrollMask])

-- %hash c:ee92 d:af3f
-- | The mouse pointer has moved. Since receiving all mouse movements is
-- expensive, it is necessary to specify exactly what mouse motions are
-- required by calling 'widgetAddEvents' on this widget with one or more of
-- the following flags:
--
-- * 'PointerMotionMask': Track all movements.
--
-- * 'ButtonMotionMask': Only track movements if a button is depressed.
--
-- * 'Button1MotionMask': Only track movements if the left button is depressed.
--
-- * 'Button2MotionMask': Only track movements if the middle button is depressed.
--
-- * 'Button3MotionMask': Only track movements if the right button is depressed.
-- 'PointerMotionHintMask' is a special flag which can be used in
-- combination with any of the above and is used to reduce the number of
-- 'motionNotifyEvent's received. Normally a 'motionNotifyEvent' event is
-- received each time the mouse moves. However, if the application spends a
-- lot of time processing the event (updating the display, for example), it
-- can lag behind the position of the mouse. When using
-- 'PointerMotionHintMask', fewer 'motionNotifyEvent's will be sent, some of
-- which are marked as a hint. To receive more motion events after a motion
-- hint event, the application needs to asks for more, by calling
-- 'Graphics.UI.Gtk.Gdk.EventM.eventRequestMotions'. This effectively limits
-- the rate at which new motion events are received. (Note that you don't
-- need to check if the hint is set as
-- 'Graphics.UI.Gtk.Gdk.EventM.eventRequestMotions' does so automatically.)
--
motionNotifyEvent :: WidgetClass self => Signal self (EventM EMotion Bool)
motionNotifyEvent = Signal (eventM "motion_notify_event" [])

-- %hash c:8783 d:3e27
-- | The 'deleteEvent' signal is emitted if a user requests that a toplevel
-- window is closed. The default handler for this signal destroys the window.
-- Calling 'widgetHide' and returning @True@ on reception of this signal will
-- cause the window to be hidden instead, so that it can later be shown again
-- without reconstructing it.
--
deleteEvent :: WidgetClass self => Signal self (EventM EAny Bool)
deleteEvent = Signal (eventM "delete_event" [])

-- %hash c:c408 d:5514
-- | The 'destroyEvent' signal is emitted when a 'DrawWindow' is destroyed.
-- You rarely get this signal, because most widgets disconnect themselves from
-- their window before they destroy it, so no widget owns the window at
-- destroy time. However, you might want to connect to the 'objectDestroy'
-- signal of 'Object'.
--
destroyEvent :: WidgetClass self => Signal self (EventM EAny Bool)
destroyEvent = Signal (eventM "destroy_event" [])

-- %hash c:c79e d:af3f

-- | Instructs the widget to redraw.
--
-- * The 'DrawWindow' that needs to be redrawn is available via
-- 'eventWindow'.
--
-- * The part that needs to be redrawn is available via 'eventArea' and
-- 'eventRegion'. The options are, in order of efficiency: (a) redraw the
-- entire window, (b) ask for the 'eventArea' and redraw that rectangle, (c)
-- ask for the 'eventRegion' and redraw each of those rectangles.
--
-- Only the exposed region will be updated; see also
-- 'drawWindowBeginPaintRegion'.

exposeEvent :: WidgetClass self => Signal self (EventM EExpose Bool)
exposeEvent = Signal (eventM "expose_event" [])

-- %hash c:5ccd d:af3f
-- | A key has been depressed. Sets the widget's 'KeyPressMask' flag.
--
keyPressEvent :: WidgetClass self => Signal self (EventM EKey Bool)
keyPressEvent = Signal (eventM "key_press_event" [KeyPressMask])

-- %hash c:bd29 d:af3f
-- | A key has been released. Sets the widget's 'KeyReleaseMask' flag.
--
keyReleaseEvent :: WidgetClass self => Signal self (EventM EKey Bool)
keyReleaseEvent = Signal (eventM "key_release_event" [KeyReleaseMask])

-- %hash c:602e d:af3f
-- | The mouse pointer has entered the widget. Sets the widget's
-- 'EnterNotifyMask' flag.
--
enterNotifyEvent :: WidgetClass self => Signal self (EventM ECrossing Bool)
enterNotifyEvent = Signal (eventM "enter_notify_event" [EnterNotifyMask])

-- %hash c:3bfb d:af3f
-- | The mouse pointer has left the widget. Sets the widget's
-- 'LeaveNotifyMask' flag.
--
leaveNotifyEvent :: WidgetClass self => Signal self (EventM ECrossing Bool)
leaveNotifyEvent = Signal (eventM "leave_notify_event" [LeaveNotifyMask])

-- %hash c:2b64 d:af3f
-- | The size of the window has changed.
--
configureEvent :: WidgetClass self => Signal self (EventM EConfigure Bool)
configureEvent = Signal (eventM "configure_event" [])

-- %hash c:427e d:af3f
-- | The widget gets the input focus. Sets the widget's 'FocusChangeMask' flag.
--
focusInEvent :: WidgetClass self => Signal self (EventM EFocus Bool)
focusInEvent = Signal (eventM "focus_in_event" [FocusChangeMask])

-- %hash c:5281 d:af3f
-- | The widget lost the input focus. Sets the widget's 'FocusChangeMask' flag.
--
focusOutEvent :: WidgetClass self => Signal self (EventM EFocus Bool)
focusOutEvent = Signal (eventM "focus_out_event" [FocusChangeMask])

-- %hash c:63c4 d:af3f
-- | The window is put onto the screen.
--
mapEvent :: WidgetClass self => Signal self (EventM EAny Bool)
mapEvent = Signal (eventM "map_event" [])

-- %hash c:342d d:af3f
-- | The window is taken off the screen.
--
unmapEvent :: WidgetClass self => Signal self (EventM EAny Bool)
unmapEvent = Signal (eventM "unmap_event" [])

-- %hash c:a1dd d:af3f
-- | A 'DrawWindow' may be associated with a set of properties that are
-- identified by a 'PropertyTag'. This event is triggered if a property is
-- changed or deleted. Sets the widget's 'PropertyChangeMask' flag.
--
_propertyNotifyEvent :: WidgetClass self => Signal self (EventM EProperty Bool)
_propertyNotifyEvent = Signal (eventM "property_notify_event" [PropertyChangeMask])

{- not sure if these are useful
-- %hash c:58cc d:af3f
-- |
--
selectionClearEvent :: WidgetClass self => Signal self ({-GdkEventSelection*-} Bool)
selectionClearEvent = Signal (connect_{-GdkEventSelection*-}__BOOL "selection_clear_event")

-- %hash c:4f92 d:af3f
-- |
--
selectionRequestEvent :: WidgetClass self => Signal self ({-GdkEventSelection*-} Bool)
selectionRequestEvent = Signal (connect_{-GdkEventSelection*-}__BOOL "selection_request_event")

-- %hash c:b842 d:af3f
-- |
--
selectionNotifyEvent :: WidgetClass self => Signal self ({-GdkEventSelection*-} Bool)
selectionNotifyEvent = Signal (connect_{-GdkEventSelection*-}__BOOL "selection_notify_event")
-}

-- %hash c:b027 d:af3f
-- | The pen of a graphics tablet was put down. Sets the widget's
-- 'ProximityInMask' flag.
--
proximityInEvent :: WidgetClass self => Signal self (EventM EProximity Bool)
proximityInEvent = Signal (eventM "proximity_in_event" [ProximityInMask])

-- %hash c:faca d:af3f
-- | The pen of a graphics tablet was lifted off the tablet. Sets the widget's
-- 'ProximityOutMask' flag.
--
proximityOutEvent :: WidgetClass self => Signal self (EventM EProximity Bool)
proximityOutEvent = Signal (eventM "proximity_out_event" [ProximityOutMask])

-- %hash c:db2c d:af3f
-- | Emitted when the window visibility status has changed. Sets the widget's
-- 'VisibilityNotifyMask' flag.
--
visibilityNotifyEvent :: WidgetClass self => Signal self (EventM EVisibility Bool)
visibilityNotifyEvent = Signal (eventM "visibility_notify_event" [VisibilityNotifyMask])
{-
-- %hash c:3f5 d:af3f
-- |
--
clientEvent :: WidgetClass self => Signal self ({-GdkEventClient*-} Bool)
clientEvent = Signal (connect_{-GdkEventClient*-}__BOOL "client_event")
-}

-- %hash c:643c d:af3f
-- | Generated when the area of a 'Drawable' being copied using, e.g.
-- 'Graphics.UI.Gtk.Gdk.Drawable.drawDrawable', is completely available.
--
noExposeEvent :: WidgetClass self => Signal self (EventM EAny Bool)
noExposeEvent = Signal (eventM "no_expose_event" [])

-- %hash c:63b6 d:af3f
-- | Emitted when the state of the window changes, i.e. when it is minimized,
-- moved to the top, etc.
--
windowStateEvent :: WidgetClass self => Signal self (EventM EWindowState Bool)
windowStateEvent = Signal (eventM "window_state_event" [])


-- %hash c:502a d:e47a
-- | Emitted when a pointer or keyboard grab on a window belonging to @widget@
-- gets broken.
--
-- On X11, this happens when the grab window becomes unviewable (i.e. it or
-- one of its ancestors is unmapped), or if the same application grabs the
-- pointer or keyboard again.
--
-- * Available since Gtk+ version 2.8
--
grabBrokenEvent :: WidgetClass self => Signal self (EventM EGrabBroken Bool)
grabBrokenEvent = Signal (eventM "grab_broken_event" [])


--------------------
-- Deprecated Signals and Events



event :: WidgetClass w => SignalName -> [EventMask] ->
  ConnectAfter -> w -> (Event -> IO Bool) -> IO (ConnectId w)
event name eMask after obj fun = do
  id <- connect_BOXED__BOOL name marshalEvent after obj fun
  widgetAddEvents obj eMask
  return id

-- | A Button was pressed.
--
-- * This widget is part of a button which was just pressed. The event passed
-- to the user function is a 'Graphics.UI.Gtk.Gdk.Events.Button' event.
--
onButtonPress, afterButtonPress :: WidgetClass w => w -> (Event -> IO Bool) ->
                                   IO (ConnectId w)
onButtonPress = event "button_press_event" [ButtonPressMask] False
afterButtonPress = event "button_press_event" [ButtonPressMask] True

-- | A Button was released.
--
onButtonRelease, afterButtonRelease :: WidgetClass w => w ->
                                       (Event -> IO Bool) -> IO (ConnectId w)
onButtonRelease = event "button_release_event" [ButtonReleaseMask] False
afterButtonRelease = event "button_release_event" [ButtonReleaseMask] True

-- |
--
onClient, afterClient :: WidgetClass w => w -> (Event -> IO Bool) ->
                         IO (ConnectId w)
onClient = event "client_event" [] False
afterClient = event "client_event" [] True

-- | The widget's status has changed.
--
onConfigure, afterConfigure :: WidgetClass w => w -> (Event -> IO Bool) ->
                               IO (ConnectId w)
onConfigure = event "configure_event" [] False
afterConfigure = event "configure_event" [] True

-- | This signal is emitted when the close icon on the
-- surrounding window is pressed. The default action is to emit the
-- @\"destroy\"@ signal.
--
onDelete, afterDelete :: WidgetClass w => w -> (Event -> IO Bool) ->
                         IO (ConnectId w)
onDelete = event "delete_event" [] False
afterDelete = event "delete_event" [] True

-- | The widget will be destroyed.
--
-- * The widget received a destroy event from the window manager.
--
onDestroyEvent, afterDestroyEvent :: WidgetClass w =>
                                     w -> (Event -> IO Bool) ->
                                     IO (ConnectId w)
onDestroyEvent = event "destroy_event" [] False
afterDestroyEvent = event "destroy_event" [] True

-- | The default text direction was changed.
--
onDirectionChanged, afterDirectionChanged :: WidgetClass w => w ->
                                             (Event -> IO Bool) ->
                                             IO (ConnectId w)
onDirectionChanged = event "direction_changed" [] False
afterDirectionChanged = event "direction_changed" [] True

-- | Mouse cursor entered widget.
--
-- * Contains a 'Crossing' event.
--
onEnterNotify, afterEnterNotify :: WidgetClass w => w -> (Event -> IO Bool) ->
                                   IO (ConnectId w)
onEnterNotify = event "enter_notify_event" [EnterNotifyMask] False
afterEnterNotify = event "enter_notify_event" [EnterNotifyMask] True

-- | Mouse cursor leaves widget.
--
-- * Contains a 'Crossing' event.
--
onLeaveNotify, afterLeaveNotify :: WidgetClass w => w -> (Event -> IO Bool) ->
                                   IO (ConnectId w)
onLeaveNotify = event "leave_notify_event" [LeaveNotifyMask] False
afterLeaveNotify = event "leave_notify_event" [LeaveNotifyMask] True

-- | Instructs the widget to redraw.
--
-- * This event is useful for the 'DrawingArea'. On receiving this signal
-- the content of the passed Rectangle or Region needs to be redrawn.
-- The return value should be 'True' if the region was completely redrawn
-- and 'False' if other handlers in the chain should be invoked.
-- If a client will redraw the whole area and is not interested in the
-- extra information in 'Expose', it is more efficient
-- to use 'onExposeRect'.
--
-- * Widgets that are very expensive to re-render, such as an image editor,
-- may prefer to use the 'onExpose' call back which delivers a
-- 'Region' in addition to a 'Rectangle'. A 'Region' consists of several
-- rectangles that need redrawing. The simpler 'onExposeRect' event encodes
-- the area to be redrawn as a bounding rectangle which might be easier
-- to deal with in a particular application.
--
onExpose, afterExpose :: WidgetClass w => w -> (Event -> IO Bool) ->
                         IO (ConnectId w)
onExpose = event "expose_event" [] False
afterExpose = event "expose_event" [] True

-- | Expose event delivering a 'Rectangle'.
--
onExposeRect, afterExposeRect ::
    WidgetClass w => w -> (Rectangle -> IO ()) -> IO (ConnectId w)
onExposeRect w act = connect_BOXED__BOOL "expose_event"
  marshExposeRect False w (\r -> act r >> return True)
afterExposeRect w act = connect_BOXED__BOOL "expose_event"
  marshExposeRect True w (\r -> act r >> return True)

-- | This signal is called if the widget receives the input focus.
--
onFocus, afterFocus :: WidgetClass w => w -> (DirectionType -> IO Bool) ->
                       IO (ConnectId w)
onFocus = connect_ENUM__BOOL "focus" False
afterFocus = connect_ENUM__BOOL "focus" True

-- | Widget gains input focus.
--
onFocusIn, afterFocusIn :: WidgetClass w => w -> (Event -> IO Bool) ->
                           IO (ConnectId w)
onFocusIn = event "focus_in_event" [FocusChangeMask] False
afterFocusIn = event "focus_in_event" [FocusChangeMask] True

-- | Widget looses input focus.
--
onFocusOut, afterFocusOut :: WidgetClass w => w -> (Event -> IO Bool) ->
                             IO (ConnectId w)
onFocusOut = event "focus_out_event" [FocusChangeMask] False
afterFocusOut = event "focus_out_event" [FocusChangeMask] True

-- | The widget is about to receive all events.
--
-- * It is possible to redirect all input events to one widget to force the
-- user to use only this widget. Such a situation is initiated by
-- 'addGrab'.
--
onGrabFocus, afterGrabFocus :: WidgetClass w => w -> IO () ->
                               IO (ConnectId w)
onGrabFocus = connect_NONE__NONE "grab_focus" False
afterGrabFocus = connect_NONE__NONE "grab_focus" True

-- | The widget will be destroyed.
--
-- * This is the last signal this widget will receive.
--
onDestroy, afterDestroy :: WidgetClass w => w -> (IO ()) ->
                           IO (ConnectId w)
onDestroy = connect_NONE__NONE "destroy" False
afterDestroy = connect_NONE__NONE "destroy" True

-- | The widget was asked to hide itself.
--
-- * This signal is emitted each time 'widgetHide' is called. Use
-- 'onUnmap' when your application needs to be informed
-- when the widget is actually removed from screen.
--
onHide, afterHide :: WidgetClass w => w -> IO () -> IO (ConnectId w)
onHide = connect_NONE__NONE "hide" False
afterHide = connect_NONE__NONE "hide" True

-- | The toplevel window changed.
--
-- * When a subtree of widgets is removed or added from a tree with a toplevel
-- window this signal is emitted. It is emitted on each widget in the
-- detached or attached subtree.
--
onHierarchyChanged, afterHierarchyChanged :: WidgetClass w => w -> IO () ->
                                             IO (ConnectId w)
onHierarchyChanged = connect_NONE__NONE "hierarchy_changed" False
afterHierarchyChanged = connect_NONE__NONE "hierarchy_changed" True

-- | A key was pressed.
--
onKeyPress, afterKeyPress :: WidgetClass w => w -> (Event -> IO Bool) ->
                             IO (ConnectId w)
onKeyPress = event "key_press_event" [KeyPressMask] False
afterKeyPress = event "key_press_event" [KeyPressMask] True

-- | A key was released.
--
onKeyRelease, afterKeyRelease :: WidgetClass w => w -> (Event -> IO Bool) ->
                                 IO (ConnectId w)
onKeyRelease = event "key_release_event" [KeyReleaseMask] False
afterKeyRelease = event "key_release_event" [KeyReleaseMask] True

-- |
--
onMnemonicActivate, afterMnemonicActivate :: WidgetClass w => w ->
                                             (Bool -> IO Bool) ->
                                             IO (ConnectId w)
onMnemonicActivate = connect_BOOL__BOOL "mnemonic_activate" False
afterMnemonicActivate = connect_BOOL__BOOL "mnemonic_activate" True

-- | Track mouse movements.
--
-- * If @hint@ is False, a callback for every movement of the mouse is
-- generated. To avoid a backlog of mouse messages, it is usually sufficient
-- to sent @hint@ to True, generating only one event. The
-- application now has to state that it is ready for the next message by
-- calling 'Graphics.UI.Gtk.Gdk.DrawWindow.drawWindowGetPointer'.
--
onMotionNotify, afterMotionNotify :: WidgetClass w => w -> Bool ->
                                     (Event -> IO Bool) ->
                                     IO (ConnectId w)
onMotionNotify w hint = event "motion_notify_event"
  (if hint then [PointerMotionMask, PointerMotionHintMask]
           else [PointerMotionMask]) False w
afterMotionNotify w hint = event "motion_notify_event"
  (if hint then [PointerMotionMask, PointerMotionHintMask]
           else [PointerMotionMask]) True w

-- |
--
onParentSet, afterParentSet :: (WidgetClass w, WidgetClass old) => w ->
                               (old -> IO ()) -> IO (ConnectId w)
onParentSet = connect_OBJECT__NONE "parent_set" False
afterParentSet = connect_OBJECT__NONE "parent_set" True

-- |
--
onPopupMenu, afterPopupMenu :: WidgetClass w => w -> IO () -> IO (ConnectId w)
onPopupMenu = connect_NONE__NONE "popup_menu" False
afterPopupMenu = connect_NONE__NONE "popup_menu" True

-- | The input device became active.
--
-- * This event indicates that a pen of a graphics tablet or similar device is
-- now touching the tablet.
--
onProximityIn, afterProximityIn :: WidgetClass w => w -> (Event -> IO Bool) ->
                                   IO (ConnectId w)
onProximityIn = event "proximity_in_event" [ProximityInMask] False
afterProximityIn = event "proximity_in_event" [ProximityInMask] True

-- | The input device became inactive.
--
-- * The pen was removed from the graphics tablet's surface.
--
onProximityOut, afterProximityOut :: WidgetClass w => w ->
                                     (Event -> IO Bool) -> IO (ConnectId w)
onProximityOut = event "proximity_out_event" [ProximityOutMask] False
afterProximityOut = event "proximity_out_event" [ProximityOutMask] True

-- | This widget's drawing area is about to be
-- destroyed.
--
onRealize, afterRealize :: WidgetClass w => w -> IO () -> IO (ConnectId w)
onRealize = connect_NONE__NONE "realize" False
afterRealize = connect_NONE__NONE "realize" True

-- | The mouse wheel has turned.
--
-- * The 'Event' is always 'Scroll'.
--
onScroll, afterScroll :: WidgetClass w => w -> (Event -> IO Bool) ->
                         IO (ConnectId w)
onScroll = event "scroll_event" [ScrollMask] False
afterScroll = event "scroll_event" [ScrollMask] True

-- | The widget was asked to show itself.
--
-- * This signal is emitted each time 'widgetShow' is called. Use
-- 'onMap' when your application needs to be informed when
-- the widget is actually shown.
--
onShow, afterShow :: WidgetClass w => w -> IO () -> IO (ConnectId w)
onShow = connect_NONE__NONE "show" False
afterShow = connect_NONE__NONE "show" True

-- | Inform widget about the size it has.
--
-- * After querying a widget for the size it wants to have (through emitting
-- the @\"sizeRequest\"@ signal) a container will emit this signal to
-- inform the widget about the real size it should occupy.
--
onSizeAllocate, afterSizeAllocate :: WidgetClass w => w ->
                                     (Allocation -> IO ()) -> IO (ConnectId w)
onSizeAllocate = connect_BOXED__NONE "size_allocate" peek False
afterSizeAllocate = connect_BOXED__NONE "size_allocate" peek True

-- | Query the widget for the size it likes to
-- have.
--
-- * A parent container emits this signal to its child to query the needed
-- height and width of the child. There is not guarantee that the widget
-- will actually get this area.
--
onSizeRequest, afterSizeRequest :: WidgetClass w => w -> (IO Requisition) ->
                                   IO (ConnectId w)
onSizeRequest w fun = connect_PTR__NONE "size_request" False w (\rqPtr -> do
  req <- fun
  unless (rqPtr==nullPtr) $ poke rqPtr req)
afterSizeRequest w fun = connect_PTR__NONE "size_request" True w (\rqPtr -> do
  req <- fun
  unless (rqPtr==nullPtr) $ poke rqPtr req)

-- |
--
onStateChanged, afterStateChanged :: WidgetClass w => w ->
                                     (StateType -> IO ()) -> IO (ConnectId w)
onStateChanged = connect_ENUM__NONE "state_changed" False
afterStateChanged = connect_ENUM__NONE "state_changed" True

-- | The widget was removed from screen.
--
onUnmap, afterUnmap :: WidgetClass w => w -> IO () -> IO (ConnectId w)
onUnmap = connect_NONE__NONE "unmap" False
afterUnmap = connect_NONE__NONE "unmap" True

-- | This widget's drawing area is about to be
-- destroyed.
--
onUnrealize, afterUnrealize :: WidgetClass w => w -> IO () -> IO (ConnectId w)
onUnrealize = connect_NONE__NONE "unrealize" False
afterUnrealize = connect_NONE__NONE "unrealize" True

-- |
--
onVisibilityNotify, afterVisibilityNotify :: WidgetClass w => w ->
                                             (Event -> IO Bool) ->
                                             IO (ConnectId w)
onVisibilityNotify =
  event "visibility_notify_event" [VisibilityNotifyMask] False
afterVisibilityNotify =
  event "visibility_notify_event" [VisibilityNotifyMask] True

-- |
--
onWindowState, afterWindowState :: WidgetClass w => w -> (Event -> IO Bool) ->
                                   IO (ConnectId w)
onWindowState = event "window_state_event" [] False
afterWindowState = event "window_state_event" [] True

foreign import ccall safe "gtk_widget_show"
  gtk_widget_show :: ((Ptr Widget) -> (IO ()))

foreign import ccall safe "gtk_widget_show_now"
  gtk_widget_show_now :: ((Ptr Widget) -> (IO ()))

foreign import ccall safe "gtk_widget_hide"
  gtk_widget_hide :: ((Ptr Widget) -> (IO ()))

foreign import ccall safe "gtk_widget_show_all"
  gtk_widget_show_all :: ((Ptr Widget) -> (IO ()))

foreign import ccall safe "gtk_widget_hide_all"
  gtk_widget_hide_all :: ((Ptr Widget) -> (IO ()))

foreign import ccall safe "gtk_widget_destroy"
  gtk_widget_destroy :: ((Ptr Widget) -> (IO ()))

foreign import ccall safe "gtk_widget_queue_draw"
  gtk_widget_queue_draw :: ((Ptr Widget) -> (IO ()))

foreign import ccall safe "gtk_widget_queue_resize"
  gtk_widget_queue_resize :: ((Ptr Widget) -> (IO ()))

foreign import ccall safe "gtk_widget_queue_resize_no_redraw"
  gtk_widget_queue_resize_no_redraw :: ((Ptr Widget) -> (IO ()))

foreign import ccall safe "gtk_widget_size_request"
  gtk_widget_size_request :: ((Ptr Widget) -> ((Ptr ()) -> (IO ())))

foreign import ccall safe "gtk_widget_get_child_requisition"
  gtk_widget_get_child_requisition :: ((Ptr Widget) -> ((Ptr ()) -> (IO ())))

foreign import ccall safe "gtk_widget_size_allocate"
  gtk_widget_size_allocate :: ((Ptr Widget) -> ((Ptr ()) -> (IO ())))

foreign import ccall safe "gtk_widget_add_accelerator"
  gtk_widget_add_accelerator :: ((Ptr Widget) -> ((Ptr CChar) -> ((Ptr AccelGroup) -> (CUInt -> (CInt -> (CInt -> (IO ())))))))

foreign import ccall safe "gtk_widget_remove_accelerator"
  gtk_widget_remove_accelerator :: ((Ptr Widget) -> ((Ptr AccelGroup) -> (CUInt -> (CInt -> (IO CInt)))))

foreign import ccall safe "gtk_widget_set_accel_path"
  gtk_widget_set_accel_path :: ((Ptr Widget) -> ((Ptr CChar) -> ((Ptr AccelGroup) -> (IO ()))))

foreign import ccall safe "gtk_widget_can_activate_accel"
  gtk_widget_can_activate_accel :: ((Ptr Widget) -> (CUInt -> (IO CInt)))

foreign import ccall safe "gtk_widget_activate"
  gtk_widget_activate :: ((Ptr Widget) -> (IO CInt))

foreign import ccall unsafe "gtk_widget_intersect"
  gtk_widget_intersect :: ((Ptr Widget) -> ((Ptr ()) -> ((Ptr ()) -> (IO CInt))))

foreign import ccall unsafe "gtk_widget_is_focus"
  gtk_widget_is_focus :: ((Ptr Widget) -> (IO CInt))

foreign import ccall safe "gtk_widget_grab_focus"
  gtk_widget_grab_focus :: ((Ptr Widget) -> (IO ()))

foreign import ccall safe "gtk_widget_grab_default"
  gtk_widget_grab_default :: ((Ptr Widget) -> (IO ()))

foreign import ccall safe "gtk_widget_set_name"
  gtk_widget_set_name :: ((Ptr Widget) -> ((Ptr CChar) -> (IO ())))

foreign import ccall unsafe "gtk_widget_get_name"
  gtk_widget_get_name :: ((Ptr Widget) -> (IO (Ptr CChar)))

foreign import ccall safe "gtk_widget_set_sensitive"
  gtk_widget_set_sensitive :: ((Ptr Widget) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_widget_get_parent_window"
  gtk_widget_get_parent_window :: ((Ptr Widget) -> (IO (Ptr DrawWindow)))

foreign import ccall unsafe "gtk_widget_get_events"
  gtk_widget_get_events :: ((Ptr Widget) -> (IO CInt))

foreign import ccall unsafe "gtk_widget_set_events"
  gtk_widget_set_events :: ((Ptr Widget) -> (CInt -> (IO ())))

foreign import ccall unsafe "gtk_widget_add_events"
  gtk_widget_add_events :: ((Ptr Widget) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_widget_set_extension_events"
  gtk_widget_set_extension_events :: ((Ptr Widget) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_widget_get_extension_events"
  gtk_widget_get_extension_events :: ((Ptr Widget) -> (IO CInt))

foreign import ccall unsafe "gtk_widget_get_toplevel"
  gtk_widget_get_toplevel :: ((Ptr Widget) -> (IO (Ptr Widget)))

foreign import ccall safe "gtk_widget_get_ancestor"
  gtk_widget_get_ancestor :: ((Ptr Widget) -> (CULong -> (IO (Ptr Widget))))

foreign import ccall safe "gtk_widget_get_colormap"
  gtk_widget_get_colormap :: ((Ptr Widget) -> (IO (Ptr Colormap)))

foreign import ccall safe "gtk_widget_set_colormap"
  gtk_widget_set_colormap :: ((Ptr Widget) -> ((Ptr Colormap) -> (IO ())))

foreign import ccall safe "gtk_widget_get_pointer"
  gtk_widget_get_pointer :: ((Ptr Widget) -> ((Ptr CInt) -> ((Ptr CInt) -> (IO ()))))

foreign import ccall unsafe "gtk_widget_is_ancestor"
  gtk_widget_is_ancestor :: ((Ptr Widget) -> ((Ptr Widget) -> (IO CInt)))

foreign import ccall safe "gtk_widget_translate_coordinates"
  gtk_widget_translate_coordinates :: ((Ptr Widget) -> ((Ptr Widget) -> (CInt -> (CInt -> ((Ptr CInt) -> ((Ptr CInt) -> (IO CInt)))))))

foreign import ccall safe "gtk_widget_set_style"
  gtk_widget_set_style :: ((Ptr Widget) -> ((Ptr Style) -> (IO ())))

foreign import ccall safe "gtk_widget_ensure_style"
  gtk_widget_ensure_style :: ((Ptr Widget) -> (IO ()))

foreign import ccall safe "gtk_widget_get_style"
  gtk_widget_get_style :: ((Ptr Widget) -> (IO (Ptr Style)))

foreign import ccall safe "gtk_widget_push_colormap"
  gtk_widget_push_colormap :: ((Ptr Colormap) -> (IO ()))

foreign import ccall safe "gtk_widget_pop_colormap"
  gtk_widget_pop_colormap :: (IO ())

foreign import ccall safe "gtk_widget_set_default_colormap"
  gtk_widget_set_default_colormap :: ((Ptr Colormap) -> (IO ()))

foreign import ccall safe "gtk_widget_get_default_style"
  gtk_widget_get_default_style :: (IO (Ptr Style))

foreign import ccall safe "gtk_widget_get_default_colormap"
  gtk_widget_get_default_colormap :: (IO (Ptr Colormap))

foreign import ccall safe "gtk_widget_set_direction"
  gtk_widget_set_direction :: ((Ptr Widget) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_widget_get_direction"
  gtk_widget_get_direction :: ((Ptr Widget) -> (IO CInt))

foreign import ccall safe "gtk_widget_set_default_direction"
  gtk_widget_set_default_direction :: (CInt -> (IO ()))

foreign import ccall safe "gtk_widget_get_default_direction"
  gtk_widget_get_default_direction :: (IO CInt)

foreign import ccall safe "gtk_widget_shape_combine_mask"
  gtk_widget_shape_combine_mask :: ((Ptr Widget) -> ((Ptr ()) -> (CInt -> (CInt -> (IO ())))))

foreign import ccall safe "gtk_widget_input_shape_combine_mask"
  gtk_widget_input_shape_combine_mask :: ((Ptr Widget) -> ((Ptr ()) -> (CInt -> (CInt -> (IO ())))))

foreign import ccall safe "gtk_widget_get_snapshot"
  gtk_widget_get_snapshot :: ((Ptr Widget) -> ((Ptr ()) -> (IO (Ptr Pixmap))))

foreign import ccall safe "gtk_widget_path"
  gtk_widget_path :: ((Ptr Widget) -> ((Ptr CUInt) -> ((Ptr (Ptr CChar)) -> ((Ptr (Ptr CChar)) -> (IO ())))))

foreign import ccall safe "gtk_widget_class_path"
  gtk_widget_class_path :: ((Ptr Widget) -> ((Ptr CUInt) -> ((Ptr (Ptr CChar)) -> ((Ptr (Ptr CChar)) -> (IO ())))))

foreign import ccall safe "gtk_widget_get_composite_name"
  gtk_widget_get_composite_name :: ((Ptr Widget) -> (IO (Ptr CChar)))

foreign import ccall safe "gtk_widget_modify_style"
  gtk_widget_modify_style :: ((Ptr Widget) -> ((Ptr RcStyle) -> (IO ())))

foreign import ccall safe "gtk_widget_get_modifier_style"
  gtk_widget_get_modifier_style :: ((Ptr Widget) -> (IO (Ptr RcStyle)))

foreign import ccall safe "gtk_widget_modify_fg"
  gtk_widget_modify_fg :: ((Ptr Widget) -> (CInt -> ((Ptr ()) -> (IO ()))))

foreign import ccall safe "gtk_widget_modify_bg"
  gtk_widget_modify_bg :: ((Ptr Widget) -> (CInt -> ((Ptr ()) -> (IO ()))))

foreign import ccall safe "gtk_widget_modify_text"
  gtk_widget_modify_text :: ((Ptr Widget) -> (CInt -> ((Ptr ()) -> (IO ()))))

foreign import ccall safe "gtk_widget_modify_base"
  gtk_widget_modify_base :: ((Ptr Widget) -> (CInt -> ((Ptr ()) -> (IO ()))))

foreign import ccall safe "gtk_widget_modify_font"
  gtk_widget_modify_font :: ((Ptr Widget) -> ((Ptr FontDescription) -> (IO ())))

foreign import ccall safe "gtk_widget_create_pango_context"
  gtk_widget_create_pango_context :: ((Ptr Widget) -> (IO (Ptr PangoContext)))

foreign import ccall safe "gtk_widget_get_pango_context"
  gtk_widget_get_pango_context :: ((Ptr Widget) -> (IO (Ptr PangoContext)))

foreign import ccall unsafe "gtk_widget_create_pango_layout"
  gtk_widget_create_pango_layout :: ((Ptr Widget) -> ((Ptr CChar) -> (IO (Ptr PangoLayoutRaw))))

foreign import ccall safe "gtk_widget_render_icon"
  gtk_widget_render_icon :: ((Ptr Widget) -> ((Ptr CChar) -> (CInt -> ((Ptr CChar) -> (IO (Ptr Pixbuf))))))

foreign import ccall safe "gtk_widget_queue_draw_area"
  gtk_widget_queue_draw_area :: ((Ptr Widget) -> (CInt -> (CInt -> (CInt -> (CInt -> (IO ()))))))

foreign import ccall safe "gtk_widget_reset_shapes"
  gtk_widget_reset_shapes :: ((Ptr Widget) -> (IO ()))

foreign import ccall safe "gtk_widget_set_app_paintable"
  gtk_widget_set_app_paintable :: ((Ptr Widget) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_widget_set_double_buffered"
  gtk_widget_set_double_buffered :: ((Ptr Widget) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_widget_set_redraw_on_allocate"
  gtk_widget_set_redraw_on_allocate :: ((Ptr Widget) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_widget_set_composite_name"
  gtk_widget_set_composite_name :: ((Ptr Widget) -> ((Ptr CChar) -> (IO ())))

foreign import ccall safe "gtk_widget_mnemonic_activate"
  gtk_widget_mnemonic_activate :: ((Ptr Widget) -> (CInt -> (IO CInt)))

foreign import ccall safe "gtk_widget_set_scroll_adjustments"
  gtk_widget_set_scroll_adjustments :: ((Ptr Widget) -> ((Ptr Adjustment) -> ((Ptr Adjustment) -> (IO CInt))))

foreign import ccall safe "gtk_widget_region_intersect"
  gtk_widget_region_intersect :: ((Ptr Widget) -> ((Ptr Region) -> (IO (Ptr Region))))

foreign import ccall safe "gtk_widget_get_accessible"
  gtk_widget_get_accessible :: ((Ptr Widget) -> (IO (Ptr AtkObject)))

foreign import ccall safe "gtk_widget_child_focus"
  gtk_widget_child_focus :: ((Ptr Widget) -> (CInt -> (IO CInt)))

foreign import ccall safe "gtk_widget_get_child_visible"
  gtk_widget_get_child_visible :: ((Ptr Widget) -> (IO CInt))

foreign import ccall safe "gtk_widget_get_parent"
  gtk_widget_get_parent :: ((Ptr Widget) -> (IO (Ptr Widget)))

foreign import ccall safe "gtk_widget_get_settings"
  gtk_widget_get_settings :: ((Ptr Widget) -> (IO (Ptr Settings)))

foreign import ccall safe "gtk_widget_get_clipboard"
  gtk_widget_get_clipboard :: ((Ptr Widget) -> ((Ptr ()) -> (IO (Ptr Clipboard))))

foreign import ccall safe "gtk_widget_get_display"
  gtk_widget_get_display :: ((Ptr Widget) -> (IO (Ptr Display)))

foreign import ccall safe "gtk_widget_get_root_window"
  gtk_widget_get_root_window :: ((Ptr Widget) -> (IO (Ptr DrawWindow)))

foreign import ccall safe "gtk_widget_get_screen"
  gtk_widget_get_screen :: ((Ptr Widget) -> (IO (Ptr Screen)))

foreign import ccall safe "gtk_widget_has_screen"
  gtk_widget_has_screen :: ((Ptr Widget) -> (IO CInt))

foreign import ccall safe "gtk_widget_get_size_request"
  gtk_widget_get_size_request :: ((Ptr Widget) -> ((Ptr CInt) -> ((Ptr CInt) -> (IO ()))))

foreign import ccall safe "gtk_widget_set_child_visible"
  gtk_widget_set_child_visible :: ((Ptr Widget) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_widget_set_size_request"
  gtk_widget_set_size_request :: ((Ptr Widget) -> (CInt -> (CInt -> (IO ()))))

foreign import ccall safe "gtk_widget_set_no_show_all"
  gtk_widget_set_no_show_all :: ((Ptr Widget) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_widget_get_no_show_all"
  gtk_widget_get_no_show_all :: ((Ptr Widget) -> (IO CInt))

foreign import ccall safe "gtk_widget_list_mnemonic_labels"
  gtk_widget_list_mnemonic_labels :: ((Ptr Widget) -> (IO (Ptr ())))

foreign import ccall safe "gtk_widget_add_mnemonic_label"
  gtk_widget_add_mnemonic_label :: ((Ptr Widget) -> ((Ptr Widget) -> (IO ())))

foreign import ccall safe "gtk_widget_remove_mnemonic_label"
  gtk_widget_remove_mnemonic_label :: ((Ptr Widget) -> ((Ptr Widget) -> (IO ())))

foreign import ccall safe "gtk_widget_get_action"
  gtk_widget_get_action :: ((Ptr Widget) -> (IO (Ptr Action)))

foreign import ccall safe "gtk_widget_is_composited"
  gtk_widget_is_composited :: ((Ptr Widget) -> (IO CInt))

foreign import ccall safe "gtk_widget_error_bell"
  gtk_widget_error_bell :: ((Ptr Widget) -> (IO ()))

foreign import ccall safe "gtk_widget_keynav_failed"
  gtk_widget_keynav_failed :: ((Ptr Widget) -> (CInt -> (IO CInt)))

foreign import ccall safe "gtk_widget_get_tooltip_markup"
  gtk_widget_get_tooltip_markup :: ((Ptr Widget) -> (IO (Ptr CChar)))

foreign import ccall safe "gtk_widget_set_tooltip_markup"
  gtk_widget_set_tooltip_markup :: ((Ptr Widget) -> ((Ptr CChar) -> (IO ())))

foreign import ccall safe "gtk_widget_get_tooltip_text"
  gtk_widget_get_tooltip_text :: ((Ptr Widget) -> (IO (Ptr CChar)))

foreign import ccall safe "gtk_widget_set_tooltip_text"
  gtk_widget_set_tooltip_text :: ((Ptr Widget) -> ((Ptr CChar) -> (IO ())))

foreign import ccall safe "gtk_widget_get_tooltip_window"
  gtk_widget_get_tooltip_window :: ((Ptr Widget) -> (IO (Ptr Window)))

foreign import ccall safe "gtk_widget_set_tooltip_window"
  gtk_widget_set_tooltip_window :: ((Ptr Widget) -> ((Ptr Window) -> (IO ())))

foreign import ccall safe "gtk_widget_get_has_tooltip"
  gtk_widget_get_has_tooltip :: ((Ptr Widget) -> (IO CInt))

foreign import ccall safe "gtk_widget_set_has_tooltip"
  gtk_widget_set_has_tooltip :: ((Ptr Widget) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_widget_trigger_tooltip_query"
  gtk_widget_trigger_tooltip_query :: ((Ptr Widget) -> (IO ()))

foreign import ccall safe "gtk_widget_get_window"
  gtk_widget_get_window :: ((Ptr Widget) -> (IO (Ptr DrawWindow)))

foreign import ccall safe "gtk_widget_reparent"
  gtk_widget_reparent :: ((Ptr Widget) -> ((Ptr Widget) -> (IO ())))

foreign import ccall safe "gtk_widget_get_allocation"
  gtk_widget_get_allocation :: ((Ptr Widget) -> ((Ptr ()) -> (IO ())))

foreign import ccall safe "gtk_widget_get_app_paintable"
  gtk_widget_get_app_paintable :: ((Ptr Widget) -> (IO CInt))

foreign import ccall safe "gtk_widget_get_can_default"
  gtk_widget_get_can_default :: ((Ptr Widget) -> (IO CInt))

foreign import ccall safe "gtk_widget_set_can_default"
  gtk_widget_set_can_default :: ((Ptr Widget) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_widget_get_has_window"
  gtk_widget_get_has_window :: ((Ptr Widget) -> (IO CInt))

foreign import ccall safe "gtk_widget_set_has_window"
  gtk_widget_set_has_window :: ((Ptr Widget) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_widget_get_sensitive"
  gtk_widget_get_sensitive :: ((Ptr Widget) -> (IO CInt))

foreign import ccall safe "gtk_widget_is_sensitive"
  gtk_widget_is_sensitive :: ((Ptr Widget) -> (IO CInt))

foreign import ccall safe "gtk_widget_get_state"
  gtk_widget_get_state :: ((Ptr Widget) -> (IO CInt))

foreign import ccall safe "gtk_widget_get_visible"
  gtk_widget_get_visible :: ((Ptr Widget) -> (IO CInt))

foreign import ccall safe "gtk_widget_has_default"
  gtk_widget_has_default :: ((Ptr Widget) -> (IO CInt))

foreign import ccall safe "gtk_widget_has_focus"
  gtk_widget_has_focus :: ((Ptr Widget) -> (IO CInt))

foreign import ccall safe "gtk_widget_has_grab"
  gtk_widget_has_grab :: ((Ptr Widget) -> (IO CInt))

foreign import ccall safe "gtk_widget_is_drawable"
  gtk_widget_is_drawable :: ((Ptr Widget) -> (IO CInt))

foreign import ccall safe "gtk_widget_is_toplevel"
  gtk_widget_is_toplevel :: ((Ptr Widget) -> (IO CInt))

foreign import ccall safe "gtk_widget_set_window"
  gtk_widget_set_window :: ((Ptr Widget) -> ((Ptr DrawWindow) -> (IO ())))

foreign import ccall safe "gtk_widget_set_receives_default"
  gtk_widget_set_receives_default :: ((Ptr Widget) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_widget_get_receives_default"
  gtk_widget_get_receives_default :: ((Ptr Widget) -> (IO CInt))

foreign import ccall safe "gtk_widget_set_state"
  gtk_widget_set_state :: ((Ptr Widget) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_widget_event"
  gtk_widget_event :: ((Ptr Widget) -> ((Ptr ()) -> (IO CInt)))

foreign import ccall unsafe "gdk_event_mask_get_type"
  gdk_event_mask_get_type :: CULong

foreign import ccall safe "gtk_widget_has_rc_style"
  gtk_widget_has_rc_style :: ((Ptr Widget) -> (IO CInt))

foreign import ccall safe "gtk_widget_get_realized"
  gtk_widget_get_realized :: ((Ptr Widget) -> (IO CInt))

foreign import ccall safe "gtk_widget_get_mapped"
  gtk_widget_get_mapped :: ((Ptr Widget) -> (IO CInt))

foreign import ccall safe "gtk_widget_set_realized"
  gtk_widget_set_realized :: ((Ptr Widget) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_widget_set_mapped"
  gtk_widget_set_mapped :: ((Ptr Widget) -> (CInt -> (IO ())))
