
{-# LINE 2 "./Graphics/UI/Gtk/ActionMenuToolbar/UIManager.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget UIManager
--
-- Author : Duncan Coutts
--
-- Created: 6 April 2005
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
-- TODO
-- check if uiManagerGetToplevels returns widgets derived from some common
-- class, eg ToolItem (though it looks like it can return MenuBars too)
--
-- |
-- Maintainer : gtk2hs-users@lists.sourceforge.net
-- Stability : provisional
-- Portability : portable (depends on GHC)
--
-- Constructing menus and toolbars from an XML description
--
-- * Module available since Gtk+ version 2.4
--
module Graphics.UI.Gtk.ActionMenuToolbar.UIManager (
-- * Detail
--
-- | A 'UIManager' constructs a user interface (menus and toolbars) from one
-- or more UI definitions, which reference actions from one or more action
-- groups.

-- ** UI Definitions
--
-- | #XML-UI# The UI definitions are specified in an XML format which can be roughly
-- described by the following DTD.
--
-- > <!ELEMENT ui (menubar|toolbar|popup|accelerator)* >
-- > <!ELEMENT menubar (menuitem|separator|placeholder|menu)* >
-- > <!ELEMENT menu (menuitem|separator|placeholder|menu)* >
-- > <!ELEMENT popup (menuitem|separator|placeholder|menu)* >
-- > <!ELEMENT toolbar (toolitem|separator|placeholder)* >
-- > <!ELEMENT placeholder (menuitem|toolitem|separator|placeholder|menu)* >
-- > <!ELEMENT menuitem EMPTY >
-- > <!ELEMENT toolitem EMPTY >
-- > <!ELEMENT separator EMPTY >
-- > <!ELEMENT accelerator EMPTY >
-- > <!ATTLIST menubar name #IMPLIED
-- > action #IMPLIED >
-- > <!ATTLIST toolbar name #IMPLIED
-- > action #IMPLIED >
-- > <!ATTLIST popup name #IMPLIED
-- > action #IMPLIED >
-- > <!ATTLIST placeholder name #IMPLIED
-- > action #IMPLIED
-- > expand (true|false) #IMPLIED >
-- > <!ATTLIST separator name #IMPLIED
-- > action #IMPLIED >
-- > <!ATTLIST menu name #IMPLIED
-- > action #REQUIRED
-- > position (top|bot) #IMPLIED >
-- > <!ATTLIST menuitem name #IMPLIED
-- > action #REQUIRED
-- > position (top|bot) #IMPLIED >
-- > <!ATTLIST toolitem name #IMPLIED
-- > action #REQUIRED
-- > position (top|bot) #IMPLIED >
-- > <!ATTLIST accelerator name #IMPLIED
-- > action #REQUIRED >
--
-- There are some additional restrictions beyond those specified in the DTD,
-- e.g. every toolitem must have a toolbar in its anchestry and every menuitem
-- must have a menubar or popup in its anchestry. Since a GMarkup
-- parser is used to parse the UI description, it must not
-- only be valid XML, but valid GMarkup.
--
-- If a name is not specified, it defaults to the action. If an action is
-- not specified either, the element name is used. The name and action
-- attributes must not contain \'\/\' characters after parsing (since that
-- would mess up path lookup) and must be usable as XML attributes when
-- enclosed in doublequotes, thus they must not \'\"\' characters or references
-- to the &quot; entity.

-- ** A UI definition
-- |
--
-- > <ui>
-- > <menubar>
-- > <menu name="FileMenu" action="FileMenuAction">
-- > <menuitem name="New" action="New2Action" />
-- > <placeholder name="FileMenuAdditions" />
-- > </menu>
-- > <menu name="JustifyMenu" action="JustifyMenuAction">
-- > <menuitem name="Left" action="justify-left"/>
-- > <menuitem name="Centre" action="justify-center"/>
-- > <menuitem name="Right" action="justify-right"/>
-- > <menuitem name="Fill" action="justify-fill"/>
-- > </menu>
-- > </menubar>
-- > <toolbar action="toolbar1">
-- > <placeholder name="JustifyToolItems">
-- > <separator/>
-- > <toolitem name="Left" action="justify-left"/>
-- > <toolitem name="Centre" action="justify-center"/>
-- > <toolitem name="Right" action="justify-right"/>
-- > <toolitem name="Fill" action="justify-fill"/>
-- > <separator/>
-- > </placeholder>
-- > </toolbar>
-- > </ui>

-- The constructed widget hierarchy is very similar to the element tree of
-- the XML, with the exception that placeholders are merged into their parents.
-- The correspondence of XML elements to widgets should be almost obvious:
--
-- [menubar] a 'MenuBar'
--
-- [toolbar] a 'Toolbar'
--
-- [popup] a toplevel 'Menu'
--
-- [menu] a 'Menu' attached to a menuitem
--
-- [menuitem] a 'MenuItem' subclass, the exact type depends on the action
--
-- [toolitem] a 'ToolItem' subclass, the exact type depends on the action.
-- Note that toolitem elements may contain a menu element, but only if their
-- associated action specifies a 'MenuToolButton' as proxy.
--
-- [separator] a 'SeparatorMenuItem' or 'SeparatorToolItem'
--
-- [accelerator] a keyboard accelerator
--
-- The \"position\" attribute determines where a constructed widget is
-- positioned wrt. to its siblings in the partially constructed tree. If it is
-- \"top\", the widget is prepended, otherwise it is appended.

-- ** UI Merging
--
-- | The most remarkable feature of 'UIManager' is that it can overlay a set
-- of menuitems and toolitems over another one, and demerge them later.
--
-- Merging is done based on the names of the XML elements. Each element is
-- identified by a path which consists of the names of its anchestors,
-- separated by slashes. For example, the menuitem named \"Left\" in the
-- example above has the path @\/ui\/menubar\/JustifyMenu\/Left@ and the
-- toolitem with the same name has path
-- @\/ui\/toolbar1\/JustifyToolItems\/Left@.

-- ** Accelerators
--
-- | Every action has an accelerator path. Accelerators are installed together
-- with menuitem proxies, but they can also be explicitly added with
-- \<accelerator> elements in the UI definition. This makes it possible to have
-- accelerators for actions even if they have no visible proxies.

-- ** Smart Separators
--
-- | The separators created by 'UIManager' are \"smart\", i.e. they do not
-- show up in the UI unless they end up between two visible menu or tool items.
-- Separators which are located at the very beginning or end of the menu or
-- toolbar containing them, or multiple separators next to each other, are
-- hidden. This is a useful feature, since the merging of UI elements from
-- multiple sources can make it hard or impossible to determine in advance
-- whether a separator will end up in such an unfortunate position.
--
-- For separators in toolbars, you can set @expand=\"true\"@ to turn them
-- from a small, visible separator to an expanding, invisible one. Toolitems
-- following an expanding separator are effectively right-aligned.

-- ** Empty Menus
--
-- | Submenus pose similar problems to separators inconnection with merging.
-- It is impossible to know in advance whether they will end up empty after
-- merging. 'UIManager' offers two ways to treat empty submenus:
--
-- * make them disappear by hiding the menu item they\'re attached to
--
-- * add an insensitive \"Empty\" item
--
-- The behaviour is chosen based on the \"hide_if_empty\" property of the
-- action to which the submenu is associated.
--

-- * Class Hierarchy
-- |
-- @
-- | 'GObject'
-- | +----UIManager
-- @


-- * Types
  UIManager,
  UIManagerClass,
  castToUIManager, gTypeUIManager,
  toUIManager,
  UIManagerItemType(..),
  MergeId,

-- * Constructors
  uiManagerNew,

-- * Methods
  uiManagerSetAddTearoffs,
  uiManagerGetAddTearoffs,
  uiManagerInsertActionGroup,
  uiManagerRemoveActionGroup,
  uiManagerGetActionGroups,
  uiManagerGetAccelGroup,
  uiManagerGetWidget,
  uiManagerGetToplevels,
  uiManagerGetAction,
  uiManagerAddUiFromString,
  uiManagerAddUiFromFile,
  uiManagerNewMergeId,
  uiManagerAddUi,
  uiManagerRemoveUi,
  uiManagerGetUi,
  uiManagerEnsureUpdate,

-- * Attributes
  uiManagerAddTearoffs,
  uiManagerUi,

-- * Signals
  addWidget,
  actionsChanged,

-- * Deprecated

  onAddWidget,
  afterAddWidget,
  onActionsChanged,
  afterActionsChanged,
  onConnectProxy,
  afterConnectProxy,
  onDisconnectProxy,
  afterDisconnectProxy,
  onPreActivate,
  afterPreActivate,
  onPostActivate,
  afterPostActivate,


  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.Flags (Flags, fromFlags)
import System.Glib.UTFString
import System.Glib.GList
import System.Glib.GError
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.UI.Gtk.Types
{-# LINE 267 "./Graphics/UI/Gtk/ActionMenuToolbar/UIManager.chs" #-}
import Graphics.UI.Gtk.Signals
{-# LINE 268 "./Graphics/UI/Gtk/ActionMenuToolbar/UIManager.chs" #-}


{-# LINE 270 "./Graphics/UI/Gtk/ActionMenuToolbar/UIManager.chs" #-}


-- | These enumeration values are used by 'uiManagerAddUi' to determine what UI
-- element to create.
--
data UIManagerItemType = UiManagerAuto
                       | UiManagerMenubar
                       | UiManagerMenu
                       | UiManagerToolbar
                       | UiManagerPlaceholder
                       | UiManagerPopup
                       | UiManagerMenuitem
                       | UiManagerToolitem
                       | UiManagerSeparator
                       | UiManagerAccelerator
                       | UiManagerPopupWithAccels
                       deriving (Bounded)
instance Enum UIManagerItemType where
  fromEnum UiManagerAuto = 0
  fromEnum UiManagerMenubar = 1
  fromEnum UiManagerMenu = 2
  fromEnum UiManagerToolbar = 4
  fromEnum UiManagerPlaceholder = 8
  fromEnum UiManagerPopup = 16
  fromEnum UiManagerMenuitem = 32
  fromEnum UiManagerToolitem = 64
  fromEnum UiManagerSeparator = 128
  fromEnum UiManagerAccelerator = 256
  fromEnum UiManagerPopupWithAccels = 512

  toEnum 0 = UiManagerAuto
  toEnum 1 = UiManagerMenubar
  toEnum 2 = UiManagerMenu
  toEnum 4 = UiManagerToolbar
  toEnum 8 = UiManagerPlaceholder
  toEnum 16 = UiManagerPopup
  toEnum 32 = UiManagerMenuitem
  toEnum 64 = UiManagerToolitem
  toEnum 128 = UiManagerSeparator
  toEnum 256 = UiManagerAccelerator
  toEnum 512 = UiManagerPopupWithAccels
  toEnum unmatched = error ("UIManagerItemType.toEnum: Cannot match " ++ show unmatched)

  succ UiManagerAuto = UiManagerMenubar
  succ UiManagerMenubar = UiManagerMenu
  succ UiManagerMenu = UiManagerToolbar
  succ UiManagerToolbar = UiManagerPlaceholder
  succ UiManagerPlaceholder = UiManagerPopup
  succ UiManagerPopup = UiManagerMenuitem
  succ UiManagerMenuitem = UiManagerToolitem
  succ UiManagerToolitem = UiManagerSeparator
  succ UiManagerSeparator = UiManagerAccelerator
  succ UiManagerAccelerator = UiManagerPopupWithAccels
  succ _ = undefined

  pred UiManagerMenubar = UiManagerAuto
  pred UiManagerMenu = UiManagerMenubar
  pred UiManagerToolbar = UiManagerMenu
  pred UiManagerPlaceholder = UiManagerToolbar
  pred UiManagerPopup = UiManagerPlaceholder
  pred UiManagerMenuitem = UiManagerPopup
  pred UiManagerToolitem = UiManagerMenuitem
  pred UiManagerSeparator = UiManagerToolitem
  pred UiManagerAccelerator = UiManagerSeparator
  pred UiManagerPopupWithAccels = UiManagerAccelerator
  pred _ = undefined

  enumFromTo x y | fromEnum x == fromEnum y = [ y ]
                 | otherwise = x : enumFromTo (succ x) y
  enumFrom x = enumFromTo x UiManagerPopupWithAccels
  enumFromThen _ _ =     error "Enum UIManagerItemType: enumFromThen not implemented"
  enumFromThenTo _ _ _ =     error "Enum UIManagerItemType: enumFromThenTo not implemented"

{-# LINE 276 "./Graphics/UI/Gtk/ActionMenuToolbar/UIManager.chs" #-}
instance Flags UIManagerItemType

newtype MergeId = MergeId { fromMergeId :: (CUInt)}

--------------------
-- Constructors

-- | Creates a new ui manager object.
--
uiManagerNew :: IO UIManager
uiManagerNew =
  wrapNewGObject mkUIManager $
  gtk_ui_manager_new
{-# LINE 289 "./Graphics/UI/Gtk/ActionMenuToolbar/UIManager.chs" #-}

--------------------
-- Methods

-- | Returns an unused merge id, suitable for use with 'uiManagerAddUi'.
--
uiManagerNewMergeId :: UIManager -> IO MergeId
uiManagerNewMergeId self =
  liftM MergeId $
  (\(UIManager arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_ui_manager_new_merge_id argPtr1)
{-# LINE 299 "./Graphics/UI/Gtk/ActionMenuToolbar/UIManager.chs" #-}
    self

-- | Sets the \"add_tearoffs\" property, which controls whether menus
-- generated by this 'UIManager' will have tearoff menu items.
--
-- Note that this only affects regular menus. Generated popup menus never
-- have tearoff menu items.
--
uiManagerSetAddTearoffs :: UIManager
 -> Bool -- ^ @addTearoffs@ - whether tearoff menu items are added
 -> IO ()
uiManagerSetAddTearoffs self addTearoffs =
  (\(UIManager arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_ui_manager_set_add_tearoffs argPtr1 arg2)
{-# LINE 312 "./Graphics/UI/Gtk/ActionMenuToolbar/UIManager.chs" #-}
    self
    (fromBool addTearoffs)

-- | Returns whether menus generated by this 'UIManager' will have tearoff
-- menu items.
--
uiManagerGetAddTearoffs :: UIManager
 -> IO Bool -- ^ returns whether tearoff menu items are added
uiManagerGetAddTearoffs self =
  liftM toBool $
  (\(UIManager arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_ui_manager_get_add_tearoffs argPtr1)
{-# LINE 323 "./Graphics/UI/Gtk/ActionMenuToolbar/UIManager.chs" #-}
    self

-- | Inserts an action group into the list of action groups associated with
-- @self@. Actions in earlier groups hide actions with the same name in later
-- groups.
--
uiManagerInsertActionGroup :: UIManager
 -> ActionGroup -- ^ @actionGroup@ - the action group to be inserted
 -> Int -- ^ @pos@ - the position at which the group will be inserted.
 -> IO ()
uiManagerInsertActionGroup self actionGroup pos =
  (\(UIManager arg1) (ActionGroup arg2) arg3 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_ui_manager_insert_action_group argPtr1 argPtr2 arg3)
{-# LINE 335 "./Graphics/UI/Gtk/ActionMenuToolbar/UIManager.chs" #-}
    self
    actionGroup
    (fromIntegral pos)

-- | Removes an action group from the list of action groups associated with
-- @self@.
--
uiManagerRemoveActionGroup :: UIManager
 -> ActionGroup -- ^ @actionGroup@ - the action group to be removed
 -> IO ()
uiManagerRemoveActionGroup self actionGroup =
  (\(UIManager arg1) (ActionGroup arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_ui_manager_remove_action_group argPtr1 argPtr2)
{-# LINE 347 "./Graphics/UI/Gtk/ActionMenuToolbar/UIManager.chs" #-}
    self
    actionGroup

-- | Returns the list of action groups associated with the UI manager.
--
uiManagerGetActionGroups :: UIManager -> IO [ActionGroup]
uiManagerGetActionGroups self =
  (\(UIManager arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_ui_manager_get_action_groups argPtr1)
{-# LINE 355 "./Graphics/UI/Gtk/ActionMenuToolbar/UIManager.chs" #-}
    self
  >>= readGList
  >>= mapM (\elemPtr -> makeNewGObject mkActionGroup (return elemPtr))

-- | Returns the 'AccelGroup' associated with @self@.
--
uiManagerGetAccelGroup :: UIManager
 -> IO AccelGroup -- ^ returns the 'AccelGroup'.
uiManagerGetAccelGroup self =
  makeNewGObject mkAccelGroup $
  (\(UIManager arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_ui_manager_get_accel_group argPtr1)
{-# LINE 366 "./Graphics/UI/Gtk/ActionMenuToolbar/UIManager.chs" #-}
    self

-- | Looks up a widget by following a path. The path consists of the names
-- specified in the XML description of the UI. separated by \'\/\'. Elements
-- which don't have a name or action attribute in the XML (e.g. \<popup>) can
-- be addressed by their XML element name (e.g. \"popup\"). The root element
-- (\"\/ui\") can be omitted in the path.
--
-- Note that the widget found by following a path that ends in a \<menu>
-- element is the menuitem to which the menu is attached, not the menu itself.
--
uiManagerGetWidget :: GlibString string => UIManager
 -> string -- ^ @path@ - a path
 -> IO (Maybe Widget) -- ^ returns the widget found by following the path, or
                      -- @Nothing@ if no widget was found.
uiManagerGetWidget self path =
  maybeNull (makeNewObject mkWidget) $
  withUTFString path $ \pathPtr ->
  (\(UIManager arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_ui_manager_get_widget argPtr1 arg2)
{-# LINE 385 "./Graphics/UI/Gtk/ActionMenuToolbar/UIManager.chs" #-}
    self
    pathPtr

-- | Obtains a list of all toplevel widgets of the requested types.
--
uiManagerGetToplevels :: UIManager
 -> [UIManagerItemType] -- ^ @types@ - specifies the types of toplevel
                            -- widgets to include. Allowed types are
                            -- 'UiManagerMenubar', 'UiManagerToolbar' and
                            -- 'UiManagerPopup'.
 -> IO [Widget] -- ^ returns a list of all toplevel
                            -- widgets of the requested types.
uiManagerGetToplevels self types =
  (\(UIManager arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_ui_manager_get_toplevels argPtr1 arg2)
{-# LINE 399 "./Graphics/UI/Gtk/ActionMenuToolbar/UIManager.chs" #-}
    self
    ((fromIntegral . fromFlags) types)
  >>= fromGSList
  >>= mapM (\elemPtr -> makeNewObject mkWidget (return elemPtr))

-- | Looks up an action by following a path. See 'uiManagerGetWidget' for more
-- information about paths.
--
uiManagerGetAction :: GlibString string => UIManager
 -> string -- ^ @path@ - a path
 -> IO (Maybe Action) -- ^ returns the action whose proxy widget is found by
                      -- following the path, or @Nothing@ if no widget was
                      -- found.
uiManagerGetAction self path =
  maybeNull (makeNewGObject mkAction) $
  withUTFString path $ \pathPtr ->
  (\(UIManager arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_ui_manager_get_action argPtr1 arg2)
{-# LINE 416 "./Graphics/UI/Gtk/ActionMenuToolbar/UIManager.chs" #-}
    self
    pathPtr

-- | Parses a string containing a UI definition and merges it with the current
-- contents of @self@. An enclosing \<ui> element is added if it is missing.
--
-- If a parse error occurres, an exception is thrown.
--
uiManagerAddUiFromString :: GlibString string => UIManager
 -> string -- ^ @buffer@ - the string to parse
 -> IO MergeId -- ^ returns The merge id for the merged UI. The merge id can be
               -- used to unmerge the UI with 'uiManagerRemoveUi'.
uiManagerAddUiFromString self buffer =
  liftM MergeId $
  propagateGError $ \errorPtr ->
  withUTFStringLen buffer $ \(bufferPtr, length) ->
  (\(UIManager arg1) arg2 arg3 arg4 -> withForeignPtr arg1 $ \argPtr1 ->gtk_ui_manager_add_ui_from_string argPtr1 arg2 arg3 arg4)
{-# LINE 433 "./Graphics/UI/Gtk/ActionMenuToolbar/UIManager.chs" #-}
    self
    bufferPtr
    (fromIntegral length)
    errorPtr

-- | Parses a file containing a UI definition and merges it with the current
-- contents of @self@.
--
-- If a parse or IO error occurres, an exception is thrown.
--
uiManagerAddUiFromFile :: GlibString string => UIManager
 -> string -- ^ @filename@ - the name of the file to parse
 -> IO MergeId -- ^ returns The merge id for the merged UI. The merge id can be
              -- used to unmerge the UI with 'uiManagerRemoveUi'.
uiManagerAddUiFromFile self filename =
  liftM MergeId $
  propagateGError $ \errorPtr ->
  withUTFString filename $ \filenamePtr ->



  (\(UIManager arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_ui_manager_add_ui_from_file argPtr1 arg2 arg3)
{-# LINE 455 "./Graphics/UI/Gtk/ActionMenuToolbar/UIManager.chs" #-}

    self
    filenamePtr
    errorPtr

-- | Adds a UI element to the current contents of @self@.
--
-- If @type@ is 'UiManagerAuto', Gtk+ inserts a menuitem, toolitem or
-- separator if such an element can be inserted at the place determined by
-- @path@. Otherwise @type@ must indicate an element that can be inserted at
-- the place determined by @path@.
--
-- If @path@ points to a menuitem or toolitem, the new element will be
-- inserted before or after this item, depending on @top@.
--
uiManagerAddUi :: GlibString string => UIManager
 -> MergeId -- ^ @mergeId@ - the merge id for the merged UI, see
                        -- 'uiManagerNewMergeId'
 -> string -- ^ @path@ - a path
 -> string -- ^ @name@ - the name for the added UI element
 -> Maybe string -- ^ @action@ - the name of the action to be proxied,
                        -- or @Nothing@ to add a separator
 -> [UIManagerItemType] -- ^ @type@ - the type of UI element to add.
 -> Bool -- ^ @top@ - if @True@, the UI element is added before
                        -- its siblings, otherwise it is added after its
                        -- siblings.
 -> IO ()
uiManagerAddUi self mergeId path name action type_ top =
  maybeWith withUTFString action $ \actionPtr ->
  withUTFString name $ \namePtr ->
  withUTFString path $ \pathPtr ->
  (\(UIManager arg1) arg2 arg3 arg4 arg5 arg6 arg7 -> withForeignPtr arg1 $ \argPtr1 ->gtk_ui_manager_add_ui argPtr1 arg2 arg3 arg4 arg5 arg6 arg7)
{-# LINE 487 "./Graphics/UI/Gtk/ActionMenuToolbar/UIManager.chs" #-}
    self
    (fromMergeId mergeId)
    pathPtr
    namePtr
    actionPtr
    ((fromIntegral . fromFlags) type_)
    (fromBool top)

-- | Unmerges the part of the UI manager's content identified by @mergeId@.
--
uiManagerRemoveUi :: UIManager
 -> MergeId -- ^ @mergeId@ - a merge id as returned by
              -- 'uiManagerAddUiFromString'
 -> IO ()
uiManagerRemoveUi self mergeId =
  (\(UIManager arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_ui_manager_remove_ui argPtr1 arg2)
{-# LINE 503 "./Graphics/UI/Gtk/ActionMenuToolbar/UIManager.chs" #-}
    self
    (fromMergeId mergeId)

-- | Creates a UI definition of the merged UI.
--
uiManagerGetUi :: GlibString string => UIManager
 -> IO string -- ^ returns string containing an XML representation of the
              -- merged UI.
uiManagerGetUi self =
  (\(UIManager arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_ui_manager_get_ui argPtr1)
{-# LINE 513 "./Graphics/UI/Gtk/ActionMenuToolbar/UIManager.chs" #-}
    self
  >>= readUTFString

-- | Makes sure that all pending updates to the UI have been completed.
--
-- This may occasionally be necessary, since 'UIManager' updates the UI in
-- an idle function. A typical example where this function is useful is to
-- enforce that the menubar and toolbar have been added to the main window
-- before showing it:
--
-- > do
-- > containerAdd window vbox
-- > onAddWidget merge (addWidget vbox)
-- > uiManagerAddUiFromFile merge "my-menus"
-- > uiManagerAddUiFromFile merge "my-toolbars"
-- > uiManagerEnsureUpdate merge
-- > widgetShow window
--
uiManagerEnsureUpdate :: UIManager -> IO ()
uiManagerEnsureUpdate self =
  (\(UIManager arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_ui_manager_ensure_update argPtr1)
{-# LINE 534 "./Graphics/UI/Gtk/ActionMenuToolbar/UIManager.chs" #-}
    self

--------------------
-- Attributes

-- | The \"add-tearoffs\" property controls whether generated menus have
-- tearoff menu items.
--
-- Note that this only affects regular menus. Generated popup menus never
-- have tearoff menu items.
--
-- Default value: @False@
--
uiManagerAddTearoffs :: Attr UIManager Bool
uiManagerAddTearoffs = newAttr
  uiManagerGetAddTearoffs
  uiManagerSetAddTearoffs

-- | An XML string describing the merged UI.
--
-- Default value: @\"\<ui\>\\n\<\/ui\>\\n\"@
--
uiManagerUi :: GlibString string => ReadAttr UIManager string
uiManagerUi = readAttrFromStringProperty "ui"

--------------------
-- Signals

-- %hash c:58ec d:2a79
-- | The add_widget signal is emitted for each generated menubar and toolbar.
-- It is not emitted for generated popup menus, which can be obtained by
-- 'uiManagerGetWidget'.
--
addWidget :: UIManagerClass self => Signal self (Widget -> IO ())
addWidget = Signal (connect_OBJECT__NONE "add-widget")

-- %hash c:2480 d:366c
-- | The \"actions-changed\" signal is emitted whenever the set of actions
-- changes.
--
actionsChanged :: UIManagerClass self => Signal self (IO ())
actionsChanged = Signal (connect_NONE__NONE "actions-changed")

--------------------
-- Deprecated Signals


-- | The add_widget signal is emitted for each generated menubar and toolbar.
-- It is not emitted for generated popup menus, which can be obtained by
-- 'uiManagerGetWidget'.
--
onAddWidget, afterAddWidget :: UIManagerClass self => self
 -> (Widget -> IO ())
 -> IO (ConnectId self)
onAddWidget = connect_OBJECT__NONE "add_widget" False
afterAddWidget = connect_OBJECT__NONE "add_widget" True

-- | The \"actions-changed\" signal is emitted whenever the set of actions
-- changes.
--
onActionsChanged, afterActionsChanged :: UIManagerClass self => self
 -> IO ()
 -> IO (ConnectId self)
onActionsChanged = connect_NONE__NONE "actions_changed" False
afterActionsChanged = connect_NONE__NONE "actions_changed" True

-- | The connect_proxy signal is emitted after connecting a proxy to an action
-- in the group.
--
-- This is intended for simple customizations for which a custom action
-- class would be too clumsy, e.g. showing tooltips for menuitems in the
-- statusbar.
--
onConnectProxy, afterConnectProxy :: UIManagerClass self => self
 -> (Action -> Widget -> IO ())
 -> IO (ConnectId self)
onConnectProxy = connect_OBJECT_OBJECT__NONE "connect_proxy" False
afterConnectProxy = connect_OBJECT_OBJECT__NONE "connect_proxy" True

-- | The disconnect_proxy signal is emitted after disconnecting a proxy from
-- an action in the group.
--
onDisconnectProxy, afterDisconnectProxy :: UIManagerClass self => self
 -> (Action -> Widget -> IO ())
 -> IO (ConnectId self)
onDisconnectProxy = connect_OBJECT_OBJECT__NONE "disconnect_proxy" False
afterDisconnectProxy = connect_OBJECT_OBJECT__NONE "disconnect_proxy" True

-- | The pre_activate signal is emitted just before the @action@ is activated.
--
-- This is intended for applications to get notification just before any
-- action is activated.
--
onPreActivate, afterPreActivate :: UIManagerClass self => self
 -> (Action -> IO ())
 -> IO (ConnectId self)
onPreActivate = connect_OBJECT__NONE "pre_activate" False
afterPreActivate = connect_OBJECT__NONE "pre_activate" True

-- | The post_activate signal is emitted just after the @action@ is activated.
--
-- This is intended for applications to get notification just after any
-- action is activated.
--
onPostActivate, afterPostActivate :: UIManagerClass self => self
 -> (Action -> IO ())
 -> IO (ConnectId self)
onPostActivate = connect_OBJECT__NONE "post_activate" False
afterPostActivate = connect_OBJECT__NONE "post_activate" True

foreign import ccall safe "gtk_ui_manager_new"
  gtk_ui_manager_new :: (IO (Ptr UIManager))

foreign import ccall safe "gtk_ui_manager_new_merge_id"
  gtk_ui_manager_new_merge_id :: ((Ptr UIManager) -> (IO CUInt))

foreign import ccall safe "gtk_ui_manager_set_add_tearoffs"
  gtk_ui_manager_set_add_tearoffs :: ((Ptr UIManager) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_ui_manager_get_add_tearoffs"
  gtk_ui_manager_get_add_tearoffs :: ((Ptr UIManager) -> (IO CInt))

foreign import ccall safe "gtk_ui_manager_insert_action_group"
  gtk_ui_manager_insert_action_group :: ((Ptr UIManager) -> ((Ptr ActionGroup) -> (CInt -> (IO ()))))

foreign import ccall safe "gtk_ui_manager_remove_action_group"
  gtk_ui_manager_remove_action_group :: ((Ptr UIManager) -> ((Ptr ActionGroup) -> (IO ())))

foreign import ccall safe "gtk_ui_manager_get_action_groups"
  gtk_ui_manager_get_action_groups :: ((Ptr UIManager) -> (IO (Ptr ())))

foreign import ccall safe "gtk_ui_manager_get_accel_group"
  gtk_ui_manager_get_accel_group :: ((Ptr UIManager) -> (IO (Ptr AccelGroup)))

foreign import ccall safe "gtk_ui_manager_get_widget"
  gtk_ui_manager_get_widget :: ((Ptr UIManager) -> ((Ptr CChar) -> (IO (Ptr Widget))))

foreign import ccall safe "gtk_ui_manager_get_toplevels"
  gtk_ui_manager_get_toplevels :: ((Ptr UIManager) -> (CInt -> (IO (Ptr ()))))

foreign import ccall safe "gtk_ui_manager_get_action"
  gtk_ui_manager_get_action :: ((Ptr UIManager) -> ((Ptr CChar) -> (IO (Ptr Action))))

foreign import ccall safe "gtk_ui_manager_add_ui_from_string"
  gtk_ui_manager_add_ui_from_string :: ((Ptr UIManager) -> ((Ptr CChar) -> (CLong -> ((Ptr (Ptr ())) -> (IO CUInt)))))

foreign import ccall safe "gtk_ui_manager_add_ui_from_file"
  gtk_ui_manager_add_ui_from_file :: ((Ptr UIManager) -> ((Ptr CChar) -> ((Ptr (Ptr ())) -> (IO CUInt))))

foreign import ccall safe "gtk_ui_manager_add_ui"
  gtk_ui_manager_add_ui :: ((Ptr UIManager) -> (CUInt -> ((Ptr CChar) -> ((Ptr CChar) -> ((Ptr CChar) -> (CInt -> (CInt -> (IO ()))))))))

foreign import ccall safe "gtk_ui_manager_remove_ui"
  gtk_ui_manager_remove_ui :: ((Ptr UIManager) -> (CUInt -> (IO ())))

foreign import ccall safe "gtk_ui_manager_get_ui"
  gtk_ui_manager_get_ui :: ((Ptr UIManager) -> (IO (Ptr CChar)))

foreign import ccall safe "gtk_ui_manager_ensure_update"
  gtk_ui_manager_ensure_update :: ((Ptr UIManager) -> (IO ()))
