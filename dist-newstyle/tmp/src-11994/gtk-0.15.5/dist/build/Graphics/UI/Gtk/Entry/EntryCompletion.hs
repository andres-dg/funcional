
{-# LINE 2 "./Graphics/UI/Gtk/Entry/EntryCompletion.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget EntryCompletion
--
-- Author : Duncan Coutts
--
-- Created: 24 April 2004
--
-- Copyright (C) 2004-2005 Duncan Coutts
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
-- Completion functionality for 'Entry'
--
-- * Module available since Gtk+ version 2.4
--
module Graphics.UI.Gtk.Entry.EntryCompletion (
-- * Detail
--
-- | 'EntryCompletion' is an auxiliary object to be used in conjunction with
-- 'Entry' to provide the completion functionality. It implements the
-- 'CellLayout' interface, to allow the user to add extra cells to the
-- 'TreeView' with completion matches.
--
-- \"Completion functionality\" means that when the user modifies the
-- text in the entry, 'EntryCompletion' checks which rows in the model
-- match the current content of the entry, and displays a list of
-- matches. By default, the matching is done by comparing the entry
-- text case-insensitively against the text in a model (see
-- 'Graphics.UI.Gtk.Entry.entryCompletionSetTextModel'), but this can
-- be overridden with a custom match function (see
-- 'entryCompletionSetMatchFunc').
--
-- When the user selects a completion, the content of the entry is updated.
-- By default, the content of the entry is replaced by the text column of the
-- model, but this can be overridden by connecting to the 'matchSelected'
-- signal and updating the entry in the signal handler. Note that you should
-- return @True@ from the signal handler to suppress the default behaviour.
--
-- To add completion functionality to an entry, use
-- 'Graphics.UI.Gtk.Entry.Entry.entrySetCompletion'.
--
-- In addition to regular completion matches, which will be inserted into
-- the entry when they are selected, 'EntryCompletion' also allows to display
-- \"actions\" in the popup window. Their appearance is similar to menu items,
-- to differentiate them clearly from completion strings. When an action is
-- selected, the 'completionActionActivated' signal is emitted.

-- * Class Hierarchy
-- |
-- @
-- | 'GObject'
-- | +----EntryCompletion
-- @


-- * Types
  EntryCompletion,
  EntryCompletionClass,
  castToEntryCompletion, gTypeEntryCompletion,
  toEntryCompletion,

-- * Constructors
  entryCompletionNew,

-- * Methods
  entryCompletionGetEntry,
  entryCompletionSetModel,
  entryCompletionGetModel,
  entryCompletionSetTextModel,
  entryCompletionSetMatchFunc,
  entryCompletionSetMinimumKeyLength,
  entryCompletionGetMinimumKeyLength,
  entryCompletionComplete,
  entryCompletionInsertActionText,
  entryCompletionInsertActionMarkup,
  entryCompletionDeleteAction,
  entryCompletionSetTextColumn,

  entryCompletionInsertPrefix,
  entryCompletionGetTextColumn,
  entryCompletionSetInlineCompletion,
  entryCompletionGetInlineCompletion,
  entryCompletionSetPopupCompletion,
  entryCompletionGetPopupCompletion,


  entryCompletionSetPopupSetWidth,
  entryCompletionGetPopupSetWidth,
  entryCompletionSetPopupSingleMatch,
  entryCompletionGetPopupSingleMatch,


-- * Attributes
  entryCompletionModel,
  entryCompletionMinimumKeyLength,

  entryCompletionTextColumn,
  entryCompletionInlineCompletion,
  entryCompletionPopupCompletion,


  entryCompletionPopupSetWidth,
  entryCompletionPopupSingleMatch,


-- * Signals

  insertPrefix,

  completionActionActivated,
  matchSelected,

-- * Deprecated


  onInsertPrefix,
  afterInsertPrefix,

  onActionActivated,
  afterActionActivated,


  ) where

import Control.Monad (liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import Graphics.UI.Gtk.Abstract.Object (makeNewObject)
import Graphics.UI.Gtk.Types
{-# LINE 146 "./Graphics/UI/Gtk/Entry/EntryCompletion.chs" #-}
import Graphics.UI.Gtk.Signals
{-# LINE 147 "./Graphics/UI/Gtk/Entry/EntryCompletion.chs" #-}
import Graphics.UI.Gtk.ModelView.Types (TreeIter, peekTreeIter,
                                            TypedTreeModelClass)
import Graphics.UI.Gtk.ModelView.CustomStore (customStoreSetColumn)
import Graphics.UI.Gtk.ModelView.TreeModel (ColumnId(..),
                                                makeColumnIdString,
                                                columnIdToNumber)


{-# LINE 155 "./Graphics/UI/Gtk/Entry/EntryCompletion.chs" #-}


--------------------
-- Constructors

-- | Creates a new 'EntryCompletion' object.
--
entryCompletionNew :: IO EntryCompletion
entryCompletionNew =
  wrapNewGObject mkEntryCompletion $
  gtk_entry_completion_new
{-# LINE 166 "./Graphics/UI/Gtk/Entry/EntryCompletion.chs" #-}

--------------------
-- Methods

-- | Gets the entry @completion@ has been attached to.
--
entryCompletionGetEntry :: EntryCompletion
 -> IO (Maybe Entry) -- ^ returns the entry @completion@ has been attached to.
entryCompletionGetEntry self =
  maybeNull (makeNewObject mkEntry) $
  liftM (castPtr :: Ptr Widget -> Ptr Entry) $
  (\(EntryCompletion arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_entry_completion_get_entry argPtr1)
{-# LINE 178 "./Graphics/UI/Gtk/Entry/EntryCompletion.chs" #-}
    self

-- | Sets the model for a 'EntryCompletion'. If @completion@ already has a
-- model set, it will remove it before setting the new model. If model is
-- @Nothing@, then it will unset the model.
--
entryCompletionSetModel :: TreeModelClass model => EntryCompletion
 -> Maybe model -- ^ @model@ - The 'TreeModel'.
 -> IO ()
entryCompletionSetModel self model =
  (\(EntryCompletion arg1) (TreeModel arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gtk_entry_completion_set_model argPtr1 argPtr2)
{-# LINE 189 "./Graphics/UI/Gtk/Entry/EntryCompletion.chs" #-}
    self
    (maybe (TreeModel nullForeignPtr) toTreeModel model)

-- | Returns the model the 'EntryCompletion' is using as data source. Returns
-- @Nothing@ if the model is unset.
--
entryCompletionGetModel :: EntryCompletion
 -> IO (Maybe TreeModel) -- ^ returns A 'TreeModel', or @Nothing@ if none is
                         -- currently being used.
entryCompletionGetModel self =
  maybeNull (makeNewGObject mkTreeModel) $
  (\(EntryCompletion arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_entry_completion_get_model argPtr1)
{-# LINE 201 "./Graphics/UI/Gtk/Entry/EntryCompletion.chs" #-}
    self

-- | Convenience function for setting up the most used case of this code: a
-- completion list with just strings. This function will set up @completion@ to
-- have a list displaying all (and just) strings in the completion list, and to
-- get those strings from @model@. This functions creates and adds a
-- 'CellRendererText' which retrieves its content from the given model.
--
entryCompletionSetTextModel :: (TreeModelClass (model string),
                                TypedTreeModelClass model, GlibString string)
 => EntryCompletion -- ^ @completion@
 -> model string -- ^ the model containing 'string's
 -> IO ()
entryCompletionSetTextModel self model = do
  let strCol = makeColumnIdString 0
  customStoreSetColumn model strCol id
  set self [entryCompletionTextColumn := strCol]

-- | Sets the match function for @completion@ to be @func@. The match function
-- is used to determine if a row should or should not be in the completion
-- list.
--
-- * The passed-in function decides whether the row indicated by the
-- 'TreeIter' matches a given key, and should be displayed as a possible
-- completion for key. Note that the key is normalized and case-folded.
-- Normalization will standardizing such issues as whether a character
-- with an accent is represented as a base character and combining accent
-- or as a single precomposed character. If this is not appropriate you
-- can extract the original text from the entry.
--
entryCompletionSetMatchFunc :: GlibString string
 => EntryCompletion -> (string -> TreeIter -> IO Bool) -> IO ()
entryCompletionSetMatchFunc ec handler = do
  hPtr <- mkHandler_GtkEntryCompletionMatchFunc
    (\_ keyPtr iterPtr _ -> do key <- peekUTFString keyPtr
                               iter <- peek iterPtr
                               liftM fromBool $ handler key iter)
  (\(EntryCompletion arg1) arg2 arg3 arg4 -> withForeignPtr arg1 $ \argPtr1 ->gtk_entry_completion_set_match_func argPtr1 arg2 arg3 arg4) ec
    (castFunPtr hPtr) (castFunPtrToPtr hPtr) destroyFunPtr

-------------------------------------------------
-- Callback stuff for entryCompletionSetMatchFunc
--

type GtkEntryCompletionMatchFunc =
  Ptr EntryCompletion -> --GtkEntryCompletion *completion
  Ptr CChar -> --const gchar *key
  Ptr TreeIter -> --GtkTreeIter *iter
  Ptr () -> --gpointer user_data
  IO (CInt)
{-# LINE 251 "./Graphics/UI/Gtk/Entry/EntryCompletion.chs" #-}

foreign import ccall "wrapper" mkHandler_GtkEntryCompletionMatchFunc ::
  GtkEntryCompletionMatchFunc ->
  IO (FunPtr GtkEntryCompletionMatchFunc)

-- | Requires the length of the search key for @completion@ to be at least
-- @length@. This is useful for long lists, where completing using a small key
-- takes a lot of time and will come up with meaningless results anyway (ie, a
-- too large dataset).
--
entryCompletionSetMinimumKeyLength :: EntryCompletion
 -> Int -- ^ @length@ - The minimum length of the key in order to
                    -- start completing.
 -> IO ()
entryCompletionSetMinimumKeyLength self length =
  (\(EntryCompletion arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_entry_completion_set_minimum_key_length argPtr1 arg2)
{-# LINE 267 "./Graphics/UI/Gtk/Entry/EntryCompletion.chs" #-}
    self
    (fromIntegral length)

-- | Returns the minimum key length as set for @completion@.
--
entryCompletionGetMinimumKeyLength :: EntryCompletion
 -> IO Int -- ^ returns The currently used minimum key length.
entryCompletionGetMinimumKeyLength self =
  liftM fromIntegral $
  (\(EntryCompletion arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_entry_completion_get_minimum_key_length argPtr1)
{-# LINE 277 "./Graphics/UI/Gtk/Entry/EntryCompletion.chs" #-}
    self

-- | Requests a completion operation, or in other words a refiltering of the
-- current list with completions, using the current key. The completion list
-- view will be updated accordingly.
--
entryCompletionComplete :: EntryCompletion -> IO ()
entryCompletionComplete self =
  (\(EntryCompletion arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_entry_completion_complete argPtr1)
{-# LINE 286 "./Graphics/UI/Gtk/Entry/EntryCompletion.chs" #-}
    self

-- | Inserts an action in @completion@'s action item list at position @index@
-- with text @text@. If you want the action item to have markup, use
-- 'entryCompletionInsertActionMarkup'.
--
entryCompletionInsertActionText :: GlibString string => EntryCompletion
 -> Int -- ^ @index@ - The index of the item to insert.
 -> string -- ^ @text@ - Text of the item to insert.
 -> IO ()
entryCompletionInsertActionText self index text =
  withUTFString text $ \textPtr ->
  (\(EntryCompletion arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_entry_completion_insert_action_text argPtr1 arg2 arg3)
{-# LINE 299 "./Graphics/UI/Gtk/Entry/EntryCompletion.chs" #-}
    self
    (fromIntegral index)
    textPtr

-- | Inserts an action in @completion@'s action item list at position @index@
-- with markup @markup@.
--
entryCompletionInsertActionMarkup :: GlibString string => EntryCompletion
 -> Int -- ^ @index@ - The index of the item to insert.
 -> string -- ^ @markup@ - Markup of the item to insert.
 -> IO ()
entryCompletionInsertActionMarkup self index markup =
  withUTFString markup $ \markupPtr ->
  (\(EntryCompletion arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gtk_entry_completion_insert_action_markup argPtr1 arg2 arg3)
{-# LINE 313 "./Graphics/UI/Gtk/Entry/EntryCompletion.chs" #-}
    self
    (fromIntegral index)
    markupPtr

-- | Deletes the action at @index@ from @completion@'s action list.
--
entryCompletionDeleteAction :: EntryCompletion
 -> Int -- ^ @index@ - The index of the item to Delete.
 -> IO ()
entryCompletionDeleteAction self index =
  (\(EntryCompletion arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_entry_completion_delete_action argPtr1 arg2)
{-# LINE 324 "./Graphics/UI/Gtk/Entry/EntryCompletion.chs" #-}
    self
    (fromIntegral index)

-- | Convenience function for setting up the most used case of this code: a
-- completion list with just strings. This function will set up @completion@ to
-- have a list displaying all (and just) strings in the completion list, and to
-- get those strings from @column@ in the model of @completion@.
--
-- This functions creates and adds a 'CellRendererText' for the selected
-- column.
--
entryCompletionSetTextColumn :: GlibString string => EntryCompletion
 -> ColumnId row string -- ^ @column@ - The column in the model of @completion@ to
                        -- get strings from.
 -> IO ()
entryCompletionSetTextColumn self column =
  (\(EntryCompletion arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_entry_completion_set_text_column argPtr1 arg2)
{-# LINE 341 "./Graphics/UI/Gtk/Entry/EntryCompletion.chs" #-}
    self
    ((fromIntegral . columnIdToNumber) column)


-- | Requests a prefix insertion.
--
-- * Available since Gtk+ version 2.6
--
entryCompletionInsertPrefix :: EntryCompletion -> IO ()
entryCompletionInsertPrefix self =
  (\(EntryCompletion arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_entry_completion_insert_prefix argPtr1)
{-# LINE 352 "./Graphics/UI/Gtk/Entry/EntryCompletion.chs" #-}
    self

-- | Returns the column in the model of the completion to get strings from.
--
-- * Available since Gtk+ version 2.6
--
entryCompletionGetTextColumn :: GlibString string => EntryCompletion
 -> IO (ColumnId row string) -- ^ returns the column containing the strings
entryCompletionGetTextColumn self =
  liftM (makeColumnIdString . fromIntegral) $
  (\(EntryCompletion arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_entry_completion_get_text_column argPtr1)
{-# LINE 363 "./Graphics/UI/Gtk/Entry/EntryCompletion.chs" #-}
    self

-- | Sets whether the common prefix of the possible completions should be
-- automatically inserted in the entry.
--
-- * Available since Gtk+ version 2.6
--
entryCompletionSetInlineCompletion :: EntryCompletion
 -> Bool -- ^ @inlineCompletion@ - @True@ to do inline completion
 -> IO ()
entryCompletionSetInlineCompletion self inlineCompletion =
  (\(EntryCompletion arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_entry_completion_set_inline_completion argPtr1 arg2)
{-# LINE 375 "./Graphics/UI/Gtk/Entry/EntryCompletion.chs" #-}
    self
    (fromBool inlineCompletion)

-- | Returns whether the common prefix of the possible completions should be
-- automatically inserted in the entry.
--
-- * Available since Gtk+ version 2.6
--
entryCompletionGetInlineCompletion :: EntryCompletion
 -> IO Bool -- ^ returns @True@ if inline completion is turned on
entryCompletionGetInlineCompletion self =
  liftM toBool $
  (\(EntryCompletion arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_entry_completion_get_inline_completion argPtr1)
{-# LINE 388 "./Graphics/UI/Gtk/Entry/EntryCompletion.chs" #-}
    self

-- | Sets whether the completions should be presented in a popup window.
--
-- * Available since Gtk+ version 2.6
--
entryCompletionSetPopupCompletion :: EntryCompletion
 -> Bool -- ^ @popupCompletion@ - @True@ to do popup completion
 -> IO ()
entryCompletionSetPopupCompletion self popupCompletion =
  (\(EntryCompletion arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_entry_completion_set_popup_completion argPtr1 arg2)
{-# LINE 399 "./Graphics/UI/Gtk/Entry/EntryCompletion.chs" #-}
    self
    (fromBool popupCompletion)

-- | Returns whether the completions should be presented in a popup window.
--
-- * Available since Gtk+ version 2.6
--
entryCompletionGetPopupCompletion :: EntryCompletion
 -> IO Bool -- ^ returns @True@ if popup completion is turned on
entryCompletionGetPopupCompletion self =
  liftM toBool $
  (\(EntryCompletion arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_entry_completion_get_popup_completion argPtr1)
{-# LINE 411 "./Graphics/UI/Gtk/Entry/EntryCompletion.chs" #-}
    self



-- | Sets whether the completion popup window will be resized to be the same
-- width as the entry.
--
-- * Available since Gtk+ version 2.8
--
entryCompletionSetPopupSetWidth :: EntryCompletion
 -> Bool -- ^ @popupSetWidth@ - @True@ to make the width of the
                    -- popup the same as the entry
 -> IO ()
entryCompletionSetPopupSetWidth self popupSetWidth =
  (\(EntryCompletion arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_entry_completion_set_popup_set_width argPtr1 arg2)
{-# LINE 426 "./Graphics/UI/Gtk/Entry/EntryCompletion.chs" #-}
    self
    (fromBool popupSetWidth)

-- | Returns whether the completion popup window will be resized to the width
-- of the entry.
--
-- * Available since Gtk+ version 2.8
--
entryCompletionGetPopupSetWidth :: EntryCompletion
 -> IO Bool -- ^ returns @True@ if the popup window will be resized to
                    -- the width of the entry
entryCompletionGetPopupSetWidth self =
  liftM toBool $
  (\(EntryCompletion arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_entry_completion_get_popup_set_width argPtr1)
{-# LINE 440 "./Graphics/UI/Gtk/Entry/EntryCompletion.chs" #-}
    self

-- | Sets whether the completion popup window will appear even if there is
-- only a single match. You may want to set this to @False@ if you are using
-- inline completion.
--
-- * Available since Gtk+ version 2.8
--
entryCompletionSetPopupSingleMatch :: EntryCompletion
 -> Bool -- ^ @popupSingleMatch@ - @True@ if the popup should
                    -- appear even for a single match
 -> IO ()
entryCompletionSetPopupSingleMatch self popupSingleMatch =
  (\(EntryCompletion arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gtk_entry_completion_set_popup_single_match argPtr1 arg2)
{-# LINE 454 "./Graphics/UI/Gtk/Entry/EntryCompletion.chs" #-}
    self
    (fromBool popupSingleMatch)

-- | Returns whether the completion popup window will appear even if there is
-- only a single match.
--
-- * Available since Gtk+ version 2.8
--
entryCompletionGetPopupSingleMatch :: EntryCompletion
 -> IO Bool -- ^ returns @True@ if the popup window will appear
                    -- regardless of the number of matches.
entryCompletionGetPopupSingleMatch self =
  liftM toBool $
  (\(EntryCompletion arg1) -> withForeignPtr arg1 $ \argPtr1 ->gtk_entry_completion_get_popup_single_match argPtr1)
{-# LINE 468 "./Graphics/UI/Gtk/Entry/EntryCompletion.chs" #-}
    self


--------------------
-- Attributes

-- | The model to find matches in.
--
entryCompletionModel :: TreeModelClass model => ReadWriteAttr EntryCompletion (Maybe TreeModel) (Maybe model)
entryCompletionModel = newAttr
  entryCompletionGetModel
  entryCompletionSetModel

-- | Minimum length of the search key in order to look up matches.
--
-- Allowed values: >= 0
--
-- Default value: 1
--
entryCompletionMinimumKeyLength :: Attr EntryCompletion Int
entryCompletionMinimumKeyLength = newAttr
  entryCompletionGetMinimumKeyLength
  entryCompletionSetMinimumKeyLength


-- | The column of the model containing the strings.
--
-- Default value: 'Graphics.UI.Gtk.ModelView.CustomStore.invalidColumnId'
--
entryCompletionTextColumn :: GlibString string => Attr EntryCompletion (ColumnId row string)
entryCompletionTextColumn = newAttr
  entryCompletionGetTextColumn
  entryCompletionSetTextColumn

-- | Determines whether the common prefix of the possible completions should
-- be inserted automatically in the entry. Note that this requires text-column
-- to be set, even if you are using a custom match function.
--
-- Default value: @False@
--
entryCompletionInlineCompletion :: Attr EntryCompletion Bool
entryCompletionInlineCompletion = newAttr
  entryCompletionGetInlineCompletion
  entryCompletionSetInlineCompletion

-- | Determines whether the possible completions should be shown in a popup
-- window.
--
-- Default value: @True@
--
entryCompletionPopupCompletion :: Attr EntryCompletion Bool
entryCompletionPopupCompletion = newAttr
  entryCompletionGetPopupCompletion
  entryCompletionSetPopupCompletion



-- | Determines whether the completions popup window will be resized to the
-- width of the entry.
--
-- Default value: @True@
--
entryCompletionPopupSetWidth :: Attr EntryCompletion Bool
entryCompletionPopupSetWidth = newAttr
  entryCompletionGetPopupSetWidth
  entryCompletionSetPopupSetWidth

-- | Determines whether the completions popup window will shown for a single
-- possible completion. You probably want to set this to @False@ if you are
-- using inline completion.
--
-- Default value: @True@
--
entryCompletionPopupSingleMatch :: Attr EntryCompletion Bool
entryCompletionPopupSingleMatch = newAttr
  entryCompletionGetPopupSingleMatch
  entryCompletionSetPopupSingleMatch


--------------------
-- Signals

-- %hash c:f4eb d:9ccf
-- | Gets emitted when the inline autocompletion is triggered. The default
-- behaviour is to make the entry display the whole prefix and select the newly
-- inserted part.
--
-- Applications may connect to this signal in order to insert only a smaller
-- part of the @prefix@ into the entry - e.g. the entry used in the
-- 'FileChooser' inserts only the part of the prefix up to the next \'\/\'.
--
-- * Available since Gtk+ version 2.6
--
insertPrefix :: (EntryCompletionClass self, GlibString string) => Signal self (string -> IO Bool)
insertPrefix = Signal (connect_GLIBSTRING__BOOL "insert-prefix")


-- %hash c:d50e d:ad7e
-- | Gets emitted when a match from the list is selected. The default
-- behaviour is to replace the contents of the entry with the contents of the
-- text column in the row pointed to by @iter@.
--
matchSelected :: EntryCompletionClass self => Signal self (TreeModel -> TreeIter -> IO Bool)
matchSelected = Signal (connect_OBJECT_BOXED__BOOL "match-selected" peekTreeIter)

-- %hash c:21ac d:2cbc
-- | Gets emitted when an action is activated.
--
completionActionActivated :: EntryCompletionClass self => Signal self (Int -> IO ())
completionActionActivated = Signal (connect_INT__NONE "action-activated")



-- | Gets emitted when the inline autocompletion is triggered. The default
-- behaviour is to make the entry display the whole prefix and select the newly
-- inserted part.
--
-- Applications may connect to this signal in order to insert only a smaller
-- part of the @prefix@ into the entry - e.g. the entry used in the
-- 'FileChooser' inserts only the part of the prefix up to the next \'\/\'.
--
onInsertPrefix, afterInsertPrefix :: (EntryCompletionClass self, GlibString string) => self
 -> (string -> IO Bool)
 -> IO (ConnectId self)
onInsertPrefix = connect_GLIBSTRING__BOOL "insert_prefix" False
afterInsertPrefix = connect_GLIBSTRING__BOOL "insert_prefix" True


-- | Gets emitted when an action is activated.
--
onActionActivated, afterActionActivated :: EntryCompletionClass self => self
 -> (Int -> IO ())
 -> IO (ConnectId self)
onActionActivated = connect_INT__NONE "action_activated" False
afterActionActivated = connect_INT__NONE "action_activated" True

foreign import ccall safe "gtk_entry_completion_new"
  gtk_entry_completion_new :: (IO (Ptr EntryCompletion))

foreign import ccall safe "gtk_entry_completion_get_entry"
  gtk_entry_completion_get_entry :: ((Ptr EntryCompletion) -> (IO (Ptr Widget)))

foreign import ccall safe "gtk_entry_completion_set_model"
  gtk_entry_completion_set_model :: ((Ptr EntryCompletion) -> ((Ptr TreeModel) -> (IO ())))

foreign import ccall safe "gtk_entry_completion_get_model"
  gtk_entry_completion_get_model :: ((Ptr EntryCompletion) -> (IO (Ptr TreeModel)))

foreign import ccall safe "gtk_entry_completion_set_match_func"
  gtk_entry_completion_set_match_func :: ((Ptr EntryCompletion) -> ((FunPtr ((Ptr EntryCompletion) -> ((Ptr CChar) -> ((Ptr TreeIter) -> ((Ptr ()) -> (IO CInt)))))) -> ((Ptr ()) -> ((FunPtr ((Ptr ()) -> (IO ()))) -> (IO ())))))

foreign import ccall safe "gtk_entry_completion_set_minimum_key_length"
  gtk_entry_completion_set_minimum_key_length :: ((Ptr EntryCompletion) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_entry_completion_get_minimum_key_length"
  gtk_entry_completion_get_minimum_key_length :: ((Ptr EntryCompletion) -> (IO CInt))

foreign import ccall safe "gtk_entry_completion_complete"
  gtk_entry_completion_complete :: ((Ptr EntryCompletion) -> (IO ()))

foreign import ccall safe "gtk_entry_completion_insert_action_text"
  gtk_entry_completion_insert_action_text :: ((Ptr EntryCompletion) -> (CInt -> ((Ptr CChar) -> (IO ()))))

foreign import ccall safe "gtk_entry_completion_insert_action_markup"
  gtk_entry_completion_insert_action_markup :: ((Ptr EntryCompletion) -> (CInt -> ((Ptr CChar) -> (IO ()))))

foreign import ccall safe "gtk_entry_completion_delete_action"
  gtk_entry_completion_delete_action :: ((Ptr EntryCompletion) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_entry_completion_set_text_column"
  gtk_entry_completion_set_text_column :: ((Ptr EntryCompletion) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_entry_completion_insert_prefix"
  gtk_entry_completion_insert_prefix :: ((Ptr EntryCompletion) -> (IO ()))

foreign import ccall safe "gtk_entry_completion_get_text_column"
  gtk_entry_completion_get_text_column :: ((Ptr EntryCompletion) -> (IO CInt))

foreign import ccall safe "gtk_entry_completion_set_inline_completion"
  gtk_entry_completion_set_inline_completion :: ((Ptr EntryCompletion) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_entry_completion_get_inline_completion"
  gtk_entry_completion_get_inline_completion :: ((Ptr EntryCompletion) -> (IO CInt))

foreign import ccall safe "gtk_entry_completion_set_popup_completion"
  gtk_entry_completion_set_popup_completion :: ((Ptr EntryCompletion) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_entry_completion_get_popup_completion"
  gtk_entry_completion_get_popup_completion :: ((Ptr EntryCompletion) -> (IO CInt))

foreign import ccall safe "gtk_entry_completion_set_popup_set_width"
  gtk_entry_completion_set_popup_set_width :: ((Ptr EntryCompletion) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_entry_completion_get_popup_set_width"
  gtk_entry_completion_get_popup_set_width :: ((Ptr EntryCompletion) -> (IO CInt))

foreign import ccall safe "gtk_entry_completion_set_popup_single_match"
  gtk_entry_completion_set_popup_single_match :: ((Ptr EntryCompletion) -> (CInt -> (IO ())))

foreign import ccall safe "gtk_entry_completion_get_popup_single_match"
  gtk_entry_completion_get_popup_single_match :: ((Ptr EntryCompletion) -> (IO CInt))
