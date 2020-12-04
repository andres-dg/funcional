{-# LANGUAGE ScopedTypeVariables #-}

{-# LINE 2 "./Graphics/UI/Gtk/Gdk/Region.chs" #-}
-- -*-haskell-*-
-- GIMP Toolkit (GTK) Region
--
-- Author : Axel Simon
--
-- Created: 22 September 2002
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
-- The Span functions and callbacks are not implemented since retrieving
-- a set of rectangles and working on them within Haskell seems to be easier.
--
-- |
-- Maintainer : gtk2hs-users@lists.sourceforge.net
-- Stability : provisional
-- Portability : portable (depends on GHC)
--
-- A set of rectangles describing areas to be redrawn.
--
-- * Regions consist of a set of non-overlapping rectangles. They are used to
-- specify the area of a window which needs updating.
--
-- This module is empty when built with Gtk3 because Pixmap has been
-- removed.
module Graphics.UI.Gtk.Gdk.Region (

  makeNewRegion,
  Region(Region),
  regionNew,
  FillRule(..),
  regionPolygon,
  regionCopy,
  regionRectangle,
  regionGetClipbox,
  regionGetRectangles,
  regionEmpty,
  regionEqual,
  regionPointIn,
  OverlapType(..),
  regionRectIn,
  regionOffset,
  regionShrink,
  regionUnionWithRect,
  regionIntersect,
  regionUnion,
  regionSubtract,
  regionXor

  ) where



import Control.Monad (liftM)

import System.Glib.FFI
import Graphics.UI.Gtk.General.Structs (Point, Rectangle(..))


{-# LINE 72 "./Graphics/UI/Gtk/Gdk/Region.chs" #-}

newtype Region = Region (ForeignPtr (Region))
{-# LINE 74 "./Graphics/UI/Gtk/Gdk/Region.chs" #-}

instance Show Region where
  show r = show (unsafePerformIO (regionGetRectangles r))

-- Construct a region from a pointer.
--
makeNewRegion :: Ptr Region -> IO Region
makeNewRegion rPtr = do
  region <- newForeignPtr rPtr region_destroy
  return (Region region)

foreign import ccall unsafe "&gdk_region_destroy"
  region_destroy :: FinalizerPtr Region

-- | Specify how to interpret a polygon.
--
-- * The flag determines what happens if a polygon has overlapping areas.
--
data FillRule = EvenOddRule
              | WindingRule
              deriving (Enum)

{-# LINE 93 "./Graphics/UI/Gtk/Gdk/Region.chs" #-}

-- | How a rectangle is contained in a 'Region'.
--
data OverlapType = OverlapRectangleIn
                 | OverlapRectangleOut
                 | OverlapRectanglePart
                 deriving (Enum)

{-# LINE 97 "./Graphics/UI/Gtk/Gdk/Region.chs" #-}

-- | Create an empty region.
--
regionNew :: IO Region
regionNew = do
  rPtr <- gdk_region_new
{-# LINE 103 "./Graphics/UI/Gtk/Gdk/Region.chs" #-}
  makeNewRegion rPtr

-- | Convert a polygon into a 'Region'.
--
regionPolygon :: [Point] -> FillRule -> IO Region
regionPolygon points rule =
  withArray (concatMap (\(x,y) -> [fromIntegral x, fromIntegral y]) points) $
  \(aPtr :: Ptr (CInt)) -> do
    rPtr <- gdk_region_polygon (castPtr aPtr)
            (fromIntegral (length points)) ((fromIntegral.fromEnum) rule)
    makeNewRegion rPtr

-- | Copy a 'Region'.
--
regionCopy :: Region -> IO Region
regionCopy r = do
  rPtr <- (\(Region arg1) -> withForeignPtr arg1 $ \argPtr1 ->gdk_region_copy argPtr1) r
  makeNewRegion rPtr

-- | Convert a rectangle to a 'Region'.
--
regionRectangle :: Rectangle -> IO Region
regionRectangle rect = with rect $ \rectPtr -> do
  regPtr <- gdk_region_rectangle (castPtr rectPtr)
  makeNewRegion regPtr

-- | Smallest rectangle including the
-- 'Region'.
--
regionGetClipbox :: Region -> IO Rectangle
regionGetClipbox r = alloca $ \rPtr -> do
  (\(Region arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gdk_region_get_clipbox argPtr1 arg2) r (castPtr rPtr)
  peek rPtr

-- | Turn the 'Region' into its rectangles.
--
-- A 'Region' is a set of horizontal bands. Each band consists of one or more
-- rectangles of the same height. No rectangles in a band touch.
--
regionGetRectangles :: Region -> IO [Rectangle]
regionGetRectangles region =
  alloca $ \(rectPtrPtr :: Ptr (Ptr Rectangle)) ->
  alloca $ \(iPtr :: Ptr (CInt)) -> do
    (\(Region arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gdk_region_get_rectangles argPtr1 arg2 arg3) region (castPtr rectPtrPtr) iPtr
    size <- peek iPtr
    rectPtr <- peek rectPtrPtr
    rects <- peekArray (fromIntegral size) rectPtr
    g_free (castPtr rectPtr)
    return rects

-- | Test if a 'Region' is empty.
--
regionEmpty :: Region -> IO Bool
regionEmpty r = liftM toBool $ (\(Region arg1) -> withForeignPtr arg1 $ \argPtr1 ->gdk_region_empty argPtr1) r

-- | Compares two 'Region's for equality.
--
regionEqual :: Region -> Region -> IO Bool
regionEqual r1 r2 = liftM toBool $ (\(Region arg1) (Region arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gdk_region_equal argPtr1 argPtr2) r1 r2

-- | Checks if a point it is within a region.
--
regionPointIn :: Region -> Point -> IO Bool
regionPointIn r (x,y) = liftM toBool $
  (\(Region arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gdk_region_point_in argPtr1 arg2 arg3) r (fromIntegral x) (fromIntegral y)

-- | Check if a rectangle is within a region.
--
regionRectIn :: Region -> Rectangle -> IO OverlapType
regionRectIn reg rect = liftM (toEnum.fromIntegral) $ with rect $
  \rPtr -> (\(Region arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gdk_region_rect_in argPtr1 arg2) reg (castPtr rPtr)

-- | Move a region.
--
regionOffset :: Region -> Int -> Int -> IO ()
regionOffset r dx dy =
  (\(Region arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gdk_region_offset argPtr1 arg2 arg3) r (fromIntegral dx) (fromIntegral dy)

-- | Move a region.
--
-- * Positive values shrink the region, negative values expand it.
--
regionShrink :: Region -> Int -> Int -> IO ()
regionShrink r dx dy =
  (\(Region arg1) arg2 arg3 -> withForeignPtr arg1 $ \argPtr1 ->gdk_region_shrink argPtr1 arg2 arg3) r (fromIntegral dx) (fromIntegral dy)

-- | Updates the region to include the rectangle.
--
regionUnionWithRect :: Region -> Rectangle -> IO ()
regionUnionWithRect reg rect = with rect $ \rPtr ->
  (\(Region arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 ->gdk_region_union_with_rect argPtr1 arg2) reg (castPtr rPtr)

-- | Intersects one region with another.
--
-- * Changes @reg1@ to include the common areas of @reg1@
-- and @reg2@.
--
regionIntersect :: Region -> Region -> IO ()
regionIntersect reg1 reg2 = (\(Region arg1) (Region arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gdk_region_intersect argPtr1 argPtr2) reg1 reg2

-- | Unions one region with another.
--
-- * Changes @reg1@ to include @reg1@ and @reg2@.
--
regionUnion :: Region -> Region -> IO ()
regionUnion reg1 reg2 = (\(Region arg1) (Region arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gdk_region_union argPtr1 argPtr2) reg1 reg2

-- | Removes pars of a 'Region'.
--
-- * Reduces the region @reg1@ so that is does not include any areas
-- of @reg2@.
--
regionSubtract :: Region -> Region -> IO ()
regionSubtract reg1 reg2 = (\(Region arg1) (Region arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gdk_region_subtract argPtr1 argPtr2) reg1 reg2

-- | XORs two 'Region's.
--
-- * The exclusive or of two regions contains all areas which were not
-- overlapping. In other words, it is the union of the regions minus
-- their intersections.
--
regionXor :: Region -> Region -> IO ()
regionXor reg1 reg2 = (\(Region arg1) (Region arg2) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->gdk_region_xor argPtr1 argPtr2) reg1 reg2

foreign import ccall unsafe "gdk_region_new"
  gdk_region_new :: (IO (Ptr Region))

foreign import ccall unsafe "gdk_region_polygon"
  gdk_region_polygon :: ((Ptr ()) -> (CInt -> (CInt -> (IO (Ptr Region)))))

foreign import ccall unsafe "gdk_region_copy"
  gdk_region_copy :: ((Ptr Region) -> (IO (Ptr Region)))

foreign import ccall unsafe "gdk_region_rectangle"
  gdk_region_rectangle :: ((Ptr ()) -> (IO (Ptr Region)))

foreign import ccall unsafe "gdk_region_get_clipbox"
  gdk_region_get_clipbox :: ((Ptr Region) -> ((Ptr ()) -> (IO ())))

foreign import ccall unsafe "gdk_region_get_rectangles"
  gdk_region_get_rectangles :: ((Ptr Region) -> ((Ptr (Ptr ())) -> ((Ptr CInt) -> (IO ()))))

foreign import ccall unsafe "g_free"
  g_free :: ((Ptr ()) -> (IO ()))

foreign import ccall unsafe "gdk_region_empty"
  gdk_region_empty :: ((Ptr Region) -> (IO CInt))

foreign import ccall unsafe "gdk_region_equal"
  gdk_region_equal :: ((Ptr Region) -> ((Ptr Region) -> (IO CInt)))

foreign import ccall unsafe "gdk_region_point_in"
  gdk_region_point_in :: ((Ptr Region) -> (CInt -> (CInt -> (IO CInt))))

foreign import ccall unsafe "gdk_region_rect_in"
  gdk_region_rect_in :: ((Ptr Region) -> ((Ptr ()) -> (IO CInt)))

foreign import ccall unsafe "gdk_region_offset"
  gdk_region_offset :: ((Ptr Region) -> (CInt -> (CInt -> (IO ()))))

foreign import ccall unsafe "gdk_region_shrink"
  gdk_region_shrink :: ((Ptr Region) -> (CInt -> (CInt -> (IO ()))))

foreign import ccall unsafe "gdk_region_union_with_rect"
  gdk_region_union_with_rect :: ((Ptr Region) -> ((Ptr ()) -> (IO ())))

foreign import ccall unsafe "gdk_region_intersect"
  gdk_region_intersect :: ((Ptr Region) -> ((Ptr Region) -> (IO ())))

foreign import ccall unsafe "gdk_region_union"
  gdk_region_union :: ((Ptr Region) -> ((Ptr Region) -> (IO ())))

foreign import ccall unsafe "gdk_region_subtract"
  gdk_region_subtract :: ((Ptr Region) -> ((Ptr Region) -> (IO ())))

foreign import ccall unsafe "gdk_region_xor"
  gdk_region_xor :: ((Ptr Region) -> ((Ptr Region) -> (IO ())))
