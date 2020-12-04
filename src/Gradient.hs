{-# LANGUAGE PackageImports, BangPatterns, QuasiQuotes, PatternGuards #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}

module Gradient 
        ( GImage
        , sobelX
        , sobelY )
where
import Data.Array.Repa                  as R
import Data.Array.Repa.Stencil          as R
import Data.Array.Repa.Stencil.Dim2     as R

type GImage      = Array U DIM2 Float

convolute :: Monad m => GImage -> Float -> Stencil DIM2 Float -> m GImage
convolute img bound stencil = computeP $ mapStencil2 (BoundConst bound) stencil img

sobelX :: Monad m => GImage -> Float -> m GImage
sobelX img bound = convolute img bound [stencil2|   -1  0  1
                                                    -2  0  2
                                                    -1  0  1 |]

{-# NOINLINE sobelX #-}

sobelY :: Monad m => GImage -> Float -> m GImage
sobelY img bound = convolute img bound [stencil2|   1  2  1
                                                    0  0  0
                                                    -1 -2 -1 |] 
{-# NOINLINE sobelY #-}
