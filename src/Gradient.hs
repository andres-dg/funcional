{-# LANGUAGE PackageImports, BangPatterns, QuasiQuotes, PatternGuards #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}

module Gradient 
        ( GImage
        , sobelX
        , sobelY
        , gaussian
        , niceGaussian
        , convolute
        , generateArrayStencil
        , sumStencil
        , normalize
        , Gradient.max
        , Gradient.min)
where
import Data.Array.Repa                  as R
import Data.Array.Repa.Stencil          as R
import Data.Array.Repa.Stencil.Dim2     as R
import Prelude

type GImage = Array U DIM2 Float

convolute :: Monad m => GImage -> Float -> Float -> Stencil DIM2 Float -> m GImage
convolute img bound norm stencil = computeP
                                        $ mapStencil2 (BoundConst bound) stencil img

normalize :: Monad m => GImage -> m GImage
normalize img = do
        maxVal <- Gradient.max img
        minVal <- Gradient.min img
        let aux = computeUnboxedS $ R.map (\x -> x -minVal) img
        return $ computeUnboxedS $ R.map (/ maxVal) img

max :: Monad m => GImage -> m Float
max = foldAllP (\x y -> if x < y then y else x) 0

min :: Monad m => GImage -> m Float
min = foldAllP (\x y -> if x > y then y else x) 1000000.0

normal2d :: Floating a => a -> a -> a -> a
normal2d sigma i j = (exp (-(i**2 + j**2) / sigma**2)) / (2 * pi * sigma**2) 

generateArrayStencil :: Floating a => DIM2 -> (a -> a -> a) -> Array D DIM2 a
generateArrayStencil (Z :. x :. y) gen = fromFunction (Z :. x :. y) 
                (\ix -> case ix of 
                        Z:. i :. j -> gen (fromIntegral (i - (div x 2))) (fromIntegral (j - (div y 2))))

normalStencil :: Floating a => Int -> a -> Stencil DIM2 a
normalStencil width sigma = makeStencil2 width width
                $ \ix -> case ix of
                        Z :. i :. j -> Just (normal2d sigma (fromIntegral i) (fromIntegral j))

sumStencil :: Floating a => DIM2 -> a -> a
sumStencil shape sigma = sumAllS (generateArrayStencil shape (normal2d sigma))

gaussian :: Monad m => GImage -> Float -> m GImage
gaussian img bound = convolute img bound 16 [stencil2|  1  2  1
                                                        2  4  2
                                                        1  2  1 |]

-- (Image, bound, size (odd), sigma) computes Image
niceGaussian :: Monad m => GImage -> Float -> Int -> Float -> m GImage
niceGaussian img bound s sigma = do
        let st = normalStencil s sigma
        let stSum = sumStencil (Z :. s :. s) sigma
        convolute img bound stSum st

sobelX :: Stencil DIM2 Float
sobelX = [stencil2|     -1  0  1
                        -2  0  2
                        -1  0  1 |]

sobelY :: Stencil DIM2 Float
sobelY = [stencil2|     1  2  1
                        0  0  0
                        -1 -2 -1 |]
