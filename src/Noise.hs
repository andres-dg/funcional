{-# LANGUAGE PackageImports, BangPatterns, QuasiQuotes, PatternGuards #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}

module Noise 
        ( applyNoise )
where

import Data.Array.Repa                  as R
import Data.Random.Normal
import Prelude
import Gradient

applyNoise :: Monad m => Float -> GImage -> m GImage
applyNoise sigma img = do
    let noise = fromListUnboxed (extent img) (take (size (extent img)) $ mkNormals' (0, sigma) 0)
    let a = computeUnboxedS $ R.map (\x -> x * 255) img
    computeUnboxedP (a +^ noise) 
