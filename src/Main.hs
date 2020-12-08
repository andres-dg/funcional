{-# LANGUAGE PackageImports, BangPatterns, QuasiQuotes, PatternGuards #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}

import qualified GI.Gtk as GI (main, init)
import qualified Data.Text as T
import Graphic
import Noise

import GI.Gtk.Enums
       (Orientation(..), WindowType(..), ButtonBoxStyle(..), FileChooserAction(..))

import GI.Gtk.Objects.Image
       (imageNewFromFile)

import GI.Gtk
       (buttonBoxNew, buttonBoxSetChildSecondary, widgetShowAll,
        setButtonBoxLayoutStyle, buttonNewWithLabel, setContainerChild,
        setContainerBorderWidth, mainQuit, onWidgetDestroy, windowNew, boxPackStart,
       containerAdd, boxNew, onButtonClicked, fileChooserButtonNew, afterFileChooserButtonFileSet,
       fileChooserGetFilename, imageSetFromFile, FileChooserButton, fileChooserSetFilename)

import Control.Monad.IO.Class

sampleImage :: FilePath
sampleImage = "sample/lena-rgb.png"

tmpImage :: FilePath
tmpImage = "tmp.png"

main :: IO ()
main = do
       GI.init Nothing      
       window <- windowNew WindowTypeToplevel

       onWidgetDestroy window mainQuit
       setContainerBorderWidth window 10

       hbuttonbox <- buttonBoxNew OrientationHorizontal

       image <- imageNewFromFile sampleImage

       loadButton <- fileChooserButtonNew (T.pack "Load image") FileChooserActionOpen 
       afterFileChooserButtonFileSet loadButton (do
                                                        filename <- fileChooserGetFilename loadButton
                                                        case filename of
                                                               (Just x) -> imageSetFromFile image filename
                                                               (Nothing) -> putStrLn "Nothing")

       button2 <- buttonNewWithLabel $ T.pack "Filter"
       onButtonClicked button2 (do
                                  filename <- getFileName loadButton
                                  transformImg filename tmpImage (normalFilter 3 7)
                                  fileChooserSetFilename loadButton tmpImage
                                  imageSetFromFile image (Just tmpImage))


       button3 <- buttonNewWithLabel $ T.pack "Edges"

       onButtonClicked button3 (do
                                  filename <- getFileName loadButton
                                  transformImg filename tmpImage sobel
                                  fileChooserSetFilename loadButton tmpImage
                                  imageSetFromFile image (Just tmpImage))

       button4 <- buttonNewWithLabel $ T.pack "Noise"

       onButtonClicked button4 (do
                                  filename <- getFileName loadButton
                                  transformImg filename tmpImage (applyNoise 5)
                                  fileChooserSetFilename loadButton tmpImage
                                  imageSetFromFile image (Just tmpImage))

       -- Add each button to the button box with the default packing and padding
       mapM_ (setContainerChild hbuttonbox) [button2, button3, button4]

       -- The final step is to display everything (the window and all the widgets
       -- contained within it)

       layout <- do
              hb <- boxNew OrientationHorizontal 0
              boxPackStart hb loadButton True True 0
              vb <- boxNew OrientationVertical 0
              boxPackStart vb hb True True 10
              boxPackStart vb image True True 0
              boxPackStart vb hbuttonbox False False 10
              return vb
       
       containerAdd window layout
       widgetShowAll window

       GI.main

getFileName :: FileChooserButton -> IO (FilePath)
getFileName button = do
                        filename <- fileChooserGetFilename button
                        case filename of
                                (Just x) -> return x
                                (Nothing) -> return sampleImage

