import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (Action, backspace)
import Graphics.UI.Gtk.Gdk.Pixbuf

main :: IO ()
main = do
  void initGUI          
  window <- windowNew

  set window [ windowTitle         := "Image processing"
    , windowDefaultWidth  := 230
    , windowDefaultHeight := 250 ]


  image <- imageNewFromFile "sample/lena-rgb.png"

  pbuf <- pixbufNewFromFile "sample/lena-rgb.png"

  nChannels <- pixbufGetNChannels pbuf
  putStrLn nChannels

  containerAdd window image
  widgetShowAll window  
  mainGUI               