module Main (main) where

import Control.Monad

import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.Gdk.GC as GC
import Graphics.UI.Gtk (AttrOp((:=)))

import Raytracer

main :: IO ()
main = do
  Gtk.initGUI
  window <- Gtk.windowNew
  Gtk.onDestroy window Gtk.mainQuit
  Gtk.set window [ Gtk.windowTitle := "Raytraing" ]
  vbox <- Gtk.vBoxNew False 4
  Gtk.set window [ Gtk.containerChild := vbox ]
  canvas <- Gtk.drawingAreaNew
  Gtk.widgetSetSizeRequest canvas 600 600
  Gtk.onExpose canvas $ \_ -> do
    d <- Gtk.widgetGetDrawWindow canvas
    gc <- GC.gcNew d
    (w, h) <- Gtk.widgetGetSize canvas
    forM_ [0..h - 1] $ \y ->
      forM_ [0..w - 1] $ \x -> do
        gcValues <- GC.gcGetValues gc
        let fx = fromIntegral x / (fromIntegral w - 1)
        let fy = fromIntegral y / (fromIntegral h - 1)
        let (r, g, b) = tracePixel fx fy
        let color = GC.Color (fromIntegral r) (fromIntegral g) (fromIntegral b)
        let gcValues' = gcValues { GC.foreground = color }
        GC.gcSetValues gc gcValues'
        Gtk.drawPoint d gc (x, y)
    return True
  Gtk.set vbox [ Gtk.containerChild := canvas ]
  Gtk.widgetShowAll window
  Gtk.mainGUI

