module Main (main) where

import Control.Monad
import Control.Parallel
import System.Environment
import Text.Read

import Raytracer
import Color

import Graphics.Image

render :: (Int, Int) -> Image VU RGB Double
render size@(width, height) = makeImageR VU size generator
  where
    generator (row, col) = PixelRGB r g b
      where
        fx = fromIntegral col / fromIntegral width
        fy = fromIntegral row / fromIntegral height
        Color r g b = tracePixel fx fy

main :: IO ()
main = do
    args <- getArgs
    case args of
        [width', height', filename]
            | Just width <- readMaybe width'
            , Just height <- readMaybe height' ->
                let image = render (width, height)
                in writeImage filename image
        _ -> do
            prog <- getProgName
            putStrLn $ "Usage: " ++ prog ++ " 640 480 filename.png"
