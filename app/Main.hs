{-# LANGUAGE RecordWildCards #-} 
{-# LANGUAGE NegativeLiterals #-} 

module Main (main) where

import System.Environment
import Text.Read

import Raytracer
import Types
import Material

import Linear
import Codec.Picture

data Arguments 
    = Arguments
        { pictureWidth :: Int
        , pictureHeight :: Int
        , picturePath :: FilePath 
        }

parseArguments :: IO Arguments
parseArguments = do
    args <- getArgs
    case args of 
        [width', height', path]
            | Just width <- readMaybe width'
            , Just height <- readMaybe height' ->
                pure $ Arguments width height path
        _ -> do
            prog <- getProgName
            error $ "Usage: " ++ prog ++ " 640 480 filename.png"

render :: Camera -> Scene -> Int -> Int -> Image PixelRGB8
render camera scene width height = generateImage generator width height
  where
    generator col row = PixelRGB8 r g b
      where
        m = fromIntegral (maxBound :: Pixel8)
        fx = fromIntegral col / fromIntegral width
        fy = fromIntegral row / fromIntegral height
        Color r g b = (\c -> round $ m * c) <$> tracePixel camera scene fx fy

plane :: Position -> Position -> Position -> Position -> Material -> [SceneObject]
plane a b c d m = map Triangle [MkTriangle a c b m, MkTriangle a d c m]

cube :: Position -> Double -> Material -> [SceneObject]
cube position size m =
    plane a d c b m ++
    plane e a b f m ++
    plane g c d h m ++
    plane d a e h m ++
    plane b c g f m ++
    plane e h g f m
  where
    s = pure $ size / 2
    a = position + s * V3  1  1 -1
    b = position + s * V3  1 -1 -1
    c = position + s * V3 -1 -1 -1
    d = position + s * V3 -1  1 -1
    e = position + s * V3  1  1  1
    f = position + s * V3  1 -1  1
    g = position + s * V3 -1 -1  1
    h = position + s * V3 -1  1  1

objects :: [SceneObject]
objects =
    plane a b c d red ++
    plane e f b a green ++
    plane g h d c blue ++
    plane d h e a purple ++
    plane b f g c purple ++
    plane e f g h purple ++
    cube (V3 -4.5 -6.5 -13) 4 gold ++
    map Sphere 
        [ MkSphere (V3 -5 -6 -16) 4 pwhite
        , MkSphere (V3 5 -5 -16) 5 silver
        , MkSphere (V3 0 -8 -12) 2 red
        , MkSphere (V3 -4 -8.5 -11) 1.5 pwhite
        ]
  where
    a = V3 10 10 -20
    b = V3 10 -10 -20
    c = V3 -10 -10 -20
    d = V3 -10 10 -20
    [e, f, g, h] = map (\(V3 x y z) -> V3 x y (-z)) [a, b, c, d]
    red    = rubber $ Color 0.9 0.3 0.3
    green  = plastic $ Color 0.3 0.9 0.3
    blue   = plastic $ Color 0.3 0.3 0.9
    purple = rubber $ Color 0.3 0.9 0.9
    gold   = plastic $ Color 0.9 0.9 0.3
    silver = plastic $ Color 0.9 0.9 0.9
    pwhite = rubber $ Color 0.9 0.9 0.9

defaultLightSources :: [LightSource]
defaultLightSources =
    [ LightSource (V3 8 8 -10) (Color 0.5 0.5 0.5)
    , LightSource (V3 -9 -9 -13) (Color 0.5 0.25 0)
    , LightSource (V3 1 -2 -12) (Color 0.5 0 0)
    , LightSource (V3 -2 1 -12) (Color 0 0.5 0)
    , LightSource (V3 -2 -2 -9) (Color 0 0 0.5)
    , LightSource (V3 9 -9 -17) (Color 0 0 0.5)
    ]

defaultCamera :: Camera
defaultCamera = Camera
    { cameraPosition   = V3 -0.001 0.01 0.01
    , cameraTopLeft    = V3 -0.5  0.5 -0.5
    , cameraTopRight   = V3  0.5  0.5 -0.5
    , cameraBottomLeft = V3 -0.5 -0.5 -0.5
    }

main :: IO ()
main = do
    Arguments{..} <- parseArguments
    let scene = Scene objects defaultLightSources 
    let image = render defaultCamera scene pictureWidth pictureHeight
    writePng picturePath image
