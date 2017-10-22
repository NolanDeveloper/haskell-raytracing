module Raytracer(tracePixel) where

import Control.Applicative
import Data.List
import Debug.Trace

import Color
import Vector

type Position = Vector
type Direction = Vector

data Ray =
    Ray
    { raySource :: Position
    , rayDirection :: Direction
    }

data Sphere =
    Sphere
    { sphereCenter :: Position
    , sphereRadius :: Double
    , sphereColor :: Color
    }

class SceneObject a where
    {- Casts a ray. If it intersects object returns position of first
    intersection and normal of object in that point. -}
    cast :: Ray -> a -> Maybe (Position, Direction)

solveQuadraticEquation :: Double -> Double -> Double -> [Double]
solveQuadraticEquation a b c
    | d < 0     = []
    | d == 0    = [-b / 2 * a]
    | otherwise = [(-b + sqrt d) / 2 * a, (-b - sqrt d) / 2 * a]
  where
    d = b * b - 4 * a * c

instance SceneObject Sphere where
    cast ray sphere
        | null positiveTs = Nothing
        | otherwise       = Just $ xn $ head positiveTs
      where
        positiveTs = sort $ filter (0 <) t
        x  t = x0 + (d `vscale` t)
        n  t = vnormal $ x t - xc
        xn t = (x t, n t)
        t = solveQuadraticEquation a b c
        a = d `vdot` d
        b = 2 * ((x0 - xc) `vdot` d)
        c = (x0 - xc) `vdot` (x0 - xc) - r^2
        x0 = raySource ray
        d  = rayDirection ray
        xc = sphereCenter sphere
        r  = sphereRadius sphere

data Camera =
    Camera
    { camPosition :: Position
    , camTopLeft :: Position
    , camTopRight :: Position
    , camBottomLeft :: Position
    }

getRay :: Camera -> Double -> Double -> Ray
getRay c x y = Ray (camPosition c) direction
  where
    dx = (camTopRight c   - camTopLeft c) `vscale` x
    dy = (camBottomLeft c - camTopLeft c) `vscale` y
    direction = vnormal $ (camTopLeft c + dx + dy) - camPosition c

data LightSource = LightSource Position Color

camera = Camera
    { camPosition   = Vec 0 0 0 0
    , camTopLeft    = Vec (-0.5) 0.5 (-0.5) 0
    , camTopRight   = Vec 0.5 0.5 (-0.5) 0
    , camBottomLeft = Vec (-0.5) (-0.5) (-0.5) 0
    }

sphere =
    Sphere
    { sphereCenter = Vec 0 0 (-7) 0
    , sphereRadius = 4
    , sphereColor  = Color 1 1 1
    }

lightSources =
    [ LightSource (Vec (10) (10) 0 0)   (Color 1 0 0)
    , LightSource (Vec (-10) (-10) 0 0) (Color 0 1 0)
    , LightSource (Vec 5 0 (-2) 0)      (Color 0 0 1)
    ]

{- https://en.wikipedia.org/wiki/Phong_reflection_model -}
phongIllumination :: Color -> Position -> Vector -> Vector -> Color
phongIllumination material fragmentPosition n v =
    ka * ambient + sum
        [ (kd * pure (lm `vdot` n) + ks * pure ((hm `vdot` n) ** alpha)) * i
        | LightSource lightPosition i <- lightSources
        , let lm = vnormal $ lightPosition - fragmentPosition
        , let hm = vnormal $ lm + v
        ]
  where
    ks = material * Color 0.7 0.7 0.7
    kd = material * Color 0.7 0.7 0.7
    ka = material * Color 0.2 0.2 0.2
    alpha = 1000 :: Double
    ambient = Color 1 1 1

tracePixel :: Double -> Double -> (Int, Int, Int)
tracePixel x y
    | Nothing <- c = black
    | Just (p, n) <- c
        = toGuiColor $ phongIllumination (sphereColor sphere) p n (-direction)
  where
    ray@(Ray _ direction) = getRay camera x y
    c = cast ray sphere
    black = (0, 0, 0)
