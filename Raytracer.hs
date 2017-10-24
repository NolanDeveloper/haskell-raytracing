module Raytracer(tracePixel) where

import Control.Applicative
import Data.List
import Data.Maybe
import Data.Ord
import Debug.Trace

import Color
import Vector

type Position = Vector
type Direction = Vector

data Ray
    = Ray
    { raySource :: Position
    , rayDirection :: Direction
    }

data Sphere
    = Sphere
    { sphereCenter :: Position
    , sphereRadius :: Double
    , sphereColor :: Color
    }

data Triangle = Triangle Position Position Position Color

class SceneObject a where
    {- Casts a ray. If it intersects object returns position of first
    intersection and normal of object in that point. -}
    cast :: Ray -> a -> Maybe (Position, Direction)
    objectColor :: a -> Color

solveQuadraticEquation :: Double -> Double -> Double -> [Double]
solveQuadraticEquation a b c
    | d < 0     = []
    | d == 0    = [-b / 2 * a]
    | otherwise = [(-b + sqrt d) / 2 * a, (-b - sqrt d) / 2 * a]
  where
    d = b * b - 4 * a * c

instance SceneObject Sphere where
    cast (Ray x0 d) (Sphere xc r _)
        | null positiveTs = Nothing
        | otherwise       = Just $ xn $ head positiveTs
      where
        positiveTs = sort $ filter (0 <) ts
        x  t = x0 + (d * pure t)
        n  t = vnormal $ x t - xc
        xn t = (x t, n t)
        ts = solveQuadraticEquation a b c
        a = d `vdot` d
        b = 2 * ((x0 - xc) `vdot` d)
        c = (x0 - xc) `vdot` (x0 - xc) - r^2
    objectColor = sphereColor

instance SceneObject Triangle where
    cast (Ray x0 d) (Triangle a b c _)
        | 0 < t
        , 0 <= k, 0 <= l
        , k + l <= 1
        , 0 /= d `vdot` n
            = Just (p, n)
        | otherwise
            = Nothing
      where
        n = vnormal $ (b - a) `vcross` (c - a)
        t = -((x0 - a) `vdot` n) / (d `vdot` n)
        p = x0 + pure t * d
        Vec k l _ = toBasis (b - a, c - a, n) (p - a)
    objectColor (Triangle _ _ _ c) = c

data Object
    = SphereObject Sphere
    | TriangleObject Triangle

instance SceneObject Object where
    cast ray (SphereObject s)   = cast ray s
    cast ray (TriangleObject t) = cast ray t
    objectColor (SphereObject s) = objectColor s
    objectColor (TriangleObject t) = objectColor t

data Camera =
    Camera
    { camPosition   :: Position
    , camTopLeft    :: Position
    , camTopRight   :: Position
    , camBottomLeft :: Position
    }

getRay :: Camera -> Double -> Double -> Ray
getRay c x y = Ray (camPosition c) direction
  where
    dx = (camTopRight c - camTopLeft c) `vscale` x
    dy = (camBottomLeft c - camTopLeft c) `vscale` y
    direction = vnormal $ (camTopLeft c + dx + dy) - camPosition c

data LightSource = LightSource Position Color

camera = Camera
    { camPosition   = Vec 0 0 0
    , camTopLeft    = Vec (-0.5) 0.5 (-0.5)
    , camTopRight   = Vec 0.5 0.5 (-0.5)
    , camBottomLeft = Vec (-0.5) (-0.5) (-0.5)
    }

sphere :: Vector -> Double -> Color -> Object
sphere p r c = SphereObject $ Sphere p r c

triangle :: Vector -> Vector -> Vector -> Color -> Object
triangle a b c color = TriangleObject $ Triangle a b c color

plane :: Vector -> Vector -> Vector -> Vector -> Color -> [Object]
plane a b c d color =
    [ triangle a c b color
    , triangle a d c color
    ]

objects =
    plane a b c d red ++
    plane e f b a green ++
    plane g h d c blue ++
    plane d h e a purple ++
    plane b f g c purple ++
    [ sphere (Vec (-6) (-6) (-16)) 4 white
    , sphere (Vec 0 (-6) (-12)) 2 white
    , sphere (Vec (-3) (-7) (-10)) 1 white
    ]
  where
    a = Vec 10 10 (-20)
    b = Vec 10 (-10) (-20)
    c = Vec (-10) (-10) (-20)
    d = Vec (-10) 10 (-20)
    [e, f, g, h] = map (\(Vec x y z) -> Vec x y (-z)) [a, b, c, d]
    red    = Color 0.8 0.4 0.4
    green  = Color 0.4 0.8 0.4
    blue   = Color 0.4 0.4 0.8
    purple = Color 0.4 0.8 0.8
    white  = Color 0.8 0.8 0.8

lightSources =
    [ LightSource (Vec 0 8 (-10)) (Color 0.8 0.8 1)
    , LightSource (Vec (-9) (-9) (-13)) (Color 1 0.5 0)
    , LightSource (Vec (-2 + 3) (-2) (-12)) (Color 1 0 0)
    , LightSource (Vec (-2) (-2 + 3) (-12)) (Color 0 1 0)
    , LightSource (Vec (-2) (-2) (-12 + 3)) (Color 0 0 1)
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
    ks = material * Color 0.5 0.5 0.5
    kd = material * Color 0.4 0.4 0.4
    ka = material * Color 0.3 0.3 0.3
    alpha = 300 :: Double
    ambient = Color 1 1 1

distanceToCamera :: Camera -> Vector -> Double
distanceToCamera c p = vlen (p - camPosition c)

tracePixel :: Double -> Double -> (Int, Int, Int)
tracePixel x y
    = case [ (s, p, n) | s <- objects, Just (p, n) <- [ cast ray s ] ] of
        []   -> (0, 0, 0)
        list ->
            let z = comparing (\(_, p, _) -> distanceToCamera camera p)
                (s, p, n) = minimumBy z list
            in toGuiColor $ phongIllumination (objectColor s) p n (-direction)
  where
    ray@(Ray _ direction) = getRay camera x y
    black = (0, 0, 0)
