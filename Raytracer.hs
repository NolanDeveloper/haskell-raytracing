{-# LANGUAGE NegativeLiterals #-}
module Raytracer(tracePixel) where

import Control.Applicative
import Data.List
import Data.Maybe
import Data.Ord
import Debug.Trace

import Color
import Vector
import Material

type Position = Vector
type Direction = Vector

data Ray
    = Ray
    { raySource    :: Position
    , rayDirection :: Direction
    }

data SceneObject
    = Sphere Position Double Material
    | Triangle Position Position Position Material

solveQuadraticEquation :: Double -> Double -> Double -> [Double]
solveQuadraticEquation a b c
    | d < 0     = [ ]
    | d == 0    = [ -b / 2 * a ]
    | otherwise = [ (-b + sqrt d) / 2 * a, (-b - sqrt d) / 2 * a ]
  where
    d = b * b - 4 * a * c

cast :: Ray -> SceneObject -> Maybe (Position, Direction)
cast (Ray x0 d) (Sphere xc r _)
    | null positiveTs = Nothing
    | otherwise       = Just xn'
  where
    positiveTs = sort $ filter (0.001 <) ts
    xn' = xn $ head positiveTs
    xn t = (x', n')
      where
        x' = x0 + (d * pure t)
        n' = vnormal $ x' - xc
    ts = solveQuadraticEquation a b c
    a = d `vdot` d
    b = 2 * ((x0 - xc) `vdot` d)
    c = (x0 - xc) `vdot` (x0 - xc) - r^2
cast (Ray x0 d) (Triangle a b c _)
    | 0.001 < t
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

getMaterial :: SceneObject -> Material
getMaterial (Sphere _ _ m) = m
getMaterial (Triangle _ _ _ m) = m

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

plane :: Vector -> Vector -> Vector -> Vector -> Material -> [SceneObject]
plane a b c d m =
    [ Triangle a c b m
    , Triangle a d c m
    ]

cube :: Vector -> Double -> Material -> [SceneObject]
cube position size material =
    plane a d c b material ++
    plane e a b f material ++
    plane g c d h material ++
    plane d a e h material ++
    plane b c g f material ++
    plane e h g f material
  where
    s = pure $ size / 2
    a = position + s * Vec  1  1 -1
    b = position + s * Vec  1 -1 -1
    c = position + s * Vec -1 -1 -1
    d = position + s * Vec -1  1 -1
    e = position + s * Vec  1  1  1
    f = position + s * Vec  1 -1  1
    g = position + s * Vec -1 -1  1
    h = position + s * Vec -1  1  1

objects =
    plane a b c d red ++
    plane e f b a green ++
    plane g h d c blue ++
    plane d h e a purple ++
    plane b f g c purple ++
    plane e f g h purple ++
    cube (Vec -4.5 -6.5 -13) 4 gold ++
    [ {- Sphere (Vec -5 -6 -16) 4 pwhite
    , -} Sphere (Vec 5 -5 -16) 5 silver
    , Sphere (Vec 0 -8 -12) 2 red
    , Sphere (Vec -4 -8.5 -11) 1.5 pwhite
    ]
  where
    a = Vec 10 10 -20
    b = Vec 10 -10 -20
    c = Vec -10 -10 -20
    d = Vec -10 10 -20
    [e, f, g, h] = map (\(Vec x y z) -> Vec x y (-z)) [a, b, c, d]
    red    = rubber $ Color 0.9 0.3 0.3
    green  = plastic $ Color 0.3 0.9 0.3
    blue   = plastic $ Color 0.3 0.3 0.9
    purple = rubber $ Color 0.3 0.9 0.9
    gold   = plastic $ Color 0.9 0.9 0.3
    silver = plastic $ Color 0.9 0.9 0.9
    pwhite = rubber $ Color 0.9 0.9 0.9

lightSources =
    [ LightSource (Vec 8 8 -10) (Color 0.5 0.5 0.5)
    , LightSource (Vec -9 -9 -13) (Color 0.5 0.25 0)
    , LightSource (Vec 1 -2 -12) (Color 0.5 0 0)
    , LightSource (Vec -2 1 -12) (Color 0 0.5 0)
    , LightSource (Vec -2 -2 -9) (Color 0 0 0.5)
    , LightSource (Vec 9 -9 -17) (Color 0 0 0.5)
    ]

traceRay :: Int -> Ray -> Color
traceRay 0 _ = 0
traceRay depth ray =
    case [ (s, p, n) | s <- objects, Just (p, n) <- [ cast ray s ] ] of
        []   -> 0
        list ->
            let z = comparing (\(_, p, _) -> vlen (p - raySource ray))
                (s, p, n) = minimumBy z list
            in illumination (getMaterial s) p n
  where
    {- https://en.wikipedia.org/wiki/Phong_reflection_model -}
    illumination (Material ks kd ka kr alpha) fragmentPosition n =
        ka + kr * reflected + sum
            [ (kd * pure (lm `vdot` n) +
              ks * pure ((hm `vdot` n) ** alpha)) * i
            | LightSource lightPosition c <- lightSources
            , let lm = vnormal $ lightPosition - fragmentPosition
            , all (not . intersects fragmentPosition lightPosition)
                  [ cast (Ray fragmentPosition lm) object | object <- objects ]
            , let hm = vnormal $ lm - rayDirection ray
            , let i = c * pure (10 / vlen (lightPosition - fragmentPosition))
            ]
      where
        intersects _ _ Nothing = False
        intersects fragment light (Just (p, _)) =
            vlen (p - fragment) < vlen (light - fragment)
        direction = rayDirection ray
        reflected = traceRay (depth - 1) ray'
          where
            ray' = Ray fragmentPosition (vreflect (-direction) n)


tracePixel :: Double -> Double -> Color
tracePixel x y = traceRay 5 (getRay camera x y)
  where
    camera = Camera
        { camPosition   = Vec 0 0 0
        , camTopLeft    = Vec (-0.5) 0.5 (-0.5)
        , camTopRight   = Vec 0.5 0.5 (-0.5)
        , camBottomLeft = Vec (-0.5) (-0.5) (-0.5)
        }
