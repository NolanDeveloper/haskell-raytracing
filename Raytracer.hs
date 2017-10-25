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

data Material
    = Material
    { matSpecular        :: Color
    , matDiffuse         :: Color
    , matAmbient         :: Color
    , matReflected       :: Color
    , matShininess       :: Double
    }

rabber :: Color -> Material
rabber c = Material ks kd ka kr s
  where
    ks = c * pure 0.7
    kd = c * pure 0.5
    ka = c * pure 0.5
    kr = c * pure 0.05
    s = 80

plastic :: Color -> Material
plastic c = Material ks kd ka kr s
  where
    ks = c * pure 0.1
    kd = c * pure 0.5
    ka = c * pure 0.5
    kr = c * pure 0.5
    s = 280

data Ray
    = Ray
    { raySource          :: Position
    , rayDirection       :: Direction
    }

data Sphere
    = Sphere
    { sphereCenter :: Position
    , sphereRadius :: Double
    , sphereMaterial :: Material
    }

data Triangle = Triangle Position Position Position Material

class SceneObject a where
    {- Casts a ray. If it intersects object returns position of first
    intersection and normal of object in that point. -}
    cast :: Ray -> a -> Maybe (Position, Direction)
    material :: a -> Material

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
        positiveTs = sort $ filter (0.001 <) ts
        xn t = (x', n')
          where
            x' = x0 + (d * pure t)
            n' = vnormal $ x' - xc
        ts = solveQuadraticEquation a b c
        a = d `vdot` d
        b = 2 * ((x0 - xc) `vdot` d)
        c = (x0 - xc) `vdot` (x0 - xc) - r^2
    material = sphereMaterial

instance SceneObject Triangle where
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
    material (Triangle _ _ _ m) = m

data Object
    = SphereObject Sphere
    | TriangleObject Triangle

instance SceneObject Object where
    cast ray (SphereObject s)   = cast ray s
    cast ray (TriangleObject t) = cast ray t
    material (SphereObject s) = material s
    material (TriangleObject t) = material t

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

sphere :: Vector -> Double -> Material -> Object
sphere p r m = SphereObject $ Sphere p r m

triangle :: Vector -> Vector -> Vector -> Material -> Object
triangle a b c m = TriangleObject $ Triangle a b c m

plane :: Vector -> Vector -> Vector -> Vector -> Material -> [Object]
plane a b c d m =
    [ triangle a c b m
    , triangle a d c m
    ]

objects =
    plane a b c d red ++
    plane e f b a green ++
    plane g h d c blue ++
    plane d h e a purple ++
    plane b f g c purple ++
    [ sphere (Vec (-6) (-6) (-16)) 4 pwhite
    , sphere (Vec 0 (-6) (-12)) 2 rwhite
    , sphere (Vec (-3) (-7) (-10)) 1 rwhite
    ]
  where
    a = Vec 10 10 (-20)
    b = Vec 10 (-10) (-20)
    c = Vec (-10) (-10) (-20)
    d = Vec (-10) 10 (-20)
    [e, f, g, h] = map (\(Vec x y z) -> Vec x y (-z)) [a, b, c, d]
    red    = rabber $ Color 0.8 0.4 0.4
    green  = rabber $ Color 0.4 0.8 0.4
    blue   = rabber $ Color 0.4 0.4 0.8
    purple = rabber $ Color 0.4 0.8 0.8
    rwhite = rabber $ Color 0.8 0.8 0.8
    pwhite = plastic $ Color 0.8 0.8 0.8

lightSources =
    map (\(LightSource p c) -> LightSource p (pure 0.5 * c))
        [ LightSource (Vec 8 8 (-10)) (Color 1 1 1)
        --, LightSource (Vec (-9) (-9) (-13)) (Color 1 (0.5 * 1) 0)
        --, LightSource (Vec (-2 + 3) (-2) (-12)) (Color 1 0 0)
        --, LightSource (Vec (-2) (-2 + 3) (-12)) (Color 0 1 0)
        --, LightSource (Vec (-2) (-2) (-12 + 3)) (Color 0 0 1)
        ]

distanceToCamera :: Camera -> Vector -> Double
distanceToCamera c p = vlen (p - camPosition c)

traceRay :: Int -> Ray -> Color
traceRay 0 _ = pure 0
traceRay depth ray =
    case [ (s, p, n) | s <- objects, Just (p, n) <- [ cast ray s ] ] of
        []   -> pure 0
        list ->
            let z = comparing (\(_, p, _) -> distanceToCamera camera p)
                (s, p, n) = minimumBy z list
            in illumination (material s) p n
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


tracePixel :: Double -> Double -> (Int, Int, Int)
tracePixel x y = toGuiColor $ traceRay 2 (getRay camera x y)
