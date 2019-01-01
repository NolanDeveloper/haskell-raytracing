module Raytracer(tracePixel) where

import Data.List
import Data.Ord

import Types

import Linear

reflect :: V3 Double -> V3 Double -> V3 Double
reflect v n = (2 * (v `dot` n)) *^ n - v

traceRay :: Scene -> Int -> Ray -> Color
traceRay _ 0 _ = 0
traceRay scene@(Scene objects lightSources) depth ray@(Ray src dir) =
    case [(s, p, n) | s <- objects, Just (p, n) <- [cast s ray]] of
        []   -> 0
        list ->
            let z = comparing (\(_, pos, _) -> norm (pos - src))
                (s, p, n) = minimumBy z list
            in illumination (material s) p n
  where
    {- https://en.wikipedia.org/wiki/Phong_reflection_model -}
    illumination (Material ks kd ka kr alpha) fragmentPosition n =
        ka + kr * reflected + sum
            [ (kd * pure (lm `dot` n) +
              ks * pure ((hm `dot` n) ** alpha)) * i
            | LightSource lightPosition c <- lightSources
            , let lm = normalize (lightPosition - fragmentPosition)
            , all (not . intersects fragmentPosition lightPosition)
                  [cast object (Ray fragmentPosition lm) | object <- objects]
            , let hm = normalize (lm - dir)
            , let i = c * pure (10 / norm (lightPosition - fragmentPosition))
            ]
      where
        intersects _ _ Nothing = False
        intersects fragment light (Just (p, _)) =
            norm (p - fragment) < norm (light - fragment)
        reflected = traceRay scene (depth - 1) ray'
          where
            ray' = Ray fragmentPosition (reflect (-dir) n)

tracePixel :: Camera -> Scene -> Double -> Double -> Color
tracePixel camera scene x y = traceRay scene 3 ray
  where
    Camera 
        { cameraPosition = cp
        , cameraTopLeft = tl
        , cameraTopRight = tr
        , cameraBottomLeft = bl
        } = camera
    ray = Ray cp direction
      where
        dx = (tr - tl) ^* x
        dy = (bl - tl) ^* y
        direction = normalize $ (tl + dx + dy) - cp
