{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.List
import Control.Applicative
import GHC.Generics
import Data.Function

import Linear

data GenericColor a = Color a a a
    deriving (Eq, Show)

type Color = GenericColor Double

instance Functor GenericColor where
    fmap f (Color r g b) = Color (f r) (f g) (f b)

instance Applicative GenericColor where
    pure x = Color x x x
    (Color f g h) <*> (Color x y z) = Color (f x) (g y) (h z)

instance (Ord a, Num a) => Num (GenericColor a) where
    (+) = liftA2 $ ((max 0 . min 1) .) . (+)
    (*) = liftA2 $ ((max 0 . min 1) .) . (*)
    abs x = abs <$> x
    signum x = signum <$> x
    fromInteger x = fromInteger <$> Color x x x
    negate x = negate <$> x

instance (Ord a, Fractional a) => Fractional (GenericColor a) where
    fromRational x = fromRational <$> Color x x x
    (/) = liftA2 $ ((max 0 . min 1) .) . (/)
    
data Material
    = Material
    { materialSpecular  :: Color
    , materialDiffuse   :: Color
    , materialAmbient   :: Color
    , materialReflected :: Color
    , materialShininess :: Double
    }

type Position = V3 Double
type Direction = V3 Double

data Ray = 
    Ray 
    { raySource :: Position 
    , rayDirection :: Direction
    }

data Sphere = 
    MkSphere 
    { sphereCenter :: Position 
    , sphereRadius :: Double 
    , sphereMaterial :: Material
    }

data Triangle = 
    MkTriangle 
    { triangleA :: Position 
    , triangleB :: Position 
    , trianglec :: Position 
    , triangleMaterial :: Material
    }

solveQuadratic :: (Eq a, Ord a, Floating a) => a -> a -> a -> [a]
solveQuadratic a b c = map (\rootD -> -b + rootD) roots
  where
    d = b * b - 4 * a * c
    roots
      | d < 0 = []
      | d == 0 = [0]
      | otherwise = [-sqrt d, sqrt d]

castSphere :: Sphere -> Ray -> Maybe (Position, Direction)
castSphere (MkSphere xc r _) (Ray src dir) 
    | (answer:_) <- answers = Just answer
    | otherwise = Nothing
  where
    answers = solveQuadratic a b c
        & filter (0.001 <)
        & sort
        & map (\t -> let x = src + t *^ dir in (x, normalize (x - xc)))
    a = dir `dot` dir
    b = 2 * ((src - xc) `dot` dir)
    c = (src - xc) `dot` (src - xc) - r * r

-- miller1997
castTriangle :: Triangle -> Ray -> Maybe (Position, Direction)
castTriangle (MkTriangle v0 v1 v2 _) (Ray o d)
    | t < eps = Nothing
    | u < 0 || v < 0 || u + v > 1 = Nothing
    | otherwise = Just ((1 - u - v) *^ v0 + u *^ v1 + v *^ v2, normalize (e1 `cross` e2))
  where
    eps = 0.00000001
    e1 = v1 - v0
    e2 = v2 - v0
    t' = o - v0
    p = d `cross` e2
    q = t' `cross` e1
    det = p `dot` e1
    V3 t u v = V3 (q `dot` e2) (p `dot` t') (q `dot` d) ^/ det

data SceneObject 
    = Triangle Triangle
    | Sphere Sphere

cast :: SceneObject -> Ray -> Maybe (Position, Direction)
cast (Triangle triangle) = castTriangle triangle
cast (Sphere sphere) = castSphere sphere

material :: SceneObject -> Material
material (Triangle (MkTriangle _ _ _ m)) = m
material (Sphere (MkSphere _ _ m)) = m

data LightSource = 
    LightSource 
    { lightSourcePosition :: Position 
    , lightSourceColor :: Color
    } deriving Generic

data Camera =
    Camera
    { cameraPosition :: Position
    , cameraTopLeft :: Position
    , cameraTopRight :: Position
    , cameraBottomLeft :: Position
    } deriving Generic

data Scene = 
    Scene 
    { sceneObjects :: [SceneObject] 
    , sceneLightSources :: [LightSource] 
    } deriving Generic
