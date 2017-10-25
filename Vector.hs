module Vector where

import Control.Applicative

data GenericVector a = Vec a a a
    deriving Eq

type Vector = GenericVector Double

instance Show a => Show (GenericVector a) where
    show (Vec x y z) =
        "(" ++ x' ++ ", " ++ y' ++ ", " ++ z' ++ ")"
      where
        x' = show x
        y' = show y
        z' = show z

instance Functor GenericVector where
    fmap f (Vec x y z) = Vec (f x) (f y) (f z)

instance Applicative GenericVector where
    pure x = (Vec x x x)
    (Vec f g h) <*> (Vec x y z) = Vec (f x) (g y) (h z)

instance Num a => Num (GenericVector a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    abs x = abs <$> x
    signum x = signum <$> x
    fromInteger x = fromInteger <$> Vec x x x
    negate x = negate <$> x

vscale :: Vector -> Double -> Vector
vscale u f = (f *) <$> u

vdot :: Vector -> Vector -> Double
vdot = (sum .) . liftA2 (*)
  where
    sum (Vec a b c) = a + b + c

vlen :: Vector -> Double
vlen v = sqrt $ v `vdot` v

vcross :: Vector -> Vector -> Vector
(Vec x y z) `vcross` (Vec x' y' z') =
    Vec (y * z' - z * y') (z * x' - x * z') (x * y' - y * x')

vcos :: Vector -> Vector -> Double
vcos u v = (u `vdot` v) / (vlen u * vlen v)

vsin :: Vector -> Vector -> Double
vsin u v = vlen (u `vcross` v) / (vlen u * vlen v)

vnormal :: Vector -> Vector
vnormal v = v `vscale` (1 / vlen v)

type Basis = (Vector, Vector, Vector)

determenant :: Basis -> Double
determenant (Vec a b c, Vec d e f, Vec g h i)
    = a * e * i + b * f * g + c * d * h - a * f * h - b * d * i - c * e * g

toBasis :: Basis -> Vector -> Vector
toBasis basis@(v1@(Vec a b c), v2@(Vec d e f), v3@(Vec g h i)) v =
    Vec (d1 / d) (d2 / d) (d3 / d)
  where
    d = determenant basis
    d1 = determenant (v, v2, v3)
    d2 = determenant (v1, v, v3)
    d3 = determenant (v1, v2, v)

vreflect :: Vector -> Vector -> Vector
vreflect v n = pure (2 * (v `vdot` n)) * n - v
