module Vector where

import Control.Applicative

data GenericVector a = Vec a a a a
    deriving Eq

type Vector = GenericVector Double

instance Show a => Show (GenericVector a) where
    show (Vec x y z w) =
        "(" ++ x' ++ ", " ++ y' ++ ", " ++ z' ++ ", " ++ w' ++ ")"
      where
        x' = show x
        y' = show y
        z' = show z
        w' = show w

instance Functor GenericVector where
    fmap f (Vec x y z w) = Vec (f x) (f y) (f z) (f w)

instance Applicative GenericVector where
    pure x = (Vec x x x x)
    (Vec f g h i) <*> (Vec x y z w) = Vec (f x) (g y) (h z) (i w)

instance Num a => Num (GenericVector a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    abs x = abs <$> x
    signum x = signum <$> x
    fromInteger x = fromInteger <$> Vec x x x x
    negate x = negate <$> x

vscale :: Vector -> Double -> Vector
vscale u f = (f *) <$> u

vdot :: Vector -> Vector -> Double
vdot = (sum .) . liftA2 (*)
  where
    sum (Vec a b c d) = a + b + c + d

vlen :: Vector -> Double
vlen v = sqrt $ v `vdot` v

vcos :: Vector -> Vector -> Double
vcos u v = (u `vdot` v) / (vlen u * vlen v)

vnormal :: Vector -> Vector
vnormal v = v `vscale` (1 / vlen v)
