module Color where

import Control.Applicative

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
