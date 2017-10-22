module Color where

import Control.Applicative

data GenericColor a = Color a a a
    deriving (Eq, Show)

type Color = GenericColor Double
type GuiColor = (Int, Int, Int)

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

toGuiColor :: Color -> GuiColor
toGuiColor (Color r g b) = (toGui r, toGui g, toGui b)
  where
    toGui c
        | c < 0     = 0
        | 1 < c     = 65535
        | otherwise = floor $ c * 65535

