module Material where

import Color

data Material
    = Material
    { matSpecular  :: Color
    , matDiffuse   :: Color
    , matAmbient   :: Color
    , matReflected :: Color
    , matShininess :: Double
    }

rubber  :: Color -> Material
rubber  c = Material ks kd ka kr s
  where
    ks = c * 0.6
    kd = c * 0.6
    ka = c * 0.6
    kr = c * 0.05
    s = 50

plastic :: Color -> Material
plastic c = Material ks kd ka kr s
  where
    ks = c * 0.8
    kd = c * 0.2
    ka = c * 0.2
    kr = c * 0.8
    s = 280

