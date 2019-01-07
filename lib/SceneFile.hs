{-# LANGUAGE OverloadedStrings #-} 

module SceneFile(SceneFile, Item(..), parseSceneFile) where

import Data.Attoparsec.Text hiding (decimal, double)
import qualified Data.Attoparsec.Text as Att (decimal, double)
import Data.Text

data Item
    = ItemVertex Double Double Double
    | ItemColor Double Double Double
    | ItemMaterial Int Int Int Int Double
    | ItemSphere Int Double Int
    | ItemTriangle Int Int Int Int
    | ItemLightSource Int Int
    | ItemCamera Int Int Int

type SceneFile = [Item]

parseSceneFile :: Text -> Either String SceneFile
parseSceneFile = parseOnly sceneFile 

sceneFile :: Parser SceneFile
sceneFile = many1 (skipSpaceAndComments *> item)
  where
    skipSpaceAndComments = many' (skipSpace >> comment)

comment :: Parser ()
comment = string "#" >> skipWhile (not . isEndOfLine)

double :: Parser Double
double = skipSpace *> Att.double

decimal :: Parser Int
decimal = skipSpace *> Att.decimal

item :: Parser Item
item = choice [vertex, color, material, sphere, triangle, light, camera]

vertex :: Parser Item
vertex = string "v" *> (ItemVertex <$> double <*> double <*> double)

color :: Parser Item
color = string "c" *> (ItemColor <$> double <*> double <*> double)

material :: Parser Item
material 
    = string "material" 
    *> (ItemMaterial <$> decimal <*> decimal <*> decimal <*> decimal <*> double)

sphere :: Parser Item
sphere = string "sphere" *> (ItemSphere <$> decimal <*> double <*> decimal)

triangle :: Parser Item
triangle = string "triangle" *> (ItemTriangle <$> decimal <*> decimal <*> decimal <*> decimal)

light :: Parser Item
light = string "light" *> (ItemLightSource <$> decimal <*> decimal)

camera :: Parser Item
camera = string "camera" *> (ItemCamera <$> decimal <*> decimal <*> decimal)
