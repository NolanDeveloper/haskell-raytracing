{
module SceneParser (parse) where

import SceneLexer
import Data.List
}

%tokentype { Token }
%token 
    vertex      { TokenVertex }
    vertices    { TokenVertices }
    colors      { TokenColors }
    materials   { TokenMaterials }
    shapes      { TokenShapes }
    poly        { TokenPoly }
    sphere      { TokenSphere }
    light       { TokenLight }
    camera      { TokenCamera }
    int         { TokenInt $$ }
    float       { TokenFloat $$ }
    id          { TokenId $$ }
    eof         { TokenEOF }

%name parse Start

%%

Start :: { [Block] }
    : Blocks eof                    { reverse $1 }

Blocks :: { [Block] }
    : Blocks Block                  { $2 : $1 }
    |                               { [] }

Block :: { Block }
    : Vertices                      { Vertices $1 }
    | Colors                        { Colors $1 }
    | Materials                     { Materials $1 }
    | Shapes                        { Shapes $1 }
    | Light                         { Light $1 }
    | Camera                        { Camera $1 }

Vertices :: { [Vertex] }
    : vertices NamedVectors         { map (\(a, b, c, d) -> MkVertex a b c d) (reverse $2) }

Colors :: { [Color] }
    : colors NamedVectors           { map (\(a, b, c, d) -> MkColor a b c d) (reverse $2) }

NamedVectors :: { [(String, Double, Double, Double)] }
    : NamedVectors NamedVector      { $2 : $1 }
    |                               { [] }

FloatOrInt :: { Double }
    : float                         { $1 }
    | int                           { fromIntegral $1 }

NamedVector :: { (String, Double, Double, Double) }
    : id FloatOrInt FloatOrInt FloatOrInt { ($1, $2, $3, $4) }

Materials :: { [Material] }
    : materials NamedMaterials      { reverse $2 }

NamedMaterials :: { [Material] }
    : NamedMaterials NamedMaterial  { $2 : $1 }
    |                               { [] }

NamedMaterial :: { Material }
    : id id id id id int            { MkMaterial $1 $2 $3 $4 $5 $6 }

Shapes :: { [Shape] }
    : shapes ListOfShapes           { reverse $2 }

ListOfShapes  :: { [Shape] }
    : ListOfShapes Shape            { $2 : $1 }
    |                               { [] }

Shape :: { Shape }
    : poly id Ids                   { MkPoly $2 (reverse $3) }
    | sphere id id FloatOrInt       { MkSphere $2 $3 $4 }

Ids :: { [String] }
    : Ids id                        { $2 : $1 }
    |                               { [] }

Light :: { Light }
    : light id id                   { MkLight $2 $3 }

Camera :: { Camera }
    : camera id id id               { MkCamera $2 $3 $4 }

{
data Vertex = MkVertex String Double Double Double

data Color = MkColor String Double Double Double

data Material = MkMaterial String String String String String Int

data Shape 
    = MkPoly String [String]
    | MkSphere String String Double

data Light = MkLight String String

data Camera = MkCamera String String String 

data Block
    = Vertices [Vertex]
    | Colors [Color]
    | Materials [Material]
    | Shapes [Shape]
    | Light Light
    | Camera Camera

instance Show Vertex where
    show (MkVertex a b c d) = a ++ " " ++ intercalate " " (map show [b, c, d])

instance Show Color where
    show (MkColor a b c d) = a ++ " " ++ intercalate " " (map show [b, c, d])

instance Show Material where
    show (MkMaterial a b c d e f) = intercalate " " [a, b, c, d, e, show f]

instance Show Shape where
    show (MkPoly m vs) = "poly " ++ m ++ " " ++ intercalate " " vs
    show (MkSphere m a b) = "sphere " ++ m ++ " " ++ a ++ " " ++ show b

instance Show Light where
    show (MkLight a b) = a ++ " " ++ b

instance Show Camera where
    show (MkCamera a b c) = a ++ " " ++ b ++ " " ++ c

instance Show Block where
    show (Vertices vs)  = concat ("vertices\n" : map (\v -> "   " ++ show v ++ "\n") vs)
    show (Colors cs)    = concat ("colors\n" : map (\c -> "   " ++ show c ++ "\n") cs)
    show (Materials ms) = concat ("materials\n" : map (\m -> "   " ++ show m ++ "\n") ms)
    show (Shapes ss)    = concat ("shapes\n" : map (\s -> "   " ++ show s ++ "\n") ss)
    show (Light l)      = "light " ++ show l ++ "\n"
    show (Camera c)     = "camera " ++ show c ++ "\n"

happyError a = error $ "parse error: " ++ show a

}
