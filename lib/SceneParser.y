{
module SceneParser (parse) where

import SceneLexer
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
    : Blocks eof                    { $1 }

Blocks :: { [Block] }
    : Blocks Block                  { $2 : $1 }
    |                               { [] }

Block :: { Block }
    : Vertices                      { Vertices $1 }
    | Colors                        { Colors $1 }
    | Materials                     { undefined }
    | Shapes                        { undefined }
    | Light                         { undefined }
    | Camera                        { undefined }

Vertices :: { [Vertex] }
    : vertices NamedVectors         { map (\(a, b, c, d) -> MkVertex a b c d) $2 }

Colors :: { [Color] }
    : colors NamedVectors           { map (\(a, b, c, d) -> MkColor a b c d) $2 }

NamedVectors :: { [(String, Double, Double, Double)] }
    : NamedVectors NamedVector      { $2 : $1 }
    |                               { [] }

FloatOrInt :: { Double }
    : float                         { $1 }
    | int                           { fromIntegral $1 }

NamedVector :: { (String, Double, Double, Double) }
    : id FloatOrInt FloatOrInt FloatOrInt { ($1, $2, $3, $4) }

Materials :: { [Material] }
    : materials NamedMaterials      { $2 }

NamedMaterials :: { [Material] }
    : NamedMaterials NamedMaterial  { $2 : $1 }
    |                               { [] }

NamedMaterial :: { Material }
    : id id id id id int            { MkMaterial $1 $2 $3 $4 $5 $6 }

Shapes :: { [Shape] }
    : shapes ListOfShapes           { $2 }

ListOfShapes  :: { [Shape] }
    : ListOfShapes Shape            { $2 : $1 }
    |                               { [] }

Shape :: { Shape }
    : poly id id id                 { MkPoly $2 $3 $4 }
    | sphere id FloatOrInt          { MkSphere $2 $3 }

Light :: { Light }
    : light id id                   { MkLight $2 $3 }

Camera
    : camera id id id               { MkCamera $2 $3 $4 }

{
data Vertex = MkVertex String Double Double Double

data Color = MkColor String Double Double Double

data Material = MkMaterial String String String String String Int

data Shape 
    = MkPoly String String String
    | MkSphere String Double

data Light = MkLight String String

data Camera = MkCamera String String String 

data Block
    = Vertices [Vertex]
    | Colors [Color]
    | Materials [Material]
    | Shapes [Shape]
    | Light Light
    | Camera Camera

happyError = error "parse error"

failUnless b msg = if b then () else error msg

}
