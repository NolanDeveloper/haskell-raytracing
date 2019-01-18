{
{-# LANGUAGE RecordWildCards #-}

module SceneParser ( parseScene ) where

import SceneLexer
import Data.List
import Control.Monad.State
import Control.Monad.Except
import qualified Types as T
import Linear
import qualified Data.Map as M
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

IntOrFloat :: { Double }
    : float                         { $1 }
    | int                           { fromIntegral $1 }

NamedVector :: { (String, Double, Double, Double) }
    : id IntOrFloat IntOrFloat IntOrFloat { ($1, $2, $3, $4) }

Materials :: { [Material] }
    : materials NamedMaterials      { reverse $2 }

NamedMaterials :: { [Material] }
    : NamedMaterials NamedMaterial  { $2 : $1 }
    |                               { [] }

NamedMaterial :: { Material }
    : id id id id id IntOrFloat     { MkMaterial $1 $2 $3 $4 $5 $6 }

Shapes :: { [Shape] }
    : shapes ListOfShapes           { reverse $2 }

ListOfShapes  :: { [Shape] }
    : ListOfShapes Shape            { $2 : $1 }
    |                               { [] }

Shape :: { Shape }
    : poly id Ids                   { MkPoly $2 (reverse $3) }
    | sphere id id IntOrFloat       { MkSphere $2 $3 $4 }

Ids :: { [String] }
    : Ids id                        { $2 : $1 }
    |                               { [] }

Light :: { Light }
    : light id id                   { MkLight $2 $3 }

Camera :: { Camera }
    : camera id id id               { MkCamera $2 $3 $4 }

{
happyError a = error $ "parse error: " ++ show a

data Vertex = MkVertex String Double Double Double

data Color = MkColor String Double Double Double

data Material = MkMaterial String String String String String Double

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

data NamedThing
    = NamedVertex (V3 Double)
    | NamedColor T.Color
    | NamedMaterial T.Material

getVertex :: NamedThing -> Maybe (V3 Double)
getVertex (NamedVertex x) = Just x
getVertex _ = Nothing

getColor :: NamedThing -> Maybe T.Color
getColor (NamedColor x) = Just x
getColor _ = Nothing

getMaterial :: NamedThing -> Maybe T.Material
getMaterial (NamedMaterial x) = Just x
getMaterial _ = Nothing

data SceneState = 
    SceneState 
    { namedThings :: M.Map String NamedThing
    , sceneObjects :: [T.SceneObject] 
    , lightSources :: [T.LightSource] 
    , camera :: Maybe Camera
    }

type Action a = StateT SceneState (Either String) a

onNamedThings :: (M.Map String NamedThing -> M.Map String NamedThing) -> Action()
onNamedThings f = modify (\s -> s { namedThings = f (namedThings s) })

onSceneObjects :: ([T.SceneObject] -> [T.SceneObject]) -> Action()
onSceneObjects f = modify (\s -> s { sceneObjects = f (sceneObjects s) })

onLightSources :: ([T.LightSource] -> [T.LightSource]) -> Action()
onLightSources f = modify (\s -> s { lightSources = f (lightSources s) })

addNamedThing :: String -> NamedThing -> Action ()
addNamedThing name thing = do
    ss@SceneState{..} <- get
    when (M.member name namedThings) (throwError $ "Named used multiple times: " ++ name)
    onNamedThings (M.insert name thing)

addSceneObject :: T.SceneObject -> Action ()
addSceneObject newObject = onSceneObjects (newObject :)

addLightSource :: T.LightSource -> Action ()
addLightSource newLightSource = onLightSources (newLightSource :)

-- addCamera

lookupNamedThing :: String -> Action NamedThing
lookupNamedThing name = do
    maybeThing <- gets (\s@SceneState{..} -> M.lookup name namedThings)
    maybe (throwError $ "No such name: " ++ name) pure maybeThing

lookupColor :: String -> Action T.Color
lookupColor name = do
    maybeColor <- getColor <$> lookupNamedThing name
    maybe (throwError $ "Expected color: " ++ name) pure maybeColor

lookupVertex :: String -> Action (V3 Double)
lookupVertex name = do
    maybeVertex <- getVertex <$> lookupNamedThing name
    maybe (throwError $ "Expected vertex: " ++ name) pure maybeVertex

lookupMaterial :: String -> Action T.Material
lookupMaterial name = do
    maybeMaterial <- getMaterial <$> lookupNamedThing name
    maybe (throwError $ "Expected material: " ++ name) pure maybeMaterial

buildScene :: [Block] -> Action T.Scene
buildScene blocks = do
    forM blocks $ \block -> do
      case block of
        Vertices vertices -> do
          forM_ vertices $ \(MkVertex name x y z) -> do
            addNamedThing name (NamedVertex (V3 x y z))
        Colors colors -> do
          forM_ colors $ \(MkColor name x y z) -> do
            addNamedThing name (NamedColor (V3 x y z))
        Materials materials -> do
          forM_ materials $ \(MkMaterial name specular diffuse ambient reflected shininess) -> do
            materials <- mapM lookupColor [specular, diffuse, ambient, reflected]
            let [specular', diffuse', ambient', reflected'] = materials
            let material = T.Material specular' diffuse' ambient' reflected' shininess
            addNamedThing name (NamedMaterial material)
        Shapes shapes -> do
          forM_ shapes $ \shape -> do
            case shape of
              MkPoly material vertices -> do
                when (length vertices < 3) (throwError "Poly has less than 3 vertices")
                vertices' <- mapM lookupVertex vertices
                material' <- lookupMaterial material
                let (v:vs) = vertices'
                let mkTriangle = (\a b c material -> T.Triangle $ T.MkTriangle a b c material)
                let triangles = zipWith (\a b -> mkTriangle v a b material') vs (tail vs)
                mapM_ addSceneObject triangles
              MkSphere material position radius -> do
                position' <- lookupVertex position
                material' <- lookupMaterial material
                let sphere = T.Sphere (T.MkSphere position' radius material')
                addSceneObject sphere
        Light (MkLight position color) -> do
          position' <- lookupVertex position
          color' <- lookupColor color
          addLightSource (T.LightSource position' color')
        Camera _{-camera-} -> do
          --addCamera (Camera (MkCamera ))
          pure ()
    T.Scene <$> gets sceneObjects <*> gets lightSources

parseScene :: String -> Either String T.Scene
parseScene text = do
    blocks <- parse <$> tokenize text 
    flip evalStateT initialState (buildScene blocks)
  where
    initialState = SceneState mempty mempty mempty Nothing
}
