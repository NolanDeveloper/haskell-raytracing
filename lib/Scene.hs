{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE NegativeLiterals #-}

module Scene 
    ( parseSceneFromFile, parseScene
    ) where

import Control.Monad.Except             ( throwError )
import Control.Monad.State              ( StateT, MonadState(..), execStateT )
import Control.Monad                    ( mapM_, when )
import Data.Maybe                       ( isNothing )
import Data.Sequence                    ( Seq, (|>), (!?), empty )
import Data.Text                        ( Text )
import Data.Text.IO                     ( readFile )
import Linear                           ( V3(..) )
import Prelude                   hiding ( readFile )
import SceneFile                        ( SceneFile, Item(..), parseSceneFile )
import Types                     hiding ( material, sceneObjects )

type SceneState = 
    ( Seq Position
    , Seq Color
    , Seq Material
    , [SceneObject]
    , [LightSource]
    , Maybe Camera)

initialState :: SceneState
initialState = (empty, empty, empty, [], [], Nothing)

parseScene :: Text -> Either String Scene
parseScene text = do
    sceneFile <- parseSceneFile text
    let finalState = execStateT (processSceneFile sceneFile) initialState 
    (_, _, _, sceneObjects, lightSources, camera) <- finalState -- ToDo: use camera here
    when (isNothing camera) (throwError "No camera defined")
    pure (Scene sceneObjects lightSources)

parseSceneFromFile :: FilePath -> IO (Either String Scene)
parseSceneFromFile path = parseScene <$> readFile path

defaultCamera :: Camera
defaultCamera = Camera
    { cameraPosition   = V3 -0.001 0.01 0.01
    , cameraTopLeft    = V3 -0.5  0.5 -0.5
    , cameraTopRight   = V3  0.5  0.5 -0.5
    , cameraBottomLeft = V3 -0.5 -0.5 -0.5
    }

processSceneFile :: SceneFile -> StateT SceneState (Either String) ()
processSceneFile = mapM_ $ \item -> do
    let handleError = maybe (throwError "Bad index") pure
    (vertices, colors, materials, objects, lightSources, camera) <- get
    case item of 
        ItemVertex x y z 
            -> put (vertices |> V3 x y z, colors, materials, objects, lightSources, camera)
        ItemColor r g b 
            -> put (vertices, colors |> V3 r g b, materials, objects, lightSources, camera)
        ItemMaterial nAmbient nDiffuse nSpecular nReflected shininess -> do
            ambient <- handleError (colors !? nAmbient)
            diffuse <- handleError (colors !? nDiffuse)
            specular <- handleError (colors !? nSpecular)
            reflected <- handleError (colors !? nReflected)
            let newMaterial = Material ambient diffuse specular reflected shininess
            put (vertices, colors, materials |> newMaterial, objects, lightSources, camera)
        ItemSphere nPosition radius nMaterial -> do
            position <- handleError (vertices !? nPosition)
            material <- handleError (materials !? nMaterial)
            let newSceneObject = Sphere $ MkSphere position radius material
            put (vertices, colors, materials, newSceneObject : objects, lightSources, camera)
        ItemTriangle nA nB nC nMaterial -> do
            a <- handleError (vertices !? nA)
            b <- handleError (vertices !? nB)
            c <- handleError (vertices !? nC)
            material <- handleError (materials !? nMaterial)
            let newSceneObject = Triangle $ MkTriangle a b c material
            put (vertices, colors, materials, newSceneObject : objects, lightSources, camera)
        ItemLightSource nPosition nColor -> do
            position <- handleError (vertices !? nPosition)
            color <- handleError (colors !? nColor)
            let newLightSource = LightSource position color
            put (vertices, colors, materials, objects, newLightSource : lightSources, camera)
        ItemCamera nPosition nLookAt nUp -> do
            position <- handleError (vertices !? nPosition)
            lookAt <- handleError (vertices !? nLookAt)
            up <- handleError (vertices !? nUp)
            let newCamera = 
                    if True 
                        then Just defaultCamera 
                        else undefined position lookAt up  -- ToDo: create sane camera
            case camera of
                Nothing -> put (vertices, colors, materials, objects, lightSources, newCamera)
                Just _ -> throwError "Camera defined many times"
