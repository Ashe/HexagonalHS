-- A module defining the scene used when playing the game
module Client.GameScene
  ( createGameScene
  ) where

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.GL.CoordTrans as GL
import Graphics.Rendering.OpenGL (($=))
import Control.Monad (foldM, when, unless, void)
import Control.Monad.RWS.Strict (liftIO, ask, get, put)
import Control.Concurrent.MVar
import Data.Maybe (isNothing, fromJust, catMaybes)
import Data.Map.Strict (insert)
import GHC.Float (double2Float)
import Foreign.Ptr (Ptr, plusPtr, nullPtr)
import Linear.OpenGL
import Linear.Affine
import Linear.V2
import Linear.V3
import Linear.Matrix
import Linear.Projection

import Client.App
import Client.App.Uniform
import Client.App.Resources
import Client.App.Resources.Shader
import Client.Camera
import Client.Rendering.Mesh
import Client.Rendering.Geometry.Hexagon

-- The game to play
data GameScene = GameScene 
  { gameSceneCamera :: MVar Camera
  , gameSceneMesh   :: MVar Mesh
  }

-- Define how this scene is interacted with
instance Scene GameScene where
  handleEvent = onHandleEvent
  update      = onUpdate
  render      = onRender

-- Easily create a blank gamescene
createGameScene :: App GameScene
createGameScene = do

  -- Generate and bind VAO
  vao <- liftIO GL.genObjectName
  GL.bindVertexArrayObject $= Just vao

  -- Draw a hexagon
  let (vertices, indices) = hexagonalPrism 0.25

  liftIO $ mapM print indices

  -- Retrieve shader from resources
  Env { envResources = rs } <- ask
  maybeShader <- liftIO $ getShader rs "simple"
  when (isNothing maybeShader) $ error "Could not find shader"
  let program = shaderProgram (fromJust maybeShader)

  -- Create a camera
  camera <- liftIO newEmptyMVar
  let c = createCamera (V3 0 2 2) (-30) 270
  liftIO $ putMVar camera c

  -- Store information about how to render the vertices
  mesh <- liftIO newEmptyMVar
  createMesh vertices indices program [] >>= liftIO . putMVar mesh

  -- Create a GameScene with this information
  pure $ GameScene camera mesh

--------------------------------------------------------------------------------

-- Process inputs
onHandleEvent :: GameScene -> Event -> App ()
onHandleEvent scene EventMouseButton {} = liftIO $ putStrLn "Click!"
onHandleEvent _ _ = pure ()

-- Update entities in the scene
onUpdate :: GameScene -> Double -> App ()
onUpdate scene dt = do

  -- Retrieve the window
  Env{envWindow = window} <- ask

  -- Retrieve the camera from the GameScene
  camera <- liftIO $ takeMVar $ gameSceneCamera scene

  -- Retrieve the App State and mouse position
  st <- get
  mousePos <- liftIO $ GLFW.getCursorPos window
  mouseStatus <- liftIO $ GLFW.getMouseButton window GLFW.MouseButton'2

  -- Disable the cursor when looking around
  liftIO $ GLFW.setCursorInputMode window $
    case mouseStatus of
      GLFW.MouseButtonState'Pressed -> GLFW.CursorInputMode'Disabled
      _ -> GLFW.CursorInputMode'Normal

  -- Turn camera on left click
  let sensitivity = 0.1
      (V2 x y) = (* sensitivity) . double2Float <$> stateDeltaMousePos st
      previousAngle@(pitch, yaw) = (cameraPitch camera, cameraYaw camera)
      (pitch', yaw') = case mouseStatus of
        GLFW.MouseButtonState'Pressed ->
            (min 79.0 (max (-79.0) (pitch - y)), yaw + x)
        _ -> previousAngle

      -- Set up camera keybindings
      keybindings :: [(GLFW.Key, V3 Float)]
      keybindings = 
        [ (GLFW.Key'W, cameraForward camera)
        , (GLFW.Key'A, - cameraRight camera)
        , (GLFW.Key'S, - cameraForward camera)
        , (GLFW.Key'D, cameraRight camera)
        , (GLFW.Key'Space, V3 0 1 0)
        , (GLFW.Key'C, V3 0 (-1) 0) ]

      -- Prepare to move the camera
      move :: (GLFW.Key, V3 Float) -> IO (V3 Float)
      move (k, d) = do
        status <- GLFW.getKey window k
        pure $ case status of
          GLFW.KeyState'Pressed -> d
          _ -> V3 0 0 0

      -- Move the camera depending on keys pressed
      calcMove :: IO (V3 Float)
      calcMove = foldM (\n m -> (+n) <$> move m) (V3 0 0 0) keybindings

  -- Update the camera
  movement <- liftIO calcMove
  let speed :: Float
      speed = 2.0 * double2Float dt
      destination = cameraPosition camera + ((* speed) <$> movement)
      newCamera = createCamera destination pitch' yaw'

  -- Replace the old camera with the new one
  liftIO $ putMVar (gameSceneCamera scene) newCamera

  -- Get the view and projection matrices from the camera
  let view = cameraView newCamera
      proj = getProjectionMatrix

  -- Update global uniforms
  let uniforms = [Uniform "projection" proj, Uniform "view" view]
      globalUniforms = stateGlobalUniforms st
  put $ st{ stateGlobalUniforms = foldl (\m u -> insert (uniformName u) u m) 
      globalUniforms uniforms }

-- Display the scene
onRender :: GameScene -> App ()
onRender gs = do

  -- Render the mesh
  mesh <- liftIO $ readMVar $ gameSceneMesh gs
  renderMesh mesh []

--------------------------------------------------------------------------------

-- Creates a pointer to data
bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral
