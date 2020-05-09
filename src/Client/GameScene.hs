-- A module defining the scene used when playing the game
module Client.GameScene
  ( createGameScene
  ) where

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.GL.CoordTrans as GL
import Graphics.Rendering.OpenGL (($=))
import Control.Monad (when, unless, void)
import GHC.Float (double2Float)
import Linear.V3
import Linear.OpenGL
import Linear.Matrix
import Linear.Projection
import Control.Monad.RWS.Strict
import Control.Concurrent.MVar
import Data.Maybe (isNothing, fromJust, catMaybes)
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

import Client.App
import Client.App.Resources
import Client.App.Resources.Shader
import Client.Camera

-- The game to play
data GameScene = GameScene 
  { gameSceneCamera      :: MVar Camera
  , gameSceneDescriptor  :: MVar Descriptor
  }

-- Define how this scene is interacted with
instance Scene GameScene where
  handleEvent = onHandleEvent
  update      = onUpdate
  render      = onRender

-- Describe an object to be rendered
data Descriptor = Descriptor 
  GL.VertexArrayObject 
  GL.ArrayIndex 
  GL.NumArrayIndices
  GL.Program

-- Easily create a blank gamescene
createGameScene :: App GameScene
createGameScene = do

  -- Generate and bind VAO
  vao <- liftIO GL.genObjectName
  GL.bindVertexArrayObject $= Just vao

  -- Define triangles
  let vertices :: [GL.Vertex3 GL.GLfloat]
      vertices = [
        GL.Vertex3 (-0.5) (-0.5) 0,  -- Triangle 1
        GL.Vertex3 0.5 (-0.5) 0,
        GL.Vertex3 0 0.5 0]
      numVertices = length vertices

  -- Generate and bind VBO
  vbo <- liftIO GL.genObjectName
  GL.bindBuffer GL.ArrayBuffer $= Just vbo

  -- Load vertex data into buffer
  liftIO $ withArray vertices $ \ptr -> do
    let size = fromIntegral (numVertices * sizeOf (head vertices))
    GL.bufferData GL.ArrayBuffer $= (size, ptr, GL.StaticDraw)

  -- Retrieve shader from resources
  Env { envResources = rs } <- ask
  maybeShader <- liftIO $ getShader rs "simple"
  when (isNothing maybeShader) $ error "Could not find shader"
  let program = shaderProgram (fromJust maybeShader)

  -- Specify what shader program to use
  GL.currentProgram GL.$= Just program

  -- Specify and enable location attribute
  let firstIndex = 0
      vPosition = GL.AttribLocation 0
  GL.vertexAttribPointer vPosition $=
    (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float 0 
      (bufferOffset firstIndex))
  GL.vertexAttribArray vPosition $= GL.Enabled

  -- Create a camera
  camera <- liftIO newEmptyMVar
  let c = createCamera (V3 0 0 1) 0 270
  liftIO $ putMVar camera c

  -- Store information about how to render the vertices
  descriptor <- liftIO newEmptyMVar
  liftIO $ putMVar descriptor $ 
    Descriptor vao firstIndex (fromIntegral numVertices) program

  -- Create a GameScene with this information
  pure $ GameScene camera descriptor

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

  -- Prepare to move the camera
  let keybindings :: [(GLFW.Key, V3 Float)]
      keybindings = 
        [ (GLFW.Key'W, cameraForward camera)
        , (GLFW.Key'A, - cameraRight camera)
        , (GLFW.Key'S, - cameraForward camera)
        , (GLFW.Key'D, cameraRight camera)
        , (GLFW.Key'Space, V3 0 1 0)
        , (GLFW.Key'C, V3 0 (-1) 0) ]
      move :: (GLFW.Key, V3 Float) -> IO (V3 Float)
      move (k, d) = do
        status <- GLFW.getKey window k
        pure $ case status of
          GLFW.KeyState'Pressed -> d
          _ -> V3 0 0 0
      calcMove :: IO (V3 Float)
      calcMove = foldM (\n m -> (+n) <$> move m) (V3 0 0 0) keybindings

  -- Move the camera
  movement <- liftIO calcMove
  let speed :: Float
      speed = 10.0 * double2Float dt
      destination = cameraPosition camera + ((* speed) <$> movement)
      pitch = cameraPitch camera
      yaw = cameraYaw camera
      newCamera = createCamera destination pitch yaw

  -- Replace the old camera with the new one
  liftIO $ putMVar (gameSceneCamera scene) newCamera

-- Display the scene
onRender :: GameScene -> App ()
onRender gs = liftIO $ do

  -- Get data to render
  (Descriptor vao index count program) <- readMVar $ gameSceneDescriptor gs

  -- Bind shader to use
  GL.currentProgram $= Just program

  -- Bind VAO
  GL.bindVertexArrayObject $= Just vao

  -- Get the view matrix from the camera
  Camera {cameraView = view} <- readMVar $ gameSceneCamera gs

  -- Construct view and projection matrices for the shaders
  let proj = perspective 1.5 (1920.0 / 1080.0) 0.5 100 :: M44 Float

  -- Give shaders correct matrices
  viewLoc <- GL.uniformLocation program "view"
  projLoc <- GL.uniformLocation program "projection"
  GL.uniform viewLoc $= view
  GL.uniform projLoc $= proj

  -- Draw vertices as triangles
  GL.drawArrays GL.Triangles index count

  -- Unbind VAO
  GL.bindVertexArrayObject $= Nothing

  -- Unbind shader
  GL.currentProgram $= Nothing

--------------------------------------------------------------------------------

-- Creates a pointer to data
bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral
