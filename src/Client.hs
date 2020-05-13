{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

-- Entry point for client program
module Client
  ( initialise
  ) where

import System.Environment (getProgName)
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Control.Monad (when, unless, void)
import Control.Monad.RWS.Strict (RWST, evalRWST, liftIO, get, put, asks, modify)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (newMVar)
import Control.Concurrent.STM (newTQueueIO, TQueue, atomically, writeTQueue, tryReadTQueue)
import Data.Maybe (catMaybes)
import Data.Map.Strict (insert, empty, elems)
import Linear.V2
import Linear.Matrix

import Client.App
import Client.App.Event
import Client.App.Resources
import Client.GameScene
import Client.Utils

-- Create a window and begin to play the game
initialise :: Int -> Int -> IO ()
initialise w h = do

  -- Welcome message
  progName <- getProgName
  putStrLn $ "\nStarting " ++ progName ++ ".."

  -- Create event queue
  eventsChan <- newTQueueIO :: IO (TQueue Event)

  -- Attempt to load resources
  loadedResources <- loadResourcesFrom "assets"

  -- Wrap resources into mvar
  resources <- newMVar loadedResources

  -- Specify how the game should be set up and played
  let state = State 
        { stateWindowSize = V2 0 0
        , stateTime = 0
        , stateDeltaTime = 0.0
        , stateDeltaTimeRaw = 0.0
        , stateDeltaTimeScale = 1.0
        , stateMousePos = V2 0 0
        , stateDeltaMousePos = V2 0.0 0.0
        , stateMouseDrag = empty
        , stateGlobalUniforms = empty 
        , stateScene = NullScene }
      start window = do
        let env = Env window resources eventsChan
        provideCallbacks eventsChan window
        setupOpenGL
        now <- getNow
        startGame env state

  -- Run a function in the GLFW window
  withWindow w h "Tides of Magic" start

-- Setup OpenGL
setupOpenGL :: IO ()
setupOpenGL = do
  GL.position (GL.Light 0) GL.$= GL.Vertex4 5 5 10 0
  GL.light    (GL.Light 0) GL.$= GL.Enabled
  GL.lighting   GL.$= GL.Enabled
  GL.cullFace   GL.$= Just GL.Back
  GL.depthFunc  GL.$= Just GL.Less
  GL.clearColor GL.$= GL.Color4 0.05 0.05 0.05 1
  GL.normalize  GL.$= GL.Enabled

--------------------------------------------------------------------------------

-- Setup and play the game
startGame :: Env -> State -> IO ()
startGame env state = void $ evalRWST beginLoop env state
  where beginLoop = do

          -- Ensure window is sized correctly
          resizeWindow

          -- Get ticks since program began
          now <- liftIO getNow

          -- Initialise first scene
          scene <- createGameScene
          
          -- Ensure state has everything necessary
          -- @NOTE: RecordWildCards used to avoid bug with Scene
          let idMatUni = Uniform "transform" (identity :: M44 Float)
          modify $ \State{..} -> State
            { stateTime = now
            , stateScene = scene
            , stateGlobalUniforms = 
                insert "transform" idMatUni stateGlobalUniforms
            , .. }

          -- Begin game loop
          run

-- Define main game loop
run :: App ()
run = do

  -- Retrieve the window from environment
  win <- asks envWindow

  -- Get previous mouse position
  State { stateMousePos = previousMousePos } <- get

  -- Poll and process events
  liftIO GLFW.pollEvents
  processEvents

  -- Retrieve state
  state <- get

  -- Calculate delta time and mouse-movement delta
  now <- liftIO getNow
  let dt = now - stateTime state
      scaledDt = dt * stateDeltaTimeScale state
      mouseDelta = stateMousePos state - previousMousePos

  -- Update state with delta-frame information
  put $ state
    { stateTime           = now
    , stateDeltaTime      = scaledDt
    , stateDeltaTimeRaw   = dt
    , stateDeltaMousePos  = mouseDelta
    }

  -- Update the scene
  State { stateScene = scene } <- get
  update scene scaledDt

  -- Render the game with global uniforms
  State { stateScene = scene, stateGlobalUniforms = uniforms } <- get
  liftIO $ GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  render scene (elems uniforms)

  -- Swap buffers
  liftIO $ do
    GLFW.swapBuffers win
    GL.flush

  -- Proceed to the next game frame unless quitting
  shouldQuit <- liftIO $ GLFW.windowShouldClose win
  unless shouldQuit run

--------------------------------------------------------------------------------

-- Resize the window and reposition camera
resizeWindow :: App ()
resizeWindow = do
  s <- get
  let pos   = GL.Position 0 0
      size  = let (V2 w h) = fromIntegral <$> stateWindowSize s in GL.Size w h
  liftIO $ GL.viewport $= (pos, size)

--------------------------------------------------------------------------------

-- Respond to user input and window events
processEvents :: App ()
processEvents = do
  State { stateScene = scene } <- get
  queue <- asks envEventsChan
  maybeEvents <- liftIO $ atomically $ tryReadTQueue queue
  case maybeEvents of
    Just event -> do
      processEvent scene event
      processEvents
    Nothing -> pure ()

-- Process the event and then give it to the scene for processing
processEvent :: Scene s => s -> Event -> App ()
processEvent scene ev = do
  case ev of

    -- Handle mouse movement
    (EventCursorPos _ x y) ->
      modify $ \s -> s{ stateMousePos = V2 x y }

    -- Handle mouse buttons
    (EventMouseButton _ mb mbs mk) ->
      when (mbs == GLFW.MouseButtonState'Pressed) $
        modify $ \s -> s
          { stateMouseDrag = insert mb (stateMousePos s) (stateMouseDrag s) }

    -- Handle window resizing
    (EventFramebufferSize _ width height) -> do
      modify $ \s -> s{ stateWindowSize = V2 width height }
      resizeWindow

    -- Perform final actions before program terminates
    (EventWindowClose _) -> liftIO $ putStrLn "Closing application.."

    -- Report errors and close the window
    (EventError e s) -> do
      liftIO $ putStrLn $ "[Error] " ++ show e ++ ", " ++ show s
      win <- asks envWindow
      liftIO $ GLFW.setWindowShouldClose win True

    -- Otherwise do nothing
    _ -> pure ()

  -- Finally, pass the event to the scene
  handleEvent scene ev
