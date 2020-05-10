{-# LANGUAGE BangPatterns #-}

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
import Data.Map.Strict (insert, empty)
import Linear.V2

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
  maybeResources <- loadResourcesFrom "assets"
  case maybeResources of

    -- If resources were loaded
    (Just rs) -> do

      -- Wrap resources into mvar
      resources <- newMVar rs

      -- Specify how the game should be set up and played
      let state time = State 
            { stateWindowSize = V2 0 0
            , stateTime = time
            , stateDeltaTime = 0.0
            , stateDeltaTimeRaw = 0.0
            , stateDeltaTimeScale = 1.0
            , stateMousePos = V2 0 0
            , stateDeltaMousePos = V2 0.0 0.0
            , stateMouseDrag = empty
            , stateGlobalUniforms = empty }
          play window = do
            let env = Env window resources eventsChan
            provideCallbacks eventsChan window
            setupOpenGL
            now <- getNow
            startGame env $ state now

      -- Run a function in the GLFW window
      withWindow w h "Tides of Magic" play

    -- If resources fail to load, don't initialise window
    _ -> do
      putStrLn "[Error] Resources could not be loaded."
      pure ()

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
          scene <- createGameScene
          resizeWindow
          run scene

-- Define main game loop
run :: Scene s => s -> App ()
run !scene = do

  -- Retrieve the window from environment
  win <- asks envWindow

  -- Retrieve state
  state <- get

  -- Calculate delta time
  now <- liftIO getNow
  let dt = now - stateTime state
      scaledDt = dt * stateDeltaTimeScale state

  -- Poll and process events
  liftIO GLFW.pollEvents
  processEvents scene

  -- Calculate current mouse position and delta
  mousePos <- liftIO $ (\(x, y) -> V2 x y) <$> GLFW.getCursorPos win
  let mouseDelta = mousePos - stateMousePos state

  -- Update state with delta-frame information
  put $ state
    { stateTime           = now
    , stateDeltaTime      = scaledDt
    , stateDeltaTimeRaw   = dt
    , stateMousePos       = mousePos
    , stateDeltaMousePos  = mouseDelta
    }

  -- Update the scene
  update scene scaledDt

  -- Render the game
  liftIO $ GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  render scene

  -- Swap buffers
  liftIO $ do
    GLFW.swapBuffers win
    GL.flush

  -- Proceed to the next game frame unless quitting
  shouldQuit <- liftIO $ GLFW.windowShouldClose win
  unless shouldQuit $ run scene

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
processEvents :: Scene s => s -> App ()
processEvents scene = do
  queue <- asks envEventsChan
  maybeEvents <- liftIO $ atomically $ tryReadTQueue queue
  case maybeEvents of
    Just event -> do
      processEvent scene event
      processEvents scene
    Nothing -> pure ()

-- Process the event and then give it to the scene for processing
processEvent :: Scene s => s -> Event -> App ()
processEvent scene ev = do
  case ev of

    -- Report errors and close the window
    (EventError e s) -> do
      liftIO $ putStrLn $ "[Error] " ++ show e ++ ", " ++ show s
      win <- asks envWindow
      liftIO $ GLFW.setWindowShouldClose win True

    -- Perform final actions before program terminates
    (EventWindowClose _) -> liftIO $ putStrLn "Closing application.."

    -- Handle window resizing
    (EventFramebufferSize _ width height) -> do
      modify $ \s -> s
        { stateWindowSize = V2 width height }
      resizeWindow

    -- Handle mouse buttons
    (EventMouseButton _ mb mbs mk) ->
      when (mbs == GLFW.MouseButtonState'Pressed) $
        modify $ \s -> s
          { stateMouseDrag = insert mb (stateMousePos s) (stateMouseDrag s) }

    -- Otherwise do nothing
    _ -> pure ()

  -- Finally, pass the event to the scene
  handleEvent scene ev
