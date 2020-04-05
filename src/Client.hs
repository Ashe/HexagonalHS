-- Entry point for client program
module Client
  ( initialise
  ) where

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Control.Monad (when, unless, void)
import Control.Concurrent.STM (newTQueueIO, TQueue, atomically, writeTQueue, tryReadTQueue)
import Control.Monad.RWS.Strict (RWST, evalRWST, liftIO, get, put, asks, modify)
import Control.Concurrent (threadDelay)
import Data.Maybe (catMaybes)

import Client.App
import Client.App.Event
import Client.GameScene
import Client.Utils

-- Create a window and begin to play the game
initialise :: Int -> Int -> IO ()
initialise w h = do

  -- Create event queue
  eventsChan <- newTQueueIO :: IO (TQueue Event)

  -- Specify how the game should be set up and played
  let state = State 0 0 False False 0 0
      play window = 
        provideCallbacks eventsChan window
        >> setupOpenGL
        >> startGame (Env window eventsChan) state

  -- Run a function in the GLFW window
  withWindow w h "Tides of Magic" play

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
  where scene = GameScene
        beginLoop = begin scene >> game scene

-- Define main game loop
game :: Scene s => s -> App ()
game scene = do

  -- Retrieve the window
  win <- asks envWindow

  -- Poll and process events
  handleEvents scene

  -- Update the scene
  update scene 0.0

  -- Render the game
  liftIO $ GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  render scene

  -- Swap buffers
  liftIO $ do
    GLFW.swapBuffers win
    GL.flush

  -- Proceed to the next game frame unless quitting
  shouldQuit <- liftIO $ GLFW.windowShouldClose win
  unless shouldQuit $ game scene

--------------------------------------------------------------------------------

-- Respond to user input and window events
handleEvents :: Scene s => s -> App ()
handleEvents scene = do
  liftIO GLFW.pollEvents
  queue <- asks envEventsChan
  maybeEvents <- liftIO $ atomically $ tryReadTQueue queue
  case maybeEvents of
    Just event -> do
      handleEvent scene event
      handleEvents scene
    Nothing -> pure ()
