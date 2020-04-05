{-# LANGUAGE BangPatterns #-}

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
        beginLoop = do
          now <- liftIO getNow
          begin scene 
          game scene now

-- Define main game loop
game :: Scene s => s -> Double -> App ()
game !scene !ticks = do

  -- Retrieve the window
  win <- asks envWindow

  -- Calculate delta time
  now <- liftIO getNow
  let dt = now - ticks

  -- Poll and process events
  liftIO GLFW.pollEvents
  processEvents scene

  -- Update the scene
  updatedScene <- update scene dt

  -- Render the game
  liftIO $ GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  render scene

  -- Swap buffers
  liftIO $ do
    GLFW.swapBuffers win
    GL.flush

  -- Proceed to the next game frame unless quitting
  shouldQuit <- liftIO $ GLFW.windowShouldClose win
  unless shouldQuit $ game updatedScene now

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

    -- Resize the window
    (EventFramebufferSize _ width height) ->
      modify $ \s -> s
        { stateWindowWidth  = width
        , stateWindowHeight = height
        }

    -- Handle mouse clicks
    (EventMouseButton _ mb mbs mk) ->
      when (mb == GLFW.MouseButton'1) $ do
        let pressed = mbs == GLFW.MouseButtonState'Pressed
        modify $ \s -> s
          { stateMouseDown = pressed
          }
        unless pressed $
          modify $ \s -> s
            { stateDragging = False
            }

    -- Handle mouse movement
    (EventCursorPos _ x y) -> do
        let x' = round x :: Int
            y' = round y :: Int
        state <- get
        when (stateMouseDown state && not (stateDragging state)) $
          put $ state
            { stateDragging        = True
            , stateDragStartX      = x
            , stateDragStartY      = y
            }

    -- Otherwise do nothing
    _ -> pure ()

  -- Finally, pass the event to the scene
  handleEvent scene ev
