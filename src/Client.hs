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
import Client.Utils

-- Entry point
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
startGame env state = void $ evalRWST (resizeGame >> game) env state

-- Define main game loop
game :: App ()
game = do

  -- Retrieve the window
  win <- asks envWindow

  -- Poll and process events
  handleEvents

  -- Render the game
  liftIO $ GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  -- @TODO: Render

  -- Swap buffers
  liftIO $ do
    GLFW.swapBuffers win
    GL.flush

  -- Proceed to the next game frame unless quitting
  shouldQuit <- liftIO $ GLFW.windowShouldClose win
  unless shouldQuit game

--------------------------------------------------------------------------------

-- Respond to user input and window events
handleEvents :: App ()
handleEvents = do
  liftIO GLFW.pollEvents
  queue <- asks envEventsChan
  maybeEvents <- liftIO $ atomically $ tryReadTQueue queue
  case maybeEvents of
    Just event -> do
      handleEvent event
      handleEvents
    Nothing -> pure ()

-- Handle each event individually
handleEvent :: Event -> App ()
handleEvent ev =
  case ev of

    -- Report errors and close the window
    (EventError e s) -> do
      printEvent "Error" [show e, show s]
      win <- asks envWindow
      liftIO $ GLFW.setWindowShouldClose win True

    -- Perform final actions before program terminates
    (EventWindowClose _) -> printEvent "Close window" []

    -- Resize the window
    (EventFramebufferSize _ width height) -> do
      printEvent "Resize frame" [show width, show height]
      modify $ \s -> s
        { stateWindowWidth  = width
        , stateWindowHeight = height
        }
      resizeGame

    -- Handle mouse clicks
    (EventMouseButton _ mb mbs mk) -> do
      printEvent "Mouse press" [show mb, show mbs, showModifierKeys mk]
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

    -- Handle keypresses
    (EventKey win k scancode ks mk) -> do
      printEvent "Key press" [show k, show scancode, show ks, showModifierKeys mk]
      when (ks == GLFW.KeyState'Pressed) $
        -- Q, Esc: exit
        when (k == GLFW.Key'Q || k == GLFW.Key'Escape) $
          liftIO $ GLFW.setWindowShouldClose win True

    -- When a character is entered
    (EventChar _ c) -> pure ()

    -- Otherwise do nothing
    _ -> pure ()

-- Outputs the event to terminal
printEvent :: String -> [String] -> App ()
printEvent cbname fields =
    liftIO $ putStrLn $ cbname ++ ": " ++ unwords fields

-- Outputs modifer keys for debugging
showModifierKeys :: GLFW.ModifierKeys -> String
showModifierKeys mk =
    "[mod keys: " ++ keys ++ "]"
  where
    keys = if null xs then "none" else unwords xs
    xs = catMaybes ys
    ys = [ if GLFW.modifierKeysShift   mk then Just "shift"   else Nothing
         , if GLFW.modifierKeysControl mk then Just "control" else Nothing
         , if GLFW.modifierKeysAlt     mk then Just "alt"     else Nothing
         , if GLFW.modifierKeysSuper   mk then Just "super"   else Nothing
         ]

--------------------------------------------------------------------------------

-- Notify program when the window size has been changed
resizeGame :: App ()
resizeGame = liftIO $ putStrLn "resizeGame not yet implemented"

--------------------------------------------------------------------------------
