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

import Client.Utils

-- Read-only environment data
data Env = Env
  { envWindow         :: !GLFW.Window
  , envEventsChan     :: TQueue Event
  }

-- Data to be modified in game
data State = State 
  { stateWindowWidth  :: Int
  , stateWindowHeight :: Int
  , stateMouseDown    :: Bool
  , stateDragging     :: Bool
  , stateDragStartX   :: Double
  , stateDragStartY   :: Double
  }

-- Contains information about the game
type App = RWST Env [Int] State IO

--------------------------------------------------------------------------------

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

-- Types of event
data Event =
    EventError           !GLFW.Error !String
  | EventWindowPos       !GLFW.Window !Int !Int
  | EventWindowSize      !GLFW.Window !Int !Int
  | EventWindowClose     !GLFW.Window
  | EventWindowRefresh   !GLFW.Window
  | EventWindowFocus     !GLFW.Window !Bool
  | EventWindowIconify   !GLFW.Window !Bool
  | EventFramebufferSize !GLFW.Window !Int !Int
  | EventMouseButton     !GLFW.Window !GLFW.MouseButton !GLFW.MouseButtonState !GLFW.ModifierKeys
  | EventCursorPos       !GLFW.Window !Double !Double
  | EventCursorEnter     !GLFW.Window !GLFW.CursorState
  | EventScroll          !GLFW.Window !Double !Double
  | EventKey             !GLFW.Window !GLFW.Key !Int !GLFW.KeyState !GLFW.ModifierKeys
  | EventChar            !GLFW.Window !Char
  deriving Show

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

-- Helper function for adding to queue
toQueue :: TQueue Event -> Event -> IO ()
toQueue tc e = atomically $ writeTQueue tc e

-- Write an appropriate Event to the events to TQueue
errorCallback           :: TQueue Event -> GLFW.Error -> String                                                           -> IO ()
windowPosCallback       :: TQueue Event -> GLFW.Window -> Int -> Int                                                      -> IO ()
windowSizeCallback      :: TQueue Event -> GLFW.Window -> Int -> Int                                                      -> IO ()
windowCloseCallback     :: TQueue Event -> GLFW.Window                                                                    -> IO ()
windowRefreshCallback   :: TQueue Event -> GLFW.Window                                                                    -> IO ()
windowFocusCallback     :: TQueue Event -> GLFW.Window -> Bool                                                            -> IO ()
windowIconifyCallback   :: TQueue Event -> GLFW.Window -> Bool                                                            -> IO ()
framebufferSizeCallback :: TQueue Event -> GLFW.Window -> Int -> Int                                                      -> IO ()
mouseButtonCallback     :: TQueue Event -> GLFW.Window -> GLFW.MouseButton -> GLFW.MouseButtonState -> GLFW.ModifierKeys  -> IO ()
cursorPosCallback       :: TQueue Event -> GLFW.Window -> Double -> Double                                                -> IO ()
cursorEnterCallback     :: TQueue Event -> GLFW.Window -> GLFW.CursorState                                                -> IO ()
scrollCallback          :: TQueue Event -> GLFW.Window -> Double -> Double                                                -> IO ()
keyCallback             :: TQueue Event -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys           -> IO ()
charCallback            :: TQueue Event -> GLFW.Window -> Char                                                            -> IO ()

errorCallback           tc e s            = toQueue tc $ EventError           e s
windowPosCallback       tc win x y        = toQueue tc $ EventWindowPos       win x y
windowSizeCallback      tc win w h        = toQueue tc $ EventWindowSize      win w h
windowCloseCallback     tc win            = toQueue tc $ EventWindowClose     win
windowRefreshCallback   tc win            = toQueue tc $ EventWindowRefresh   win
windowFocusCallback     tc win fa         = toQueue tc $ EventWindowFocus     win fa
windowIconifyCallback   tc win ia         = toQueue tc $ EventWindowIconify   win ia
framebufferSizeCallback tc win w h        = toQueue tc $ EventFramebufferSize win w h
mouseButtonCallback     tc win mb mba mk  = toQueue tc $ EventMouseButton     win mb mba mk
cursorPosCallback       tc win x y        = toQueue tc $ EventCursorPos       win x y
cursorEnterCallback     tc win ca         = toQueue tc $ EventCursorEnter     win ca
scrollCallback          tc win x y        = toQueue tc $ EventScroll          win x y
keyCallback             tc win k sc ka mk = toQueue tc $ EventKey             win k sc ka mk
charCallback            tc win c          = toQueue tc $ EventChar            win c

--------------------------------------------------------------------------------

-- Gives GLFW appropriate callbacks
provideCallbacks :: TQueue Event -> GLFW.Window -> IO ()
provideCallbacks events win = do
  GLFW.setErrorCallback               $ Just $ errorCallback           events
  GLFW.setWindowPosCallback       win $ Just $ windowPosCallback       events
  GLFW.setWindowSizeCallback      win $ Just $ windowSizeCallback      events
  GLFW.setWindowCloseCallback     win $ Just $ windowCloseCallback     events
  GLFW.setWindowRefreshCallback   win $ Just $ windowRefreshCallback   events
  GLFW.setWindowFocusCallback     win $ Just $ windowFocusCallback     events
  GLFW.setWindowIconifyCallback   win $ Just $ windowIconifyCallback   events
  GLFW.setFramebufferSizeCallback win $ Just $ framebufferSizeCallback events
  GLFW.setMouseButtonCallback     win $ Just $ mouseButtonCallback     events
  GLFW.setCursorPosCallback       win $ Just $ cursorPosCallback       events
  GLFW.setCursorEnterCallback     win $ Just $ cursorEnterCallback     events
  GLFW.setScrollCallback          win $ Just $ scrollCallback          events
  GLFW.setKeyCallback             win $ Just $ keyCallback             events
  GLFW.setCharCallback            win $ Just $ charCallback            events

