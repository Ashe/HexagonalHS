module Client
  ( initialise
  ) where

import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import Control.Monad (when, void)
import Control.Concurrent.STM (TQueue, newTQueueIO, atomically, writeTQueue)
import Control.Monad.RWS.Strict (RWST, evalRWST, liftIO)
import Control.Concurrent (threadDelay)
import System.Environment (getArgs, getProgName)

-- Entry point
initialise :: IO ()
initialise = do

  -- Check arguments
  args <- getArgs

  -- Determine size of the window
  let defaultSize = (1920, 1080)
      winSize = case args of
        (x : y : xs) -> (read x, read y)
        _ -> defaultSize

  -- Create event queue
  eventsChan <- newTQueueIO :: IO (TQueue Event)

  -- Specify how the game should be set up and played
  let state = State 0 0
      play window = 
        provideCallbacks eventsChan window
        >> setupOpenGL
        >> startGame (Env window eventsChan) state

  -- Run a function in the GLFW window
  uncurry withWindow winSize "Tides of Magic" play

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

-- Execute a function inside a GLFW window
withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow width height title f = do

  -- Attempt to initialise GLFW and report anything
  GLFW.setErrorCallback $ Just simpleErrorCallback
  r <- GLFW.init
  when r $ do

    -- Attempt to create the window
    m <- GLFW.createWindow width height title Nothing Nothing
    case m of

      -- If the window was successfully created
      (Just win) -> do

        -- Focus on the new window's context
        GLFW.makeContextCurrent m

        -- Apply a function to the window
        f win

        -- Destroy the window
        GLFW.destroyWindow win

      -- If no window was created, do nothing
      Nothing -> return ()

    -- Shutdown GLFW 
    GLFW.terminate

  where
    simpleErrorCallback e s =
      putStrLn $ unwords [show e, show s]

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

--------------------------------------------------------------------------------

-- Read-only environment data
data Env = Env
  { envWindow         :: !GLFW.Window
  , envEventsChan     :: TQueue Event
  }

-- Data to be modified in game
data State = State 
  { foo :: Int
  , bar :: Int
  }

-- Contains information about the game
type App = RWST Env [Int] State IO

-- Setup and play the game
startGame :: Env -> State -> IO ()
startGame env state = void $ evalRWST game env state

-- Define main game loop
game :: App ()
game = liftIO $ do
  putStrLn "Wait for it.."
  threadDelay 2000000
  putStrLn "Hello World"

