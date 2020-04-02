module Client
  ( initialise
  ) where

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Control.Monad (void)
import Control.Concurrent.STM (newTQueueIO)
import Control.Monad.RWS.Strict (RWST, evalRWST, liftIO)
import Control.Concurrent (threadDelay)

import Client.Utils
import Client.Event

-- Entry point
initialise :: Int -> Int -> IO ()
initialise w h = do

  -- Create event queue
  eventsChan <- newTQueueIO :: IO (TQueue Event)

  -- Specify how the game should be set up and played
  let state = State 0 0
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

