module Main where

import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import Control.Monad (when)
import Control.Monad.RWS.Strict (RWST, execRWST, lift, ask, tell)
import Control.Concurrent (threadDelay)
import System.Environment (getArgs, getProgName)

-- Read-only data
newtype Env = Env String

-- Data to be modified in game
data State = State 
  { foo :: Int
  , bar :: Int
  }

-- Contains information about the game
type App a = RWST Env [Int] State IO a

-- Entry point
main :: IO ()
main = do

  -- Start main loop if GLFW initialised succeessfully
  r <- GLFW.init
  when r $ do

    -- Set error callback in case initialisation fails
    GLFW.setErrorCallback $ Just simpleErrorCallback

    -- Open window
    m <- GLFW.createWindow 400 400 "GLFW Demo" Nothing Nothing
    case m of
      (Just win) -> do

        -- Set the color to clear background
        GL.clearColor $= GL.Color4 0 0 0 0

        -- Set callbacks
        GLFW.setWindowSizeCallback win $ Just windowSizeCallback

        -- @TODO: Do main loop here
        (s, o) <- execRWST play (Env "Hello World") (State 0 1)

        -- Destroy window
        GLFW.destroyWindow win

      -- If the window wasn't created, do nothing
      Nothing -> pure()

    -- Terminate GLFW and close program
    GLFW.terminate

  where
    
    -- Report errors
    simpleErrorCallback :: GLFW.ErrorCallback
    simpleErrorCallback e s = putStrLn $ unwords [show e, show s]

    -- Change viewport on resize
    windowSizeCallback :: GLFW.WindowSizeCallback
    windowSizeCallback win w h = do
      let size = GL.Size (fromIntegral w) (fromIntegral h)
      GL.viewport   $= (GL.Position 0 0, size)
      GL.matrixMode $= GL.Projection
      GL.loadIdentity
      GL.ortho2D 0 (realToFrac w) (realToFrac h) 0

-- Run the actual game within app context
play :: App ()
play = do
  (Env msg) <- ask
  lift $ do
    threadDelay 2000000
    putStrLn msg
