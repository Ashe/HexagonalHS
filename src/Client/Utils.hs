-- A module containing helpful functions unrelated to other modules
module Client.Utils
  ( withWindow
  ) where

import qualified Graphics.UI.GLFW as GLFW
import Control.Monad (when)

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
