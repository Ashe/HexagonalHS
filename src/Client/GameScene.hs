-- A module defining the scene used when playing the game
module Client.GameScene
  ( GameScene (..)
  ) where

import qualified Graphics.UI.GLFW as GLFW
import Control.Monad (when, unless, void)
import Control.Monad.RWS.Strict (RWST, evalRWST, liftIO, get, put, asks, modify)
import Data.Maybe (catMaybes)

import Client.App

-- The game to play
data GameScene = GameScene

-- Define how this scene is interacted with
instance Scene GameScene where

  -- Initialise scene
  begin scene = liftIO $ putStrLn "Beginning GameScene"

  -- Process inputs
  handleEvent scene EventMouseButton {} = liftIO $ putStrLn "Click!"
  handleEvent scene _ = pure ()

  -- Update entities in the scene
  update scene dt = pure ()

  -- Display the scene
  render scene = pure ()

  -- Resize the scene
  resize scene =
    liftIO $ putStrLn "resize GameScene not yet implemented"
