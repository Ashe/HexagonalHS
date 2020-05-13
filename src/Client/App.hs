{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

-- A module defining how an app is structured
module Client.App
  ( App (..)
  , Scene (..)
  , Env (..)
  , State (..)
  , Event (..)
  , Uniform (..)
  , NullScene (..)
  ) where

import qualified Graphics.UI.GLFW as GLFW
import Control.Monad.RWS.Strict (RWST, liftIO)
import Control.Concurrent.STM (TQueue)
import Control.Concurrent.MVar (MVar)
import Data.Map.Strict
import Linear.V2

import Client.App.Resources
import Client.App.Event
import Client.App.Uniform

-- Contains information about the game
type App = RWST Env [Int] State IO

-- Read-only environment data
data Env = Env
  { envWindow             :: !GLFW.Window
  , envResources          :: MVar Resources
  , envEventsChan         :: TQueue Event
  }

-- Data to be modified in game
data State = forall s. Scene s => State 

  -- Application
  { stateWindowSize       :: V2 Int
  , stateTime             :: Double
  , stateDeltaTime        :: Double
  , stateDeltaTimeRaw     :: Double
  , stateDeltaTimeScale   :: Double

  -- Input
  , stateMousePos         :: V2 Double
  , stateDeltaMousePos    :: V2 Double
  , stateMouseDrag        :: Map GLFW.MouseButton (V2 Double)

  -- Rendering
  , stateGlobalUniforms   :: Map String Uniform

  -- Scene management
  , stateScene            :: s
  }

-- Scenes determine what the user can see and interact with
class Scene s where

  -- Respond to keypresses
  handleEvent :: s -> Event -> App ()

  -- Update the scene every frame
  update      :: s -> Double -> App ()

  -- Render the scene to the screen
  render      :: s -> [Uniform] -> App ()

--------------------------------------------------------------------------------

-- A scene that does nothing
data NullScene    = NullScene
instance Scene NullScene where
  handleEvent _ _ = pure ()
  render _ _      = pure ()
  update _ _      = liftIO $ do 
    putStrLn "[Note] Attempted to update a NullScene."
    putStrLn "[Note] Please ensure that State contains a valid Scene."
    putStrLn "[Note] Program will be terminated.."
    GLFW.terminate
