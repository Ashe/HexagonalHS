-- A module defining how an app is structured
module Client.App
  ( App (..)
  , Scene (..)
  , Env (..)
  , State (..)
  , Event (..)
  ) where

import qualified Graphics.UI.GLFW as GLFW
import Control.Monad.RWS.Strict (RWST)
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
data State = State 
  { stateWindowSize       :: V2 Int
  , stateTime             :: Double
  , stateDeltaTime        :: Double
  , stateDeltaTimeRaw     :: Double
  , stateDeltaTimeScale   :: Double
  , stateMousePos         :: V2 Double
  , stateDeltaMousePos    :: V2 Double
  , stateMouseDrag        :: Map GLFW.MouseButton (V2 Double)
  , stateGlobalUniforms   :: Map String Uniform
  }

-- Something that can run in the app
class Scene s where

  -- Respond to keypresses
  handleEvent :: s -> Event -> App ()

  -- Update the scene every frame
  update      :: s -> Double -> App ()

  -- Render the scene every frame
  render      :: s -> App ()
