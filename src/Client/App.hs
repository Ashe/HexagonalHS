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

import Client.App.Event

-- Contains information about the game
type App = RWST Env [Int] State IO

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

-- Something that can run in the app
class Scene s where

  -- Initialise the scene before play
  begin       :: s -> App ()

  -- Respond to keypresses
  handleEvent :: s -> Event -> App ()

  -- Update the scene every frame
  update      :: s -> Double -> App s

  -- Render the scene every frame
  render      :: s -> App ()

  -- Notify the scene of changes in window size
  resize      :: s -> App ()
