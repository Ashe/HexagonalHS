module Client.App
  ( App (..)
  , Env (..)
  , State (..)
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
