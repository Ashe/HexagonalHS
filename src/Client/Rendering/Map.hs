-- A module dedicated to rendering the game map
module Client.Rendering.Map
  ( renderMap
  ) where

import Control.Monad.RWS.Strict (liftIO, ask)

import Tides.Map

import Client.App hiding (render)
import Client.App.Resources
import Client.Rendering.Renderable

-- Render every tile on the map
renderMap :: Map -> [Uniform] -> App ()
renderMap map uniforms = do

  -- Retrieve resources
  Env { envResources = rs } <- ask

  -- Obtain the mesh and shader from resources (or crash)
  mesh    <- liftIO $ getMesh   rs "hexagonal_prism"
  shader  <- liftIO $ getShader rs "simple"

  -- Render a single tile
  liftIO $ render mesh shader uniforms
