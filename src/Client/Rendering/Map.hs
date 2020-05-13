-- A module dedicated to rendering the game map
module Client.Rendering.Map
  ( renderMap
  ) where

import Control.Monad.RWS.Strict (liftIO, ask, forM_)
import Data.Map.Strict (toList)
import Linear.OpenGL
import Linear.Vector
import Linear.Matrix
import Linear.V2
import Linear.V3
import Linear.V4

import Tides.Map

import Client.App hiding (render)
import Client.App.Uniform
import Client.App.Resources
import Client.Rendering.Renderable
import Client.Rendering.Geometry.Hexagon

-- Render every tile on the map
renderMap :: Map -> [Uniform] -> App ()
renderMap map uniforms = do

  -- Retrieve resources
  Env { envResources = rs } <- ask

  -- Obtain the mesh and shader from resources (or crash)
  mesh    <- liftIO $ getMesh   rs "hexagonal_prism"
  shader  <- liftIO $ getShader rs "simple"

  -- Render each tile in the map
  liftIO $ forM_ (toList $ mapTiles map) $ \(index, height) ->
    let uniforms' = uniforms ++ makeUniforms index height in
      render mesh shader uniforms'

--------------------------------------------------------------------------------

-- Create the relevant uniforms for a given tile
makeUniforms :: Index -> Int -> [Uniform]
makeUniforms pos height = [transform]
  where transform = Uniform "transform" (makeTransform pos height)

-- Create a transformation matrix based on the tile's coordinates and height
makeTransform :: Index -> Int -> M44 Float
makeTransform (x, y) height = translate !*! scale
  where scale :: M44 Float
        translate :: M44 Float
        scale = scaled (V4 1 (fromIntegral height) 1 1)
        V2 x' y' = tessellateHexagon (V2 x y)
        translate = mkTransformationMat identity (V3 x' 0 y')
