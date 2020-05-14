-- A module dedicated to rendering the game map
module Client.Rendering.Map
  ( render
  ) where

import Control.Monad (unless, forM_)
import Control.Monad.RWS.Strict (liftIO, ask)
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
import Client.Rendering.Mesh
import Client.Rendering.Geometry.Hexagon

-- Allow maps to be rendered
instance Renderable Map where
  render = renderMap

--------------------------------------------------------------------------------

-- Render every tile on the map
renderMap :: Map -> Shader -> [Uniform] -> App ()
renderMap map shader uniforms = do

  -- Retrieve resources
  Env { envResources = rs } <- ask

  -- Obtain the mesh and shader from resources (or crash)
  mesh    <- liftIO $ getMesh   rs "hexagonal_prism"

  -- Render each tile in the map
  forM_ (toList $ mapTiles map) $ \(index, height) ->
    let uniforms' = uniforms ++ makeUniforms index height in
      unless (height == 0) $ render mesh shader uniforms'

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
