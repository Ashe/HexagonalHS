-- A module dedicated to rendering the game map
module Client.Rendering.Map
  ( render
  ) where

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Control.Monad (unless, forM_)
import Control.Monad.RWS.Strict (liftIO, ask)
import Data.Map.Strict (toList)
import qualified Data.Map.Strict as D
import Data.Vector.Storable (fromList)
import Linear.OpenGL
import Linear.Vector
import Linear.Matrix
import Linear.V2
import Linear.V3
import Linear.V4

import Foreign.Ptr (Ptr, plusPtr, nullPtr)
import Foreign.Storable (sizeOf)

import HexagonalHS.Map

import Client.App hiding (render)
import Client.App.Resources
import Client.App.Resources.Mesh
import Client.Rendering.Renderable
import Client.Rendering.Mesh
import Client.Rendering.Geometry.Hexagon

-- Allow maps to be rendered
instance Renderable Map where
  render = renderMap

--------------------------------------------------------------------------------

-- Render every tile on the map
renderMap :: Map -> Shader -> [Uniform] -> App ()
renderMap map shader globalUniforms = do

  -- Retrieve resources
  Env { envResources = rs } <- ask

  -- Obtain the mesh from resources (or crash)
  mesh <- liftIO $ getMesh rs "hexagonal_prism"

  -- Apply global uniforms to all tiles (this also binds the shader)
  applyUniforms shader globalUniforms

  -- Bind VAO of mesh
  GL.bindVertexArrayObject $= Just (meshVAO mesh)

  -- Prepare to render the map
  let tiles = assembleTile <$> toList (mapTiles map)
      tileInfo = Uniform "tiles" (UniformDataMulti $ fromList tiles)
      offset = bufferOffset $ meshFirstIndex mesh

  -- Provide tile information to the shaders for instanced rendering
  applyUniforms shader [tileInfo]

  -- Render the map via instances of the hexagonal mesh
  liftIO $ GL.drawElementsInstanced 
      GL.Triangles (meshNumIndices mesh) GL.UnsignedInt 
      offset (fromIntegral $ length tiles)

--------------------------------------------------------------------------------

-- Assemble information about a tile to be sent to shaders
assembleTile :: (Index, Int) -> GL.Vector3 Float
assembleTile ((x, z), height) = GL.Vector3 x' z' (fromIntegral height)
  where V2 x' z' = tessellateHexagon $ V2 x z

-- Create the relevant uniforms for a given tile
makeUniforms :: Index -> Int -> [Uniform]
makeUniforms pos height = [transform]
  where transform = Uniform "transform" 
          (UniformData $ makeTransform pos height)

-- Create a transformation matrix based on the tile's coordinates and height
makeTransform :: Index -> Int -> M44 Float
makeTransform (x, y) height = translate !*! scale
  where scale :: M44 Float
        translate :: M44 Float
        scale = scaled (V4 1 (fromIntegral height) 1 1)
        V2 x' y' = tessellateHexagon (V2 x y)
        translate = mkTransformationMat identity (V3 x' 0 y')

--------------------------------------------------------------------------------

-- Creates a pointer to data
-- @NOTE: DUPLICATED IN Resources/Mesh.hs AND Rendering/Mesh.hs
bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral
