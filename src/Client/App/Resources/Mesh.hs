{-# LANGUAGE RankNTypes #-}

-- A module used to create and render objects in OpenGL
module Client.App.Resources.Mesh
  ( Mesh (..)
  , createMesh
  ) where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.GL.Shaders.Uniform as GL
import Graphics.Rendering.OpenGL (($=))
import Data.Word (Word32)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (Ptr, plusPtr, nullPtr)
import Foreign.Storable (sizeOf)
import Linear.Affine
import Linear.V3

import Client.App.Resources.Shader
import Client.Rendering.Renderable

-- Describe a set of vertices that can be rendered
data Mesh = Mesh
  { meshVAO         :: GL.VertexArrayObject 
  , meshVBO         :: GL.BufferObject
  , meshEBO         :: GL.BufferObject
  , meshFirstIndex  :: GL.ArrayIndex 
  , meshNumIndices  :: GL.NumArrayIndices
  , meshUniforms    :: [Uniform]
  }

-- Allow meshes to be rendered
instance Renderable Mesh where
  render = renderMesh

--------------------------------------------------------------------------------

-- Create a mesh with vertices, indices, uniforms and a shader
createMesh :: [Point V3 Float] -> [Word32] -> [Uniform] -> IO Mesh
createMesh vertices indices uniforms = do

  -- Generate and bind VAO
  vao <- GL.genObjectName
  GL.bindVertexArrayObject $= Just vao

  -- Generate and bind VBO
  vbo <- GL.genObjectName
  GL.bindBuffer GL.ArrayBuffer $= Just vbo

  -- Generate and bind EBO
  ebo <- GL.genObjectName
  GL.bindBuffer GL.ElementArrayBuffer $= Just ebo

  -- Load vertex data into buffer
  let vSize = fromIntegral (length vertices * sizeOf (head vertices))
  withArray vertices $ \ptr ->
    GL.bufferData GL.ArrayBuffer $= (vSize, ptr, GL.StaticDraw)

  -- Load index data into buffer
  let iSize = fromIntegral (length indices * sizeOf (head indices))
  withArray indices $ \ptr ->
    GL.bufferData GL.ElementArrayBuffer $= (iSize, ptr, GL.StaticDraw)

  -- Specify and enable location attribute
  let firstIndex = 0
      vPosition = GL.AttribLocation 0
  GL.vertexAttribPointer vPosition $=
    (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float 0 
      (bufferOffset firstIndex))
  GL.vertexAttribArray vPosition $= GL.Enabled

  -- Create and return a mesh
  pure $ Mesh
    { meshVAO = vao
    , meshVBO = vbo
    , meshEBO = ebo
    , meshFirstIndex = 0
    , meshNumIndices = fromIntegral $ length indices
    , meshUniforms = uniforms
    }

-- Render the mesh
renderMesh :: Mesh -> Shader -> [Uniform] -> IO ()
renderMesh mesh shader uniforms = do

  -- Retrieve data from mesh
  let count = meshNumIndices mesh
      offset = bufferOffset $ meshFirstIndex mesh

  -- Bind shader
  GL.currentProgram $= Just shader

  -- Bind VAO
  GL.bindVertexArrayObject $= Just (meshVAO mesh)

  -- Provide all uniform data to shaders
  applyUniforms shader $ uniforms ++ meshUniforms mesh

  -- Draw vertices as triangles
  GL.drawElements GL.Triangles count GL.UnsignedInt offset

  -- Unbind VAO
  GL.bindVertexArrayObject $= Nothing

  -- Unbind shader
  GL.currentProgram $= Nothing

-- Creates a pointer to data
bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral
