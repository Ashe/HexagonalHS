{-# LANGUAGE RankNTypes #-}

-- A module used to create and render objects in OpenGL
module Client.Rendering.Mesh
  ( Mesh (..)
  , createMesh
  ) where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.GL.Shaders.Uniform as GL
import Graphics.Rendering.OpenGL (($=))
import Control.Monad.RWS.Strict (liftIO, get)
import Data.Word (Word32)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (Ptr, plusPtr, nullPtr)
import Foreign.Storable (sizeOf)
import Linear.Affine
import Linear.V3

import Client.App

-- Describe a set of vertices that can be rendered
data Mesh = Mesh
  { meshVAO         :: GL.VertexArrayObject 
  , meshVBO         :: GL.BufferObject
  , meshEBO         :: GL.BufferObject
  , meshShader      :: GL.Program
  , meshFirstIndex  :: GL.ArrayIndex 
  , meshNumIndices  :: GL.NumArrayIndices
  , meshUniforms    :: [Uniform]
  }

-- Allow meshes to be rendered
instance Renderable Mesh where
  render = renderMesh

--------------------------------------------------------------------------------

-- Create a mesh with vertices, indices, uniforms and a shader
createMesh :: [Point V3 Float] -> [Word32] -> GL.Program -> [Uniform] -> App Mesh
createMesh vertices indices program uniforms = do

  -- Generate and bind VAO
  vao <- liftIO GL.genObjectName
  GL.bindVertexArrayObject $= Just vao

  -- Generate and bind VBO
  vbo <- liftIO GL.genObjectName
  GL.bindBuffer GL.ArrayBuffer $= Just vbo

  -- Generate and bind EBO
  ebo <- liftIO GL.genObjectName
  GL.bindBuffer GL.ElementArrayBuffer $= Just ebo

  -- Load vertex data into buffer
  let vSize = fromIntegral (length vertices * sizeOf (head vertices))
  liftIO $ withArray vertices $ \ptr ->
    GL.bufferData GL.ArrayBuffer $= (vSize, ptr, GL.StaticDraw)

  -- Load index data into buffer
  let iSize = fromIntegral (length indices * sizeOf (head indices))
  liftIO $ withArray indices $ \ptr ->
    GL.bufferData GL.ElementArrayBuffer $= (iSize, ptr, GL.StaticDraw)

  -- Specify what shader program to use
  GL.currentProgram GL.$= Just program

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
    , meshShader = program
    , meshFirstIndex = 0
    , meshNumIndices = fromIntegral $ length indices
    , meshUniforms = uniforms
    }

-- Render the mesh
renderMesh :: Mesh -> [Uniform] -> App ()
renderMesh mesh uniforms = do

  -- Retrieve data from mesh
  let program = meshShader mesh
      count = meshNumIndices mesh
      offset = bufferOffset $ meshFirstIndex mesh

  -- Bind shader
  GL.currentProgram $= Just program

  -- Bind VAO
  GL.bindVertexArrayObject $= Just (meshVAO mesh)

  -- Provide all uniform data to shaders
  applyUniforms program $ uniforms ++ meshUniforms mesh

  -- Draw vertices as triangles
  liftIO $ GL.drawElements GL.Triangles count GL.UnsignedInt offset

  -- Unbind VAO
  GL.bindVertexArrayObject $= Nothing

  -- Unbind shader
  GL.currentProgram $= Nothing

-- Provide a list of uniforms to the shader
applyUniforms :: GL.Program -> [Uniform] -> App ()
applyUniforms program uniforms = liftIO $ mapM_ f uniforms
  where f (Uniform n d) = do
          location <- GL.uniformLocation program n
          GL.uniform location $= d

-- Creates a pointer to data
bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral
