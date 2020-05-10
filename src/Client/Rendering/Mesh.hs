{-# LANGUAGE RankNTypes #-}

-- A module used to create and render objects in OpenGL
module Client.Rendering.Mesh
  ( Mesh (..)
  , createMesh
  , renderMesh
  ) where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.GL.Shaders.Uniform as GL
import Graphics.Rendering.OpenGL (($=))
import Control.Monad.RWS.Strict
import Data.Map.Strict (elems)
import Linear.V3
import Linear.Affine
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

import Client.App
import Client.App.Uniform

-- Describe an object to be rendered
data Mesh = Mesh
  { meshVAO         :: GL.VertexArrayObject 
  , meshVBO         :: GL.BufferObject
  , meshShader      :: GL.Program
  , meshFirstIndex  :: GL.ArrayIndex 
  , meshNumVertices :: GL.NumArrayIndices
  , meshUniforms    :: [Uniform]
  }

--------------------------------------------------------------------------------

-- Easily create a mesh
createMesh :: [Point V3 Float] -> GL.Program -> [Uniform] -> App Mesh
createMesh vertices program uniforms = do

  -- Generate and bind VAO
  vao <- liftIO GL.genObjectName
  GL.bindVertexArrayObject $= Just vao

  -- Generate and bind VBO
  vbo <- liftIO GL.genObjectName
  GL.bindBuffer GL.ArrayBuffer $= Just vbo

  -- Load vertex data into buffer
  liftIO $ withArray vertices $ \ptr -> do
    let size = fromIntegral (length vertices * sizeOf (head vertices))
    GL.bufferData GL.ArrayBuffer $= (size, ptr, GL.StaticDraw)

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
    , meshShader = program
    , meshFirstIndex = 0
    , meshNumVertices = fromIntegral $ length vertices
    , meshUniforms = uniforms
    }

-- Render the mesh
renderMesh :: Mesh -> [Uniform] -> App ()
renderMesh mesh uniforms = do

  -- Retrieve the state
  state <- get

  -- Retrieve data from mesh and state
  let program = meshShader mesh
      count = meshNumVertices mesh
      firstIndex = meshFirstIndex mesh
      meshUnis = meshUniforms mesh
      globalUnis = elems $ stateGlobalUniforms state

  -- Bind shader to use
  let program = meshShader mesh
  GL.currentProgram $= Just program

  -- Bind VAO
  GL.bindVertexArrayObject $= Just (meshVAO mesh)

  -- Provide all uniform data to shaders
  applyUniforms program $ globalUnis ++ meshUnis ++ uniforms

  -- Draw vertices as triangles
  liftIO $ GL.drawArrays GL.Triangles firstIndex count

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