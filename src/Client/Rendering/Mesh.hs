-- Module detailing how to render a mesh
module Client.Rendering.Mesh
  ( render
  ) where

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Control.Monad.RWS.Strict (liftIO)
import Foreign.Ptr (Ptr, plusPtr, nullPtr)
import Foreign.Storable (sizeOf)

import Client.App (App)
import Client.App.Resources.Shader
import Client.App.Resources.Mesh
import Client.Rendering.Renderable

-- Allow meshes to be rendered
instance Renderable Mesh where
  render = renderMesh

-- Render the mesh
renderMesh :: Mesh -> Shader -> [Uniform] -> App ()
renderMesh mesh shader uniforms = do

  -- Retrieve data from mesh
  let count = meshNumIndices mesh
      offset = bufferOffset $ meshFirstIndex mesh

  -- Bind VAO
  GL.bindVertexArrayObject $= Just (meshVAO mesh)

  -- Provide all uniform data to shaders
  applyUniforms shader $ uniforms ++ meshUniforms mesh

  -- Draw vertices as triangles
  liftIO $ GL.drawElements GL.Triangles count GL.UnsignedInt offset

-- Creates a pointer to data
-- @NOTE: DUPLICATED IN Resources/Mesh.hs
bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral
