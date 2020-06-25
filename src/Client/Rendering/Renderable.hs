-- Module definining a typeclass that allows things to be rendered
module Client.Rendering.Renderable
  ( Renderable (..)
  , Uniform (..)
  , applyUniforms
  ) where

import Prelude hiding (length)
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Control.Monad.RWS.Strict (liftIO)
import Data.Vector.Storable (Vector, unsafeWith, length)
import Foreign.Ptr

import Client.App
import Client.App.Uniform
import Client.App.Resources.Shader

-- Renderables can be rendered to the screen
class Renderable r where
  render      :: r -> Shader -> [Uniform] -> App ()

-- Provide a list of uniforms to the supplied shader for rendering
applyUniforms :: Shader -> [Uniform] -> App ()
applyUniforms shader uniforms = do
  GL.currentProgram $= Just shader
  liftIO $ mapM_ f uniforms
  where f (Uniform name d) = do
            location <- GL.uniformLocation shader name
            case d of
              UniformData u -> GL.uniform location $= u
              UniformDataMulti v -> unsafeWith v $
                  GL.uniformv location (fromIntegral $ length v)
