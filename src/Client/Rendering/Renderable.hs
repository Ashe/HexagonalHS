-- Module definining a typeclass that allows things to be rendered
module Client.Rendering.Renderable
  ( Renderable (..)
  , Uniform (..)
  , applyUniforms
  ) where

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Control.Monad.RWS.Strict (liftIO)

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
  where f (Uniform n d) = do
          location <- GL.uniformLocation shader n
          GL.uniform location $= d
