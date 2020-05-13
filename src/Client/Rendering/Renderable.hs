-- Module definining a typeclass that allows things to be rendered
module Client.Rendering.Renderable
  ( Renderable (..)
  , Uniform (..)
  , applyUniforms
  ) where

import Client.App.Uniform
import Client.App.Resources.Shader

-- Renderables can be rendered to the screen
class Renderable r where
  render      :: r -> Shader -> [Uniform] -> IO ()
