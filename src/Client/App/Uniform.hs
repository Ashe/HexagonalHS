{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

-- A module used to create and render objects in OpenGL
module Client.App.Uniform
  ( Uniform (..)
  , applyUniforms
  ) where

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))

-- Easy representation of uniform data
data Uniform = forall a. GL.Uniform a => Uniform 
  { uniformName :: String
  , uniformData :: a
  }

-- Provide a list of uniforms to the supplied shader
applyUniforms :: GL.Program -> [Uniform] -> IO ()
applyUniforms program uniforms = mapM_ f uniforms
  where f (Uniform n d) = do
          location <- GL.uniformLocation program n
          GL.uniform location $= d
