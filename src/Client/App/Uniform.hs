{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

-- A module used to create and render objects in OpenGL
module Client.App.Uniform
  ( Uniform (..) 
  ) where

import qualified Graphics.Rendering.OpenGL as GL

import Client.App.Resources.Shader

-- Easy representation of uniform data
data Uniform = forall a. GL.Uniform a => Uniform 
  { uniformName :: String
  , uniformData :: a
  }
