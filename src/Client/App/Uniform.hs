{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

-- A module used to create and render objects in OpenGL
module Client.App.Uniform
  ( Uniform (..) 
  , UniformData (..)
  ) where

import qualified Graphics.Rendering.OpenGL as GL
import Foreign.Storable (Storable)
import Data.Vector.Storable (Vector)

-- Easy representation of uniform data
data Uniform = Uniform
  { uniformName   :: String
  , uniformData   :: UniformData
  }

-- Easy representation of uniform data
data UniformData where
  UniformData :: GL.Uniform a => a -> UniformData
  UniformDataMulti :: (GL.Uniform a, Storable a) => Vector a -> UniformData
