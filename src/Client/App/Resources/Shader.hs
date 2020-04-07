{-# LANGUAGE OverloadedStrings #-}

-- Module enabling the loading and usage of shaders
module Client.App.Resources.Shader
  ( Shader (..)
  ) where

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Data.ByteString as B
import Control.Exception
import Control.Monad
import Data.Yaml as Y

-- Find shader files that will be used by the program
data Description = Description
  { vertexPath    :: Maybe FilePath
  , fragmentPath  :: Maybe FilePath
  }

-- Allow shader programs to be read from files
instance FromJSON Description where
  parseJSON (Object v) = 
    Description <$> v .: "vertex"
                <*> v .: "fragment"
  parseJSON _ = mempty

-- Store the created program
data Shader = Shader
  { shaderDescription :: Description
  , shaderProgram     :: GL.Program 
  }

--------------------------------------------------------------------------------

data ShaderSource =
     ByteStringSource B.ByteString
     -- ^ The shader source code is directly given as a 'B.ByteString'.
   | StringSource String
     -- ^ The shader source code is directly given as a 'String'.
   | FileSource FilePath
     -- ^ The shader source code is located in the file at the given 'FilePath'.
   deriving ( Eq, Ord, Show )

getSource :: ShaderSource -> IO B.ByteString
getSource (ByteStringSource bs) = return bs
getSource (StringSource str) = return $ GL.packUtf8 str
getSource (FileSource path) = B.readFile path


-- | A description of a shader: The type of the shader plus its source code.

data ShaderInfo = ShaderInfo GL.ShaderType ShaderSource
   deriving ( Eq, Ord, Show )

-- | Create a new program object from the given shaders, throwing an
-- 'IOException' if something goes wrong.

loadShaders :: [ShaderInfo] -> IO GL.Program
loadShaders infos =
   GL.createProgram `bracketOnError` GL.deleteObjectName $ \program -> do
      loadCompileAttach program infos
      linkAndCheck program
      return program

linkAndCheck :: GL.Program -> IO ()
linkAndCheck = checked GL.linkProgram GL.linkStatus GL.programInfoLog "link"

loadCompileAttach :: GL.Program -> [ShaderInfo] -> IO ()
loadCompileAttach _ [] = return ()
loadCompileAttach program (ShaderInfo shType source : infos) =
   GL.createShader shType `bracketOnError` GL.deleteObjectName $ \shader -> do
      src <- getSource source
      GL.shaderSourceBS shader GL.$= src
      compileAndCheck shader
      GL.attachShader program shader
      loadCompileAttach program infos

compileAndCheck :: GL.Shader -> IO ()
compileAndCheck = checked 
  GL.compileShader GL.compileStatus GL.shaderInfoLog "compile"

checked :: (t -> IO ())
        -> (t -> GL.GettableStateVar Bool)
        -> (t -> GL.GettableStateVar String)
        -> String
        -> t
        -> IO ()
checked action getStatus getInfoLog message object = do
   action object
   ok <- GL.get (getStatus object)
   unless ok $ do
      infoLog <- GL.get (getInfoLog object)
      fail (message ++ " log: " ++ infoLog)
