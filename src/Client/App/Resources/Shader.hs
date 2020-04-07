{-# LANGUAGE OverloadedStrings #-}

-- Module enabling the loading and usage of shaders
module Client.App.Resources.Shader
  ( Shader (..)
  , loadShader
  ) where

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Data.ByteString as B
import Control.Exception
import Control.Monad
import qualified Data.Yaml as Y
import Data.Yaml ((.:))
import Data.Maybe (catMaybes)
import System.FilePath

-- Find shader files that will be used by the program
data Description = Description
  { vertexPath    :: Maybe FilePath
  , fragmentPath  :: Maybe FilePath
  }

-- Allow shader programs to be read from files
instance Y.FromJSON Description where
  parseJSON (Y.Object v) = 
    Description <$> v .: "vertex"
                <*> v .: "fragment"
  parseJSON _ = mempty

-- Store the created program
data Shader = Shader
  { shaderDescription :: Description
  , shaderProgram     :: GL.Program 
  }

-- Store a to-be-compiled shader
data ShaderInfo = ShaderInfo GL.ShaderType FilePath

--------------------------------------------------------------------------------

-- Hide yaml's decode function for our own
decode :: FilePath -> IO (Either Y.ParseException Description)
decode = Y.decodeFileEither

-- Load and compile a shader program at a given filepath
loadShader :: FilePath -> IO (Maybe Shader)
loadShader fp = do
  attempt <- decode fp
  case attempt of

    -- If parsing succeeded, prepare a list of shaders to compile
    Right desc@(Description v f) -> 
      let pathTo = replaceFileName fp
          shaders = 
              [ ShaderInfo GL.VertexShader    . pathTo <$> v
              , ShaderInfo GL.FragmentShader  . pathTo <$> f
              ] in

            -- Attempt to create a program, crash on fail with error
            GL.createProgram `bracketOnError` GL.deleteObjectName $ 
              \program -> do
                 loadCompileAttach program $ catMaybes shaders
                 linkAndCheck program
                 pure $ Just $ Shader desc program

    -- If parsing failed, report the error and do nothing
    Left exc -> do
      putStrLn $ Y.prettyPrintParseException exc
      pure Nothing

--------------------------------------------------------------------------------

-- Link the program and check for errors
linkAndCheck :: GL.Program -> IO ()
linkAndCheck = checked GL.linkProgram GL.linkStatus GL.programInfoLog "link"

-- Load a list of shaders and compile them
loadCompileAttach :: GL.Program -> [ShaderInfo] -> IO ()
loadCompileAttach _ [] = pure ()
loadCompileAttach program ((ShaderInfo t fp) : infos) =
   GL.createShader t `bracketOnError` GL.deleteObjectName $ \shader -> do
      src <- B.readFile fp
      GL.shaderSourceBS shader $= src
      compileAndCheck shader
      GL.attachShader program shader
      loadCompileAttach program infos

-- Compile a shader and check for errors
compileAndCheck :: GL.Shader -> IO ()
compileAndCheck = checked 
  GL.compileShader GL.compileStatus GL.shaderInfoLog "compile"

-- Generic error checker for shader processing
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
