-- A module defining the scene used when playing the game
module Client.GameScene
  ( createGameScene
  ) where

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Control.Monad (when, unless, void)
import Control.Monad.RWS.Strict
import Control.Concurrent.MVar
import Data.Maybe (catMaybes)
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

-- Shader includes
import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
-- End Shader includes

import Client.App

-- The game to play
newtype GameScene = GameScene (MVar Descriptor)

-- Define how this scene is interacted with
instance Scene GameScene where
  handleEvent = onHandleEvent
  update      = onUpdate
  render      = onRender

-- Describe an object to be rendered
data Descriptor = Descriptor GL.VertexArrayObject 
  GL.ArrayIndex GL.NumArrayIndices

-- Easily create a blank gamescene
createGameScene :: App GameScene
createGameScene = do

  -- Generate and bind VAO
  vao <- liftIO GL.genObjectName
  GL.bindVertexArrayObject $= Just vao

  -- Define triangles
  let vertices :: [GL.Vertex2 GL.GLfloat]
      vertices = [
        GL.Vertex2 (-0.90) (-0.90),  -- Triangle 1
        GL.Vertex2   0.85  (-0.90),
        GL.Vertex2 (-0.90)   0.85 ,
        GL.Vertex2   0.90  (-0.85),  -- Triangle 2
        GL.Vertex2   0.90    0.90 ,
        GL.Vertex2 (-0.85)   0.90 ]
      numVertices = length vertices

  -- Generate and bind VBO
  vbo <- liftIO GL.genObjectName
  GL.bindBuffer GL.ArrayBuffer $= Just vbo

  -- Load vertex data into buffer
  liftIO $ withArray vertices $ \ptr -> do
    let size = fromIntegral (numVertices * sizeOf (head vertices))
    GL.bufferData GL.ArrayBuffer $= (size, ptr, GL.StaticDraw)

  -- Compile shaders
  program <- liftIO $ loadShaders [
     ShaderInfo GL.VertexShader (FileSource "assets/shaders/shader.vert"),
     ShaderInfo GL.FragmentShader (FileSource "assets/shaders/shader.frag")]
  GL.currentProgram GL.$= Just program

  -- Specify and enable location attribute
  let firstIndex = 0
      vPosition = GL.AttribLocation 0
  GL.vertexAttribPointer vPosition $=
    (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 0 
      (bufferOffset firstIndex))
  GL.vertexAttribArray vPosition $= GL.Enabled

  -- Store information about how to render the vertices
  descriptor <- liftIO newEmptyMVar
  liftIO $ putMVar descriptor $ 
    Descriptor vao firstIndex (fromIntegral numVertices)

  -- Create a GameScene with this information
  pure $ GameScene descriptor

--------------------------------------------------------------------------------

-- Process inputs
onHandleEvent :: GameScene -> Event -> App ()
onHandleEvent scene EventMouseButton {} = liftIO $ putStrLn "Click!"
onHandleEvent _ _ = pure ()

-- Update entities in the scene
onUpdate :: GameScene -> Double -> App ()
onUpdate scene dt = pure ()

-- Display the scene
onRender :: GameScene -> App ()
onRender (GameScene mvar) = liftIO $ do
  (Descriptor vao index count) <- readMVar mvar
  GL.bindVertexArrayObject $= Just vao
  GL.drawArrays GL.Triangles index count
  GL.bindVertexArrayObject $= Nothing

--------------------------------------------------------------------------------

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

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

--------------------------------------------------------------------------------

-- | A description of a shader: The type of the shader plus its source code.

data ShaderInfo = ShaderInfo GL.ShaderType ShaderSource
   deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------

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
      GL.shaderSourceBS shader $= src
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
