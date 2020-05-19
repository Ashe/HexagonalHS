{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

-- A module for loading and storing assets used in game
module Client.App.Resources
  ( Resources
  , loadResourcesFrom
  , Mesh
  , Shader
  , getShader
  , getMesh
  , tryGetShader
  , tryGetMesh
  ) where

import Control.Lens
import Control.Concurrent.MVar
import Data.Proxy
import Data.Foldable (foldlM)
import System.Directory
import System.FilePath
import Data.Map.Strict

import Client.App.Resources.Mesh
import Client.App.Resources.Shader

import Client.Rendering.Geometry.Hexagon

-- Simplified types
type RInfo = (String, String)
type RMap a = Map String (Resource a)
type RLens a = Lens' Resources (RMap a) 

-- Store a resource along with a way of loading it
data Resource a = Resource 
  { resource        :: Maybe a
  , resourceLoader  :: IO (Maybe a)
  }

-- Store resources by type and name
data Resources = Resources
  { _shaders  :: RMap Shader
  , _meshes   :: RMap Mesh
  }

-- Create lenses for each of Resources' records
makeLenses ''Resources

--------------------------------------------------------------------------------
  
-- Load all resources recusively given a directory
loadResourcesFrom :: FilePath -> IO Resources
loadResourcesFrom fp = do
  let path = addTrailingPathSeparator fp
  putStrLn $ "Loading resources from: " ++ path
  exists <- doesPathExist path
  defaultResources <- generateResources
  if exists then do
    loadAt defaultResources path
  else do
    putStrLn "[Warning] Directory doesn't exist."
    pure defaultResources

-- Load resources recursively
loadAt :: Resources -> FilePath -> IO Resources
loadAt r fp = do
  isDir <- doesDirectoryExist fp
  isFile <- doesFileExist fp
  if isDir then do
    files <- listDirectory fp
    foldlM loadAt r $ (fp </>) <$> files
  else if isFile then
    case initResource r fp of
      (Just resources) -> do
        putStrLn $ "Found resource: " ++ fp
        pure resources
      _ -> pure r
  else
    pure r

-- Initialise different resources depending on extensions
initResource :: Resources -> FilePath -> Maybe Resources
initResource r fp = 
  case takeExtensions fp of
    ".shader.yaml" -> Just $ over shaders (init $ loadShader fp) r
    _ -> Nothing
  where name = dropExtensions . takeBaseName $ fp
        init f = insert name $ makeResource f

--------------------------------------------------------------------------------

-- Generates a blank set of resources
generateResources :: IO Resources
generateResources = do

  -- Hexagonal prism mesh
  let (vertices, indices) = hexagonalPrism 0.25
      makeHex = Just <$> createMesh vertices indices []
  
  -- Return loaders for generated resources
  pure $ Resources
    { _shaders = empty
    , _meshes = fromList [ ("hexagonal_prism", makeResource makeHex) ]
    }

--------------------------------------------------------------------------------

-- Get a resource, or crash the program with an error message

getShader :: MVar Resources -> String -> IO Shader
getShader mR name = do 
  resource <- tryGetShader mR name
  case resource of
    Just shader -> pure shader
    _ -> error $ "[Error] could not get shader: '" ++ name ++ "'."

getMesh :: MVar Resources -> String -> IO Mesh
getMesh mR name = do
  resource <- tryGetMesh mR name
  case resource of
    Just mesh -> pure mesh
    _ -> error $ "[Error] could not get mesh: '" ++ name ++ "'."

--------------------------------------------------------------------------------

-- Attempts to get a resource without taking mvars, but loads if required

tryGetShader :: MVar Resources -> String -> IO (Maybe Shader)
tryGetShader mR name = tryGet mR (info "shader") shaders
  where info t = (t, name)

tryGetMesh :: MVar Resources -> String -> IO (Maybe Mesh)
tryGetMesh mR name = tryGet mR (info "mesh") meshes
  where info t = (t, name)

--------------------------------------------------------------------------------

-- Attempts to retrieve a loaded resource
tryGet :: MVar Resources -> RInfo -> RLens a -> IO (Maybe a)
tryGet mR info@(t, name) lens = do
  rs <- readMVar mR
  let maybeRes = Data.Map.Strict.lookup name $ view lens rs
  case maybeRes of 
    (Just (Resource r _)) -> 
      case r of
        (Just res) -> pure $ Just res
        _ -> tryLoad mR info lens
    _ -> do
      putStrLn $ 
        "[Error] Could not find " ++ t ++ ": '" ++ name ++ "'."
      pure Nothing

-- Try and load an unloaded resource
tryLoad :: MVar Resources -> RInfo -> RLens a -> IO (Maybe a)
tryLoad mR info@(t, name) lens = do

  -- Declare the resource to load
  putStrLn $ "Loading " ++ t ++ ": '" ++ name ++ "'.."

  -- Retrieve resources from mvar
  rs <- takeMVar mR

  -- Get the element to load and the accessor to place it
  let res@(Resource _ l) = view lens rs ! name

  -- Use the loading function to try and load the resource
  maybeRes <- l
  case maybeRes of

    -- If a resource was loaded, update collection
    (Just r) -> do
      let newRs = over lens (insert name res { resource = maybeRes }) rs
      putMVar mR newRs
      pure maybeRes

    -- If nothing was done, do nothing
    _ -> do
      putStrLn $ "[Error] Failed to load " ++ t ++ ": '" ++ name ++ "'."
      putMVar mR rs
      pure Nothing

--------------------------------------------------------------------------------

-- Easy way of creating a resource
makeResource :: IO (Maybe a) -> Resource a
makeResource = Resource Nothing
