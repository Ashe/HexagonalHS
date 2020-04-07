-- A module for loading and storing assets used in game
module Client.App.Resources
  ( Resources
  , ResourceType (..)
  , loadResourcesFrom
  , getResource
  ) where

import Control.Concurrent.MVar
import Data.Foldable (foldlM)
import System.Directory
import System.FilePath
import Data.Map.Strict

import Client.App.Resources.Shader

-- Store resources by type and name
newtype Resources = Resources
  { resources :: Map (ResourceType, String) (Resource Shader)
  }

-- Resource types
data ResourceType = ShaderResource
  deriving (Eq, Ord, Read, Show)

-- Store a resource along with a way of loading it
data Resource a = Resource 
  { resource        :: Maybe a
  , resourceLoader  :: IO (Maybe a)
  }
  
-- Load all resources recusively given a directory
loadResourcesFrom :: FilePath -> IO (Maybe Resources)
loadResourcesFrom fp = do
  let path = addTrailingPathSeparator fp
  putStrLn $ "Loading resources from: " ++ path
  exists <- doesPathExist path
  if exists then Just <$> loadAt (Resources empty) path
  else pure Nothing

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
    ".shader.yaml" -> Just $ addTo ShaderResource temp
    _ -> Nothing
  where name = dropExtensions . takeBaseName $ fp
        addTo t f = r { resources = 
            insert (t, name) (Resource Nothing f) $ resources r}
        temp = putStrLn "LOAD FUNCTION!" >> pure Nothing

-- Attempts to get a shader without taking mvars, but loads if required
getResource :: MVar Resources -> ResourceType -> String -> IO (Maybe Shader)
getResource mR rType name = do
  let key = (rType, name)
  rs <- readMVar mR
  let maybeRes = Data.Map.Strict.lookup key $ resources rs
  case maybeRes of 
    (Just (Resource r _)) -> 
      case r of
        (Just res) -> pure $ Just res
        _ -> tryLoadResource mR key
    _ -> do
      putStrLn $ "[Error] Could not find " ++ show rType ++ ": '" ++ name ++ "'."
      pure Nothing

-- Get resource by name and load if necessary
tryLoadResource :: MVar Resources -> (ResourceType, String) -> IO (Maybe Shader)
tryLoadResource mR key@(t, name) = do

  -- Declare the resource to load
  putStrLn $ "Loading " ++ show t ++ ": '" ++ name ++ "'.."

  -- Retrieve resources from mvar
  rs <- takeMVar mR

  -- Get the element to load
  let res@(Resource _ l) = resources rs ! key

  -- Use the loading function to try and load the resource
  maybeRes <- l
  case maybeRes of

    -- If a resource was loaded, update collection
    (Just r) -> do
      let newRs = rs { resources = 
        insert key res { resource = maybeRes } $ resources rs }
      putMVar mR newRs
      pure maybeRes

    -- If nothing was done, do nothing
    _ -> do
      putStrLn $ "[Error] Failed to load " ++ show t ++ ": '" ++ name ++ "'."
      putMVar mR rs
      pure Nothing
