{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

-- A module for loading and storing assets used in game
module Client.App.Resources
  ( Resources
  , loadResourcesFrom
  , getShader
  ) where

import Control.Lens
import Control.Concurrent.MVar
import Data.Proxy
import Data.Foldable (foldlM)
import System.Directory
import System.FilePath
import Data.Map.Strict

import Client.App.Resources.Shader

-- Store a resource along with a way of loading it
data Resource a = Resource 
  { resource        :: Maybe a
  , resourceLoader  :: IO (Maybe a)
  }

-- Collection of resources and method of access
type RMap a = Map String (Resource a)
type RKey = (String, String)

-- Store resources by type and name
newtype Resources = Resources
  { _shaders :: RMap Shader
  }
makeLenses ''Resources
  
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
    ".shader.yaml" -> Just $ over shaders (addNull temp) r
    _ -> Nothing
  where name = dropExtensions . takeBaseName $ fp
        addNull f = insert name $ Resource Nothing f
        temp = putStrLn "LOAD FUNCTION!" >> pure Nothing

-- Attempts to get a shader without taking mvars, but loads if required
getShader :: MVar Resources -> String -> IO (Maybe Shader)
getShader mR name = tryGet mR (key "Shader") shaders
  where key t = (t, name)

-- Attempts to retrieve a loaded resource
tryGet :: MVar Resources -> RKey -> Lens' Resources (RMap a) -> IO (Maybe a)
tryGet mR key@(t, name) lens = do
  rs <- readMVar mR
  let maybeRes = Data.Map.Strict.lookup name $ view lens rs
  case maybeRes of 
    (Just (Resource r _)) -> 
      case r of
        (Just res) -> pure $ Just res
        _ -> tryLoad mR key lens
    _ -> do
      putStrLn $ 
        "[Error] Could not find " ++ t ++ ": '" ++ name ++ "'."
      pure Nothing

-- Try and load an unloaded resource
tryLoad :: MVar Resources -> RKey -> Lens' Resources (RMap a) -> IO (Maybe a)
tryLoad mR key@(t, name) lens = do

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
