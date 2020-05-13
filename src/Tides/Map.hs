-- A module defining how maps are represented and interacted with in Tides 
module Tides.Map
  ( Map (..)
  , Index
  , blankMap
  , randomMap
  ) where

import System.Random
import Math.Geometry.Grid hiding (Index)
import qualified Math.Geometry.Grid as G
import Math.Geometry.Grid.Hexagonal
import qualified Data.Map.Strict as D

-- The field of play
data Map = Map 
  { mapSize      :: Int
  , mapTiles     :: D.Map Index Int
  }

-- Locations of interest in the map
type Index = G.Index HexHexGrid

--------------------------------------------------------------------------------

-- Creates a blank, hexagonal map of a given size
blankMap :: Int -> Map
blankMap size = Map
  { mapSize = size
  , mapTiles = D.fromList (zip (indices grid) [0..])
  }
  where grid = hexHexGrid size

-- Create a random map of a given size
randomMap :: Int -> IO Map
randomMap size = do

  -- Start with the blank map
  let blank = blankMap size

  -- Randomise map
  tiles <- mapM (genRandomTile 8) $ mapTiles blank

  -- Return the new map
  pure $ blank { mapTiles = tiles }

-- Generate a tile with a random height
genRandomTile :: Int -> a -> IO Int
genRandomTile max _ = do
  land <- randomRIO (0, 2) :: IO Int
  if land == 0 then pure 0
  else randomRIO (1, max)

