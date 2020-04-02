module Main where

import System.Environment (getArgs, getProgName)

import qualified Client (initialise)

-- Start the client
main :: IO ()
main = do

  -- Check arguments
  args <- getArgs

  -- Determine size of the window
  let defaultSize = (1920, 1080)
      winSize = case args of
        (x : y : xs) -> (read x, read y)
        _ -> defaultSize

  -- Start the client
  uncurry Client.initialise winSize
