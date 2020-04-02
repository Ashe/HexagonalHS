module Main where

import qualified Client (initialise)

-- Start the client
main :: IO ()
main = Client.initialise
