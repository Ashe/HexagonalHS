-- A module defining the scene used when playing the game
module Client.GameScene
  ( GameScene (..)
  ) where

import qualified Graphics.UI.GLFW as GLFW
import Control.Monad (when, unless, void)
import Control.Monad.RWS.Strict (RWST, evalRWST, liftIO, get, put, asks, modify)
import Data.Maybe (catMaybes)

import Client.App

-- The game to play
data GameScene = GameScene

-- Define how this scene is interacted with
instance Scene GameScene where

  -- Initialise scene
  begin scene = liftIO $ putStrLn "Beginning GameScene"

  -- Process inputs
  handleEvent scene ev =
    case ev of

      -- Report errors and close the window
      (EventError e s) -> do
        printEvent "Error" [show e, show s]
        win <- asks envWindow
        liftIO $ GLFW.setWindowShouldClose win True

      -- Perform final actions before program terminates
      (EventWindowClose _) -> printEvent "Close window" []

      -- Resize the window
      (EventFramebufferSize _ width height) -> do
        printEvent "Resize frame" [show width, show height]
        modify $ \s -> s
          { stateWindowWidth  = width
          , stateWindowHeight = height
          }
        resize scene

      -- Handle mouse clicks
      (EventMouseButton _ mb mbs mk) -> do
        printEvent "Mouse press" [show mb, show mbs, showModifierKeys mk]
        when (mb == GLFW.MouseButton'1) $ do
          let pressed = mbs == GLFW.MouseButtonState'Pressed
          modify $ \s -> s
            { stateMouseDown = pressed
            }
          unless pressed $
            modify $ \s -> s
              { stateDragging = False
              }

      -- Handle mouse movement
      (EventCursorPos _ x y) -> do
          let x' = round x :: Int
              y' = round y :: Int
          state <- get
          when (stateMouseDown state && not (stateDragging state)) $
            put $ state
              { stateDragging        = True
              , stateDragStartX      = x
              , stateDragStartY      = y
              }

      -- Handle keypresses
      (EventKey win k scancode ks mk) -> do
        printEvent "Key press" [show k, show scancode, show ks, showModifierKeys mk]
        when (ks == GLFW.KeyState'Pressed) $
          -- Q, Esc: exit
          when (k == GLFW.Key'Q || k == GLFW.Key'Escape) $
            liftIO $ GLFW.setWindowShouldClose win True

      -- When a character is entered
      (EventChar _ c) -> pure ()

      -- Otherwise do nothing
      _ -> pure ()

  -- Update entities in the scene
  update scene dt = pure ()

  -- Display the scene
  render scene = pure ()

  -- Resize the scene
  resize scene =
    liftIO $ putStrLn "resize GameScene not yet implemented"

--------------------------------------------------------------------------------

-- Outputs the event to terminal
printEvent :: String -> [String] -> App ()
printEvent cbname fields =
    liftIO $ putStrLn $ cbname ++ ": " ++ unwords fields

-- Outputs modifer keys for debugging
showModifierKeys :: GLFW.ModifierKeys -> String
showModifierKeys mk =
    "[mod keys: " ++ keys ++ "]"
  where
    keys = if null xs then "none" else unwords xs
    xs = catMaybes ys
    ys = [ if GLFW.modifierKeysShift   mk then Just "shift"   else Nothing
         , if GLFW.modifierKeysControl mk then Just "control" else Nothing
         , if GLFW.modifierKeysAlt     mk then Just "alt"     else Nothing
         , if GLFW.modifierKeysSuper   mk then Just "super"   else Nothing
         ]
