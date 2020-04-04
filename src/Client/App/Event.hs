module Client.App.Event
  ( Event (..)
  , provideCallbacks
  ) where

import qualified Graphics.UI.GLFW as GLFW
import Control.Concurrent.STM (TQueue, atomically, writeTQueue)

-- Types of event
data Event =
    EventError           !GLFW.Error !String
  | EventWindowPos       !GLFW.Window !Int !Int
  | EventWindowSize      !GLFW.Window !Int !Int
  | EventWindowClose     !GLFW.Window
  | EventWindowRefresh   !GLFW.Window
  | EventWindowFocus     !GLFW.Window !Bool
  | EventWindowIconify   !GLFW.Window !Bool
  | EventFramebufferSize !GLFW.Window !Int !Int
  | EventMouseButton     !GLFW.Window !GLFW.MouseButton !GLFW.MouseButtonState !GLFW.ModifierKeys
  | EventCursorPos       !GLFW.Window !Double !Double
  | EventCursorEnter     !GLFW.Window !GLFW.CursorState
  | EventScroll          !GLFW.Window !Double !Double
  | EventKey             !GLFW.Window !GLFW.Key !Int !GLFW.KeyState !GLFW.ModifierKeys
  | EventChar            !GLFW.Window !Char
  deriving Show

--------------------------------------------------------------------------------

-- Helper function for adding to queue
toQueue :: TQueue Event -> Event -> IO ()
toQueue tc e = atomically $ writeTQueue tc e

-- Write an appropriate Event to the events to TQueue
errorCallback           :: TQueue Event -> GLFW.Error -> String                                                           -> IO ()
windowPosCallback       :: TQueue Event -> GLFW.Window -> Int -> Int                                                      -> IO ()
windowSizeCallback      :: TQueue Event -> GLFW.Window -> Int -> Int                                                      -> IO ()
windowCloseCallback     :: TQueue Event -> GLFW.Window                                                                    -> IO ()
windowRefreshCallback   :: TQueue Event -> GLFW.Window                                                                    -> IO ()
windowFocusCallback     :: TQueue Event -> GLFW.Window -> Bool                                                            -> IO ()
windowIconifyCallback   :: TQueue Event -> GLFW.Window -> Bool                                                            -> IO ()
framebufferSizeCallback :: TQueue Event -> GLFW.Window -> Int -> Int                                                      -> IO ()
mouseButtonCallback     :: TQueue Event -> GLFW.Window -> GLFW.MouseButton -> GLFW.MouseButtonState -> GLFW.ModifierKeys  -> IO ()
cursorPosCallback       :: TQueue Event -> GLFW.Window -> Double -> Double                                                -> IO ()
cursorEnterCallback     :: TQueue Event -> GLFW.Window -> GLFW.CursorState                                                -> IO ()
scrollCallback          :: TQueue Event -> GLFW.Window -> Double -> Double                                                -> IO ()
keyCallback             :: TQueue Event -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys           -> IO ()
charCallback            :: TQueue Event -> GLFW.Window -> Char                                                            -> IO ()

errorCallback           tc e s            = toQueue tc $ EventError           e s
windowPosCallback       tc win x y        = toQueue tc $ EventWindowPos       win x y
windowSizeCallback      tc win w h        = toQueue tc $ EventWindowSize      win w h
windowCloseCallback     tc win            = toQueue tc $ EventWindowClose     win
windowRefreshCallback   tc win            = toQueue tc $ EventWindowRefresh   win
windowFocusCallback     tc win fa         = toQueue tc $ EventWindowFocus     win fa
windowIconifyCallback   tc win ia         = toQueue tc $ EventWindowIconify   win ia
framebufferSizeCallback tc win w h        = toQueue tc $ EventFramebufferSize win w h
mouseButtonCallback     tc win mb mba mk  = toQueue tc $ EventMouseButton     win mb mba mk
cursorPosCallback       tc win x y        = toQueue tc $ EventCursorPos       win x y
cursorEnterCallback     tc win ca         = toQueue tc $ EventCursorEnter     win ca
scrollCallback          tc win x y        = toQueue tc $ EventScroll          win x y
keyCallback             tc win k sc ka mk = toQueue tc $ EventKey             win k sc ka mk
charCallback            tc win c          = toQueue tc $ EventChar            win c

--------------------------------------------------------------------------------

-- Gives GLFW appropriate callbacks
provideCallbacks :: TQueue Event -> GLFW.Window -> IO ()
provideCallbacks events win = do
  GLFW.setErrorCallback               $ Just $ errorCallback           events
  GLFW.setWindowPosCallback       win $ Just $ windowPosCallback       events
  GLFW.setWindowSizeCallback      win $ Just $ windowSizeCallback      events
  GLFW.setWindowCloseCallback     win $ Just $ windowCloseCallback     events
  GLFW.setWindowRefreshCallback   win $ Just $ windowRefreshCallback   events
  GLFW.setWindowFocusCallback     win $ Just $ windowFocusCallback     events
  GLFW.setWindowIconifyCallback   win $ Just $ windowIconifyCallback   events
  GLFW.setFramebufferSizeCallback win $ Just $ framebufferSizeCallback events
  GLFW.setMouseButtonCallback     win $ Just $ mouseButtonCallback     events
  GLFW.setCursorPosCallback       win $ Just $ cursorPosCallback       events
  GLFW.setCursorEnterCallback     win $ Just $ cursorEnterCallback     events
  GLFW.setScrollCallback          win $ Just $ scrollCallback          events
  GLFW.setKeyCallback             win $ Just $ keyCallback             events
  GLFW.setCharCallback            win $ Just $ charCallback            events

