-- A module containing camera functionality
module Client.Camera
  ( Camera (..)
  , createCamera
  , getProjectionMatrix
  ) where

import Linear.V2
import Linear.V3
import Linear.Metric
import Linear.Matrix
import Linear.Projection

-- Cameras are used to view a scene
data Camera = Camera
  { cameraPosition  :: V3 Float
  , cameraForward   :: V3 Float
  , cameraRight     :: V3 Float
  , cameraPitch     :: Float
  , cameraYaw       :: Float
  , cameraView      :: M44 Float
  } deriving (Eq, Show)

-- Create a camera with given position, pitch and yaw values
createCamera :: V3 Float -> Float -> Float -> Camera
createCamera pos pitch yaw = Camera pos forward right pitch yaw view
  where forward = calculateForward (radians pitch) (radians yaw)
        right = calculateRight forward
        view = calculateViewMatrix pos forward

-- Calculate the forward vector
calculateForward :: Float -> Float -> V3 Float
calculateForward pitch yaw = normalize $ V3 x y z
  where x = cos yaw * cos pitch
        y = sin pitch
        z = sin yaw * cos pitch

-- Calculate the 'right' vector
calculateRight :: V3 Float -> V3 Float
calculateRight forward = normalize $ cross forward $ V3 0 1 0

-- Calculate view matrix
calculateViewMatrix :: V3 Float -> V3 Float -> M44 Float
calculateViewMatrix pos forward = lookAt pos target $ V3 0 1 0
  where target = pos + forward

-- Get projection matrix
getProjectionMatrix :: V2 Int -> M44 Float
getProjectionMatrix size = perspective 1.5 (w / h) 0.1 100
  where (V2 w h) = fromIntegral <$> size

-- Convert an angle from degrees to radians
radians :: Float -> Float
radians angle = pi * (angle / 180)
