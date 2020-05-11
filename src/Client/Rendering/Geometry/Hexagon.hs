-- A module containing definitions for creating hexagons

module Client.Rendering.Geometry.Hexagon
  ( hexagon
  ) where

import Data.Word (Word32)
import Linear.V2 (V2(..))
import Linear.Affine (Point(..))

-- Hard coded hexagon size
hexagonSize :: Float
hexagonSize = 1

-- Create a hexagon centered at (0, 0)
hexagon :: ([Point V2 Float], [Word32])
hexagon = (vertices, indices)
  where center = P $ V2 0 0
        toRad a = (pi / 180.0) * a
        calcX angle = hexagonSize * cos angle
        calcY angle = hexagonSize * sin angle
        angles = (\a -> toRad (60 * a - 30)) <$> [0..5]
        vertices = center : [P (V2 (calcX a) (calcY a)) | a <- angles]
        indices = concat [[0, i + 1, i] | i <- [1..5]] ++ [0, 1, 6]
