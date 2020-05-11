-- A module containing definitions for creating hexagons

module Client.Rendering.Geometry.Hexagon
  ( hexagon
  , hexagonalPrism
  ) where

import Data.Word
import Linear.V2
import Linear.V3
import Linear.Affine

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

-- Create a hexagonal prism centered at the base at (0, 0, 0)
hexagonalPrism :: Float -> ([Point V3 Float], [Word32])
hexagonalPrism height = (vertices, indices)
  where hex@(vs, is) = hexagon
        len = toEnum (length vs)
        top = (fmap (\(P (V2 x y)) -> P $ V3 x height y) vs, is)
        bottom = (fmap (\(P (V2 x y)) -> P $ V3 x 0 y) vs, (+ len) <$> reverse is)
        side aTop bTop aBot bBot = [aTop, bTop, aBot, aBot, bTop, bBot]
        sides = concat [side i (i + 1) (i + len) (i + 1 + len) | i <- [1..5]]
        vertices = fst top ++ fst bottom
        indices = snd top ++ snd bottom ++ sides ++ side 6 1 (6 + len) (1 + len)
        
