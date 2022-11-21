module Utils where

import qualified Data.HashMap as Map
import Foreign.C (CFloat)
import GHC.Exts (coerce)
import Raylib.Types (Rectangle (..), Vector2 (..))

permutate :: [a] -> [a] -> [(a, a)]
permutate l1 l2 = (,) <$> l1 <*> l2

sumV2 :: Vector2 -> CFloat
sumV2 (Vector2 x y) = x + y

-- | Elevate to exponent
(|**|) :: Vector2 -> CFloat -> Vector2
(Vector2 x y) |**| exponent = Vector2 (x ** exponent) (y ** exponent)

-- | Adds a Vector2 with another Vector2
(|+|) :: Vector2 -> Vector2 -> Vector2
(Vector2 x1 y1) |+| (Vector2 x2 y2) = Vector2 (x1 + x2) (y1 + y2)

(|+#|) :: Vector2 -> CFloat -> Vector2
(Vector2 x y) |+#| factor = Vector2 (x + factor) (y + factor)

-- | Subtract a Vector2 from another Vector2
(|-|) :: Vector2 -> Vector2 -> Vector2
(Vector2 x1 y1) |-| (Vector2 x2 y2) = Vector2 (x1 - x2) (y1 - y2)

-- | Subtract a factor from a Vector2
(|-#|) :: Vector2 -> CFloat -> Vector2
(Vector2 x y) |-#| factor = Vector2 (x - factor) (y - factor)

-- | Divide a Vector2 by another Vector2
(|/|) :: Vector2 -> Vector2 -> Vector2
(Vector2 x1 y1) |/| (Vector2 x2 y2) = Vector2 (x1 / x2) (y1 / y2)

-- | Divide a Vector2 by a factor
(|/#|) :: Vector2 -> CFloat -> Vector2
(Vector2 x y) |/#| factor = Vector2 (x / factor) (y / factor)

-- | Multiply a Vector2 by another Vector2
(|*|) :: Vector2 -> Vector2 -> Vector2
(Vector2 x1 y1) |*| (Vector2 x2 y2) = Vector2 (x1 * x2) (y1 * y2)

-- | Multiply a Vector2 by a factor
(|*#|) :: Vector2 -> CFloat -> Vector2
(Vector2 x y) |*#| factor = Vector2 (x * factor :: CFloat) (y * factor)

vectorLength :: Vector2 -> CFloat
vectorLength (Vector2 x y) = sqrt . sumV2 $ Vector2 (x ** 2) (y ** 2)

normalizeVector :: Vector2 -> Vector2
normalizeVector vec = vec |/#| vectorLength vec

-- normalizeVector vec = (/ vectorLength vec) <$> vec

areBoxesColliding :: Rectangle -> Rectangle -> Bool
areBoxesColliding (Rectangle b1x b1y b1w b1h) (Rectangle b2x b2y b2w b2h) =
  b1x + (b1w - 1) >= b2x
    && b1x <= b2x + (b2w - 1)
    && b1y + (b1h - 1) >= b2y
    && b1y <= b2y + (b2h - 1)

isPointInsideBox :: Vector2 -> Rectangle -> Bool
isPointInsideBox (Vector2 px py) (Rectangle bx by bw bh) =
  px >= bx
    && px <= (bx + bw - 1.0)
    && py >= by
    && py <= (by + bh - 1.0)

mergeMaps :: (Ord k, Num a) => Map.Map k a -> Map.Map k a -> Map.Map k a
mergeMaps = Map.unionWith (+)
