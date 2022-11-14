module Utils where

import Raylib.Types (Rectangle (..), Vector2 (..))

permutate :: [a] -> [a] -> [(a, a)]
permutate l1 l2 = (,) <$> l1 <*> l2

areBoxesColliding :: Rectangle -> Rectangle -> Bool
areBoxesColliding (Rectangle b1x b1y b1w b1h) (Rectangle b2x b2y b2w b2h) = 
    b1x + (b1w - 1) >= b2x &&
    b1x <= b2x + (b2w - 1) &&
    b1y + (b1h - 1) >= b2y &&
    b1y <= b2y + (b2h - 1)

isPointInsideBox :: Vector2 -> Rectangle -> Bool
isPointInsideBox (Vector2 px py) (Rectangle bx by bw bh) = 
    px >= bx &&
    px <= (bx + bw - 1.0) &&
    py >= by &&
    py <= (by + bh - 1.0)
    
