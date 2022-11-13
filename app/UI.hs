{-# LANGUAGE ScopedTypeVariables #-}
module UI where
import Components (System', CameraComponent (..), TextureButton (..), UIElement (..), ButtonState (..), InputState (..))
import Raylib.Types (Texture, Camera2D (..), Vector2 (..), Rectangle (..))
import Apecs
import Raylib (drawFPS, drawTexturePro)
import Control.Monad (when)
import Tilemap (tileSizeCF)
import Linear.Vector (Additive(zero))
import Raylib.Colors (white)

drawUI :: Texture -> System' ()
drawUI uiAtlas = do
    liftIO $ drawFPS 10 10
    drawUITextureButtons uiAtlas
    return ()

drawUITextureButtons :: Texture -> System' ()
drawUITextureButtons uiAtlas = do
  cmapM_ $ \(CameraComponent _ (Camera2D _ _ _ zoom)) -> do
    cmapM_ $ \(Button (Rectangle rx ry rw rh) state _, UIElement pos@(Vector2 ex ey) _ _ visible) -> do
      when visible $ do
        let newRX = case state of
              Hovered -> rx + 16.0
              Held -> rx + 32.0
              _ -> rx
        let src = Rectangle newRX ry rw rh
        let dest = Rectangle ex (ey - (tileSizeCF * zoom)) (tileSizeCF * zoom) (tileSizeCF * zoom)
        liftIO $ drawTexturePro uiAtlas src dest (Vector2 0.0 0.0) 0.0 white
