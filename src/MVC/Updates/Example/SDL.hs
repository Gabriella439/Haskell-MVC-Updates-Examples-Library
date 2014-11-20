module MVC.Updates.Example.SDL where

import Control.Applicative
import Control.Foldl (lastDef)
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image (load)
import MVC
import MVC.Updates
import MVC.Prelude (producer)

import Paths_mvc_updates_examples

data Frame = Frame0 | Frame1 | Frame2 | Frame3

data Cloud = Cloud Frame Int Int

data Mouse = Mouse Int Int

sdl :: Managed (View Cloud, Updatable Mouse)
sdl = managed $ \k -> withInit [InitVideo, InitEventthread] $ do
    surface <- setVideoMode 640 480 32 [SWSurface]
    pathFrame0 <- getDataFileName "frame0.png"
    pathFrame1 <- getDataFileName "frame1.png"
    pathFrame2 <- getDataFileName "frame2.png"
    pathFrame3 <- getDataFileName "frame3.png"
    pathIsland <- getDataFileName "island.jpg"
    frame0  <- load pathFrame0
    frame1  <- load pathFrame1
    frame2  <- load pathFrame2
    frame3  <- load pathFrame3
    island  <- load pathIsland
    _       <- blitSurface island Nothing surface (Just (Rect 0 0 640 480))

    let view = asSink $ \(Cloud f x y) -> do
            let frame = case f of
                    Frame0 -> frame0
                    Frame1 -> frame1
                    Frame2 -> frame2
                    Frame3 -> frame3
            _ <- blitSurface island Nothing surface (Just (Rect 0 0 640 480))
            _ <- blitSurface frame  Nothing surface (Just (Rect (x - 66) (y - 36) 132 72))
            SDL.flip surface

        mouseMotion :: Producer Mouse IO ()
        mouseMotion = do
            e <- lift waitEvent
            case e of
                MouseMotion x y _ _ -> do
                    let x' = fromIntegral x
                        y' = fromIntegral y
                    yield (Mouse x' y')
                    mouseMotion
                Quit                -> lift quit
                _                   -> mouseMotion

        mouse :: Updatable Mouse
        mouse = on (lastDef (Mouse 320 240)) (producer Single mouseMotion)

    k (view, mouse) <* quit
