import Control.Applicative
import Control.Foldl (length)
import MVC
import MVC.Updates
import MVC.Prelude (tick)
import Prelude hiding (length)

import MVC.Updates.Example.SDL

main :: IO ()
main = runMVC () (asPipe cat) $ do
    (cloudOut, mouse) <- sdl
    let seconds = on length (tick (1 / 60))

        toFrame n = case (n `div` 15) `rem` 4 of
            0 -> Frame0
            1 -> Frame1
            2 -> Frame2
            _ -> Frame3

        cloudOrbit t (Mouse x y) = Cloud (toFrame t) x' y'
          where
            x' = x + truncate (100 * cos (fromIntegral t / 10) :: Double)
            y' = y + truncate (100 * sin (fromIntegral t / 10) :: Double)

    cloudIn <- updates Unbounded (cloudOrbit <$> seconds <*> mouse)
    return (cloudOut, cloudIn)
