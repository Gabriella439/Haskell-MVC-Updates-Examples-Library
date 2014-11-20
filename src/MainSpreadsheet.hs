{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Applicative (Applicative, (<$>), (<*>))
import Lens.Family.TH (makeLenses)
import MVC
import MVC.Updates (updates)
import MVC.Updates.Example.Spreadsheet (spreadsheet)

data In  = I
    { _i1 :: Double
    , _i2 :: Double
    , _i3 :: Double
    , _i4 :: Double
    }

data Out = O
    { _o1 :: Double
    , _o2 :: Double
    , _o3 :: Double
    , _o4 :: Double
    }
makeLenses ''Out
o1, o2, o3, o4 :: Functor f => (Double -> f Double) -> (Out -> f Out)

model :: Model () In Out
model = asPipe $ loop $ \(I i1 i2 i3 i4) -> do
    return $ O (i1 + i2) (i2 * i3) (i3 - i4) (max i4 i1)

main :: IO ()
main = runMVC () model $ do
    (inCell, outCell, go) <- spreadsheet
    c <- updates Unbounded $
        I <$> inCell <*> inCell <*> inCell <*> inCell
    v <- fmap (handles o1) outCell
      <> fmap (handles o2) outCell
      <> fmap (handles o3) outCell
      <> fmap (handles o4) outCell
    liftIO go
    return (v, c)
