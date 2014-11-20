 -- This must be compiled with the `-threaded` flag

 module MVC.Updates.Example.Spreadsheet (spreadsheet) where

 import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
 import Control.Concurrent.Async (async, wait)
 import Control.Foldl (lastDef)
 import Graphics.UI.Gtk as GTK
 import MVC as MVC
 import MVC.Updates as MVC
 
 makeInCell :: VBox -> Updatable Double
 makeInCell vBox = MVC.on (lastDef 0) $ managed $ \k -> do -- On
     (output, input) <- spawn Unbounded
     spinButton <- spinButtonNewWithRange 0 100 1
     _ <- onValueSpinned spinButton $ do
         n <- get spinButton spinButtonValue
         _ <- atomically (send output n)
         return ()
     boxPackStartDefaults vBox spinButton
     widgetShowAll vBox
     k (asInput input)
 
 makeOutCell :: VBox -> Managed (View Double)
 makeOutCell vBox = liftIO $ do
     entry <- entryNew
     boxPackStartDefaults vBox entry
     return $ asSink $ \n -> postGUISync $ entrySetText entry (show n)
 
 spreadsheet :: Managed (Updatable Double, Managed (View Double), IO ())
 spreadsheet = managed $ \k -> do
     _ <- initGUI
     window <- windowNew
     hBox   <- hBoxNew False 0
     vBoxL  <- vBoxNew False 0
     vBoxR  <- vBoxNew False 0
     set window [windowTitle := "Spreadsheet", containerChild := hBox]
     boxPackStartDefaults hBox vBoxL
     boxPackStartDefaults hBox vBoxR
 
     mvar <- newEmptyMVar
     a    <- async $ k (makeInCell vBoxL, makeOutCell vBoxR, putMVar mvar ())
     takeMVar mvar
 
     _ <- GTK.on window deleteEvent $ do -- on
         liftIO mainQuit
         return False
     widgetShowAll window
     mainGUI
     wait a
