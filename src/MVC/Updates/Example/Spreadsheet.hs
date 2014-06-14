 -- This must be compiled with the `-threaded` flag

 module MVC.Updates.Example.Spreadsheet (spreadsheet) where

 import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
 import Control.Concurrent.Async (async, wait)
 import Control.Foldl (lastDef)
 import Graphics.UI.Gtk
 import MVC
 import MVC.Updates
 
 makeInCell :: VBox -> Updatable Double
 makeInCell vBox = On (lastDef 0) $ managed $ \k -> do
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
 
     _ <- on window deleteEvent $ do
         liftIO mainQuit
         return False
     widgetShowAll window
     mainGUI
     wait a
