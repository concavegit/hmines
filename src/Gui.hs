{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
module Gui where

import           Data.GI.Base
import qualified GI.Gtk                     as Gtk
import           Reactive.Banana
import           Reactive.Banana.Frameworks
import           Reactive.Banana.GI.Gtk

gui :: IO ()
gui = do
  Gtk.init Nothing
  win <- new Gtk.Window [ #title := "Hello" ]
  on win #destroy Gtk.mainQuit
  button <- new Gtk.Button [#label := "Click me"]
  on button #clicked (set button [#sensitive := False,
                                 #label := "Thanks for clicking"])
  #add win button
  #showAll win
  Gtk.main

networkDescription = do
  window <- new Gtk.Window [#title := "Hello"]
  grid <- new Gtk.Grid []
  destroyE <- signalE0 window #destroy
  reactimate $ Gtk.mainQuit <$ destroyE
  button <- new Gtk.Button [#label := "Click Me"]
  pressedE <- signalE0 button #clicked
  reactimate (putStrLn "Hi" <$ pressedE)
  #add window grid

  #attach grid button 0 0 1 1
  #showAll window

runGui = do
  Gtk.init Nothing
  compile networkDescription >>= actuate
  Gtk.main
