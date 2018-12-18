{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Gtk.Widgets where

import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.Array                   (Array)
import qualified Data.Array                   as A
import           Data.Foldable
import           Data.GI.Base
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified GI.Gtk                       as Gtk
import           Math.Geometry.Grid
import           Math.Geometry.Grid.Octagonal
import           Reactive.Banana
import           Reactive.Banana.Frameworks
import           Reactive.Banana.GI.Gtk
import           System.Random

import           Board
import           Types

unionsConst :: [Event a] -> Event a
unionsConst = foldr1 (unionWith const)

genBoard :: (Int, Int) -> MomentIO (Gtk.Grid, Event (Bool, Board))
genBoard d@(h, w) = do
  grid <- new Gtk.Grid []
  btns <- A.array ((0, 0), (h-1, w-1)) <$> traverse
    (\(p@(r, c)) -> do
        btn <- new Gtk.Button
          [ #hexpand := True
          , #vexpand := True
          ]
        #attach grid btn (fromIntegral c) (fromIntegral r) 1 1
        pure (p, btn)
    ) (indices $ rectOctGrid w h)

  btnEs <- traverse (\(p, btn) -> (p <$) <$> signalE0 btn #clicked)
    $ A.assocs btns
  firstClick <- stepper True . unionsConst $ (False <$) <$> btnEs

  let btnUncovers = fmap (flip uncover) <$> btnEs
      btnGenerate = unionsConst
        $ fmap (\p -> const (uncover (True, evalState (gameBoard 10 d p) (mkStdGen 0)) p)) <$> btnEs
      btnGenerateFst = whenE firstClick btnGenerate

  boardE <- accumE (True, undefined) (unionWith const btnGenerateFst (unions btnUncovers))

  let labelBoardE = labelBtns btns . snd <$> boardE
  reactimate labelBoardE

  pure (grid, boardE)

labelBtns :: MonadIO m => Array (Int, Int) Gtk.Button -> Board -> m ()
labelBtns grid board =
  traverse_
    (\(p, btn) ->
        set btn [ #label := btnLabel (board A.! p) ]
        ) $ A.assocs grid

btnLabel :: GameTile -> Text
btnLabel (Nothing, Uncovered) = "💣"
btnLabel (Just n, Uncovered)  = T.pack $ show n
btnLabel (_, Covered)         = ""
btnLabel (_, tileMark)        = T.pack $ show tileMark

networkDescription :: MomentIO ()
networkDescription = do
  win <- new Gtk.Window [#title := "hMines"]
  destroyE <- signalE0 win #destroy
  reactimate $ Gtk.mainQuit <$ destroyE

  (grid, _) <- genBoard (8, 8)
  #add win grid
  #showAll win

runGui :: IO ()
runGui = do
  _ <- Gtk.init Nothing
  compile networkDescription >>= actuate
  Gtk.main
