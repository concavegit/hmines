{-# LANGUAGE TupleSections #-}

module Board where

import           Control.Arrow
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Array                   (Array)
import qualified Data.Array                   as A
import           Data.IntSet                  (IntSet)
import qualified Data.IntSet                  as IntSet
import           Data.List
import           Data.Maybe
import           Data.Vector                  (Vector)
import qualified Data.Vector                  as V
import           Math.Geometry.Grid
import           Math.Geometry.Grid.Octagonal
import           Numeric.LinearAlgebra
import           System.Random

import           Types

randRUniq :: RandomGen g => (Int, Int) -> State (g, IntSet) Int
randRUniq (l, h) = state $ \(g, ns) ->
  let vacant = IntSet.toList $ IntSet.fromList [l..h] IntSet.\\ ns
      (idx, g') = randomR (0, length vacant - 1) g
      n = vacant !! idx
  in (n, (g', IntSet.insert  n ns))

randRUniq1 :: RandomGen g => Int -> (Int, Int) -> State g [Int]
randRUniq1 n range  = state
  $ second fst . runState (replicateM n $ randRUniq range) . (, IntSet.empty)

randSubList :: RandomGen g => Int -> [a] -> State g [a]
randSubList n xs = map (xs !!) <$> randRUniq1 n (0, length xs - 1)

genMinesSafe
  :: RandomGen g => Int -> (Int, Int) -> (Int, Int) -> State g [(Int, Int)]
genMinesSafe n (w, h) (x, y) = randSubList n
  $ ((,) <$> [0..w-1] <*> [0..h-1]) \\ ((,) <$> [x-1..x+1] <*> [y-1..y+1])

minesMask :: (Int, Int) -> [(Int, Int)] -> Matrix Double
minesMask (w, h) xs =
  let blank = V.replicate (w * h) 0
      withMines = blank V.// zip (map (uncurry $ (. (* w)) . (+)) xs) (repeat 1)
  in subMatrix (1, 1) (h, w) . conv2 (konst 1 (3, 3)) . (h >< w)
  $ V.toList withMines

minesKey :: (Int, Int) -> [(Int, Int)] -> Array (Int, Int) Tile
minesKey d@(w, h) xs = A.listArray ((0, 0), (w - 1, h - 1))
  (pure . round <$> toList (flatten $ minesMask d xs))
  A.// zip xs (repeat Nothing)

minesBoard
  :: RandomGen g
  => Int -> (Int, Int) -> (Int, Int) -> State g (Array (Int, Int) Tile)
minesBoard n d = fmap (minesKey d) . genMinesSafe n d

gameBoard :: RandomGen g => Int -> (Int, Int) -> (Int, Int) -> State g Board
gameBoard = ((fmap ((, Uncovered) <$>) .) .) . minesBoard

uncoverMines :: Board -> Board
uncoverMines = fmap (\b@(t, _) -> if isJust t then b else (Nothing, Uncovered))

addMark :: Mark -> (Bool, Board) -> (Int, Int) -> (Bool, Board)
addMark m s@(b, board) x
  | b = (True, board A.// [(x, second (const m) $ board A.! x)])
  | otherwise = s

mark = addMark Marked
flag = addMark Flagged

uncover :: (Bool, Board) -> (Int, Int) -> (Bool, Board)
uncover s@(inPlay, board) idx
  | inPlay && isNothing tile = (False, uncoverMines board)
  | inPlay && (mark == Covered || mark == Marked) && maybe False (/= 0) tile
  = (inPlay, board A.// [(idx, (tile, Uncovered))])
  | otherwise = s
  where
    (tile, mark) = board A.! idx
    surrounding =
      let ((x0, y0), (x1, y1)) = A.bounds board
          (boardW, boardH) = (x1 - x0 + 1, y1 - y0 + 1)
      in neighbours (rectOctGrid boardH boardW) idx
