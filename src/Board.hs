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
import qualified Data.Vector                  as V
import           Math.Geometry.Grid
import           Math.Geometry.Grid.Octagonal
import           Numeric.LinearAlgebra
import           System.Random

import           Types

-- | Generate a unique 'Int' in a range.
-- The state variable is a random number seed and an 'IntSet' of used
-- 'Int's.
randRUniq :: RandomGen g => (Int, Int) -> State (g, IntSet) Int
randRUniq (l, h) = state $ \(g, ns) ->
  let vacant = IntSet.toList $ IntSet.fromList [l..h] IntSet.\\ ns
      (idx, g') = randomR (0, length vacant - 1) g
      n = vacant !! idx
  in (n, (g', IntSet.insert  n ns))

-- | Generate 'n' random unique numbers in an inclusive range.
randRUniq1 :: RandomGen g => Int -> (Int, Int) -> State g [Int]
randRUniq1 n bounds  = state
  $ second fst . runState (replicateM n $ randRUniq bounds) . (, IntSet.empty)

-- | Generate a subset of a list.
randSubList :: RandomGen g => Int -> [a] -> State g [a]
randSubList n xs = map (xs !!) <$> randRUniq1 n (0, length xs - 1)

-- | Generate 'n' mines on a board that is 'w' columns by 'h' rows
-- such that none are adjacent to the specified location ('x', 'y').
genMinesSafe
  :: RandomGen g => Int -> (Int, Int) -> (Int, Int) -> State g [(Int, Int)]
genMinesSafe n (w, h) (x, y) = randSubList n
  $ ((,) <$> [0..w-1] <*> [0..h-1]) \\ ((,) <$> [x-1..x+1] <*> [y-1..y+1])

-- | Generate a logical matrix of dimensions ('w', 'h') representing
-- the locations of the mines in 'xs'.
minesMask :: (Int, Int) -> [(Int, Int)] -> Matrix Double
minesMask (w, h) xs =
  let blank = V.replicate (w * h) 0
      withMines = blank V.// zip (map (uncurry $ (. (* w)) . (+)) xs) (repeat 1)
  in subMatrix (1, 1) (h, w) . conv2 (konst 1 (3, 3)) . (h >< w)
  $ V.toList withMines

-- | Generate the minesweeper answer key of dimensions 'd' and mines at 'xs'.
minesKey :: (Int, Int) -> [(Int, Int)] -> Array (Int, Int) Tile
minesKey d@(w, h) xs = A.listArray ((0, 0), (w - 1, h - 1))
  (pure . round <$> toList (flatten $ minesMask d xs))
  A.// zip xs (repeat Nothing)

-- | Generate a random minesweeper key with 'n' mines and dimensions 'd'.
minesBoard
  :: RandomGen g
  => Int -- ^ The amount of mines to generate.
  -> (Int, Int) -- ^ The dimensions of the board.
  -> (Int, Int) -- ^ The initial safe location. No mines will be
    -- adjacent to this cell.
  -> State g (Array (Int, Int) Tile)
minesBoard n d = fmap (minesKey d) . genMinesSafe n d

gameBoard :: RandomGen g => Int -> (Int, Int) -> (Int, Int) -> State g Board
gameBoard = ((fmap ((, Covered) <$>) .) .) . minesBoard

uncoverMines :: Board -> Board
uncoverMines = fmap (\b@(t, _) -> if isJust t then b else (Nothing, Uncovered))

addMark :: Mark -> (Bool, Board) -> (Int, Int) -> (Bool, Board)
addMark m s@(b, board) x
  | b = (True, board A.// [(x, second (const m) $ board A.! x)])
  | otherwise = s

mark, flag :: (Bool, Board) -> (Int, Int) -> (Bool, Board)
mark = addMark Marked
flag = addMark Flagged

surroundingTiles :: Board -> (Int, Int) -> [(Int, Int)]
surroundingTiles board idx =
  let ((x0, y0), (x1, y1)) = A.bounds board
      boardW = x1 - x0 + 1
      boardH = y1 - y0 + 1
  in neighbours (rectOctGrid boardH boardW) idx

uncover :: (Bool, Board) -> (Int, Int) -> (Bool, Board)
uncover s@(inPlay, board) idx
  -- Click on a mine
  | inPlay && isNothing tile = (False, uncoverMines board)

  -- Click on a covered or marked positive square
  | inPlay && (tileMark == Covered || tileMark == Marked)
    && not isZero
  = (inPlay, board A.// [(idx, (tile, Uncovered))])

  -- Click on a square that is either 0 or marked all around
  | inPlay && (isZero || tile == Just (length unFlaggedSurround))
  = foldr (flip uncover) (True, board A.// [(idx, (tile, Uncovered))]) unFlaggedSurround

  | otherwise = s
  where
    (tile, tileMark) = board A.! idx
    isZero = tile == Just 0
    surrounding = surroundingTiles board idx
    flaggedSurround = filter ((== Flagged) . snd . (board A.!)) surrounding
    unFlaggedSurround = surrounding \\ flaggedSurround
