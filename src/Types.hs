module Types
  ( Tile
  , Mark (..)
  , GameTile
  , Board
  ) where

import           Control.Arrow
import qualified Control.Category as Cat
import           Data.Array

data Mark = Uncovered | Flagged | Marked | Covered deriving (Eq, Show)
type Tile = Maybe Int
type GameTile = (Tile, Mark)
type Board = Array (Int, Int) GameTile
