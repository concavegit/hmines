module Types
        ( Board
        , GameTile
        , Tile
        , Mark(..)
        )
where

import           Data.Array

data Mark = Uncovered | Flagged | Marked | Covered deriving (Eq, Show)
type Tile = Maybe Int
type GameTile = (Tile, Mark)
type Board = Array (Int, Int) GameTile
