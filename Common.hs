module Common where

import Data.Array (Array)
import Data.Map.Strict (Map)
import BiMap (BiMap)

type Solution = [Step]

data Step = Up | Down | Left | Right deriving (Eq, Show)

type Boards = Map Char Board

data Input = Input
    { boards :: Boards
    , initialState :: GameState
    , requirements :: Requirements
    }

type GameState = BiMap Piece Coord

type Requirements = Map Coord Requirement

data Requirement = RequirePlayer | RequireNonPlayer

data Board = Board
    { width :: Int
    , cells :: Array Int Cell
    }

data Piece
    = Player
    | Block Int
    | BoardPiece Char
    | Clone Char Int
    deriving (Eq, Ord, Show)

data Coord = Coord
    { board :: Char
    , space :: (Int, Int)
    }
    deriving (Eq, Ord)

instance Show Coord where
    show (Coord board (x, y)) = board : (show x ++ "," ++ show y)

data Cell = Wall | Space deriving (Eq, Ord)
