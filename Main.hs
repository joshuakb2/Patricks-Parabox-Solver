{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import Prelude hiding (Right, Left)
import qualified Data.Array as Arr
import Data.Array (Array)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import System.Exit (exitFailure)
import Data.List (intercalate, sort, sortOn, foldl', uncons)
import Data.Bifunctor (Bifunctor(first, second))
import Data.Maybe (mapMaybe, fromMaybe)
import BiMap (BiMap)
import qualified BiMap
import Control.Monad (when, MonadPlus (mzero))
import Control.Applicative ((<|>), Alternative (empty))
import qualified Data.Set as Set
import Data.Traversable (for)
import Control.Monad.State (execStateT, MonadState (get, put), StateT (runStateT), MonadIO (liftIO), runState)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Data.Foldable (for_, Foldable (toList))
import ListT (ListT(ListT))
import qualified ListT

main :: IO ()
main = do
    solution <- runMaybeT (solve input7)
    case solution of
        Nothing -> do
            putStrLn "No solutions were found!"
            exitFailure
        Just solution -> do
            printSolution solution

input :: Input
input = Input
    { boards = Map.fromList
        [ ('G', makeBoard 5 [repeat ' ']
          )
        ]
    , initialState = BiMap.fromList
        [ (Player, Coord 'G' (2, 3))
        , (Clone 'G' 1, Coord 'G' (1, 0))
        , (Clone 'G' 2, Coord 'G' (0, 3))
        , (Clone 'G' 3, Coord 'G' (3, 4))
        , (Clone 'G' 4, Coord 'G' (4, 1))
        ]
    , requirements = Map.fromList
        [ (Coord 'G' (1, 2), RequireNonPlayer)
        , (Coord 'G' (2, 2), RequireNonPlayer)
        , (Coord 'G' (3, 2), RequireNonPlayer)
        , (Coord 'G' (2, 1), RequirePlayer)
        ]
    }

input2 :: Input
input2 = Input
    { boards = Map.fromList
        [ ('B', makeBoard 9
            [ "XXXXXXXXX"
            , "X    XXXX"
            , "X    XXXX"
            , "X    X XX"
            , "        X"
            , "X       X"
            , "X       X"
            , "X       X"
            , "XXXX XXXX"
            ]
          )
        ]
    , initialState = BiMap.fromList
        [ (Player, Coord 'B' (3, 5))
        , (BoardPiece 'B', Coord 'B' (6, 3))
        , (Clone 'B' 1, Coord 'B' (3, 7))
        , (Block 1, Coord 'B' (0, 4))
        ]
    , requirements = Map.fromList
        [ (Coord 'B' (7, 5), RequireNonPlayer)
        , (Coord 'B' (7, 6), RequireNonPlayer)
        , (Coord 'B' (7, 7), RequirePlayer)
        ]
    }

input3 :: Input
input3 = Input
    { boards = Map.fromList
        [ ('G', makeBoard 7
            [ "XXXXXXX"
            , "X    XX"
            , "X     X"
            , "X     X"
            , "X     X"
            , "X     X"
            , "XXX XXX"
            ]
          )
        , ('Y', makeBoard 3
            [ "XXX"
            , "X  "
            , "XXX"
            ]
          )
        ]
    , initialState = BiMap.fromList
        [ (Player, Coord 'G' (3, 3))
        , (BoardPiece 'G', Coord 'G' (4, 1))
        , (Clone 'G' 1, Coord 'G' (1, 2))
        , (Clone 'G' 2, Coord 'G' (1, 5))
        , (BoardPiece 'Y', Coord 'G' (5, 5))
        ]
    , requirements = Map.fromList
        [ (Coord 'Y' (1, 1), RequirePlayer)
        ]
    }

input4 :: Input
input4 = Input
    { boards = Map.fromList
        [ ('G', makeBoard 9
            [ "XXXXXXXXX"
            , "XXXX    X"
            , "X       X"
            , "X       X"
            , "        X"
            , "XXXXXXXXX"
            , "XXXXXXXXX"
            , "XXXXXXXXX"
            , "XXXXXXXXX"
            ]
          )
        ]
    , initialState = BiMap.fromList
        [ (Player, Coord 'G' (2, 3))
        , (BoardPiece 'G', Coord 'G' (7, 1))
        , (Clone 'G' 1, Coord 'G' (7, 4))
        , (Block 1, Coord 'G' (2, 4))
        , (Block 2, Coord 'G' (3, 4))
        , (Block 3, Coord 'G' (4, 4))
        , (Block 4, Coord 'G' (5, 4))
        ]
    , requirements = Map.fromList
        [ (Coord 'G' (7, 3), RequirePlayer)
        , (Coord 'G' (0, 4), RequireNonPlayer)
        , (Coord 'G' (2, 4), RequireNonPlayer)
        , (Coord 'G' (4, 4), RequireNonPlayer)
        , (Coord 'G' (6, 4), RequireNonPlayer)
        ]
    }

input5 :: Input
input5 = Input
    { boards = Map.fromList
        [ ('G', makeBoard 9
            [ "XXXXXXXXX"
            , "X       X"
            , "X       X"
            , "X       X"
            , "        X"
            , "X       X"
            , "XXXXX XXX"
            , "XXX   XXX"
            , "XXXXXXXXX"
            ]
          )
        , ('B', makeBoard 7
            [ "XXXXXXX"
            , "X     X"
            , "X     X"
            , "X     X"
            , "X     X"
            , "X     X"
            , "XXXXXXX"
            ]
          )
        ]
    , initialState = BiMap.fromList
        [ (Player, Coord 'B' (2, 4))
        , (BoardPiece 'G', Coord 'B' (4, 2))
        , (Clone 'G' 1, Coord 'G' (4, 3))
        , (Block 1, Coord 'B' (2, 2))
        ]
    , requirements = Map.fromList
        [ (Coord 'G' (3, 7), RequirePlayer)
        , (Coord 'G' (5, 7), RequireNonPlayer)
        ]
    }

input6 :: Input
input6 = Input
    { boards = Map.fromList
        [ ('M', makeBoard 5
            [ "XXXXX"
            , "XXXXX"
            , "X  XX"
            , "XXXXX"
            , "XXXXX"
            ]
          )
        , ('A', makeBoard 3
            [ "XX "
            , "XXX"
            , "XXX"
            ]
          )
        , ('X', makeBoard 3
            [ " XX"
            , "XXX"
            , "XXX"
            ]
          )
        , ('B', makeBoard 3
            [ "XXX"
            , "XXX"
            , "XX "
            ]
          )
        , ('Y', makeBoard 3
            [ "XXX"
            , "XXX"
            , "  X"
            ]
          )
        ]
    , initialState = BiMap.fromList
        [ (Player, Coord 'B' (2, 2))
        , (BoardPiece 'A', Coord 'M' (1, 2))
        , (BoardPiece 'B', Coord 'A' (2, 0))
        , (BoardPiece 'X', Coord 'M' (2, 2))
        , (BoardPiece 'Y', Coord 'X' (0, 0))
        , (Block 1, Coord 'Y' (0, 2))
        ]
    , requirements = Map.fromList [(Coord 'Y' (0, 2), RequirePlayer)]
    }

input7 :: Input
input7 = Input
    { boards = Map.fromList
        [ ('P', makeBoard 9
            [ "XXXXXXXXX"
            , "XXX    XX"
            , "XXX    XX"
            , "XXX    XX"
            , "XX     XX"
            , "XXX    XX"
            , "XXX    XX"
            , "XXX    XX"
            , "XXXXXXXXX"
            ]
          )
        , ('B', makeBoard 5
            [ "XXXXX"
            , "XXXXX"
            , "XX   "
            , "   XX"
            , "XXXXX"
            ]
          )
        , ('G', makeBoard 5
            [ "XXXXX"
            , "XXXXX"
            , "XXXXX"
            , "XXXX "
            , "XXXXX"
            ]
          )
        ]
    , initialState = BiMap.fromList
        [ (Player, Coord 'P' (4, 4))
        , (BoardPiece 'B', Coord 'P' (3, 2))
        , (BoardPiece 'G', Coord 'P' (3, 6))
        ]
    , requirements = Map.fromList
        [ (Coord 'P' (5, 4), RequireNonPlayer)
        , (Coord 'P' (5, 2), RequirePlayer)
        ]
    }

charsToCells :: String -> [Cell]
charsToCells =
    map (\case ' ' -> Space; 'X' -> Wall)

makeBoard :: Int -> [String] -> Board
makeBoard w rows =
    Board w (Arr.listArray (0, w ^ 2 - 1) (charsToCells (concat rows)))

solve :: Input -> MaybeT IO Solution
solve Input { boards, initialState, requirements } =
    solve' [(initialState, [])] 0 (Set.singleton (canonicalGameStateStr initialState))
    where
        solve' states moves visitedStates = do
            liftIO . putStrLn $ "Moves: " ++ show moves ++ ", states: " ++ {-intercalate "   " (map (canonicalGameStateStr . fst) states)-} show (length states)
            let solutions = map (reverse . snd) $ filter (isSolved requirements . fst) states
            if not (null solutions) then
                return (head solutions)
            else if null states then
                mzero
            else do
                let calcNewStates = do
                     (state, steps) <- ListT.fromFoldable states
                     dir <- ListT.fromFoldable [Up, Down, Left, Right]
                     newState <- ListT.fromFoldable (movePiece Player (BiMap.unsafeLookupA Player state) dir Map.empty (boards, state))
                     let canonical = canonicalGameStateStr newState
                     visitedStates <- get
                     when (Set.member canonical visitedStates) mzero
                     put (Set.insert canonical visitedStates)
                     return (newState, dir : steps)
                let (newStates, newVisitedStates) = runState (ListT.toList calcNewStates) visitedStates
                solve' newStates (moves + 1) (foldl' (flip (Set.insert . canonicalGameStateStr . fst)) newVisitedStates newStates)

uniqueStates :: [(GameState, [Step])] -> [(GameState, [Step])]
uniqueStates states =
    uniqueStates' states Set.empty
    where
        uniqueStates' [] _ = []
        uniqueStates' ((state, steps):rest) seen =
            let canonical = canonicalGameStateStr state
            in if Set.member canonical seen then
                uniqueStates' rest seen
            else
                (state, steps) : uniqueStates' rest (Set.insert canonical seen)

type InMotion = Map Piece Step

movePiece :: Piece -> Coord -> Step -> InMotion -> (Boards, GameState) -> Maybe GameState
movePiece piece coord dir inMotion (boards, state) = do
    case Map.lookup piece inMotion of
        Just moving ->
            if moving == dir then Just state -- Nothing needs to be done, non-conflicting looping movement
            else Nothing -- Conflicting loop found, fail
        Nothing -> do -- No loop found
            (target, piecesExited) <- targetCell dir coord (boards, state)
            when (lookupCellByCoord target boards == Wall) Nothing
            case BiMap.lookupB target state of
                Nothing -> Just (BiMap.insert (piece, target) state)
                Just inTheWay ->
                    onPieceInTheWay piece inTheWay target dir inMotion piecesExited (boards, state)

onPieceInTheWay
    :: Piece
    -> Piece
    -> Coord
    -> Step
    -> InMotion
    -> [(Int, Int)]
    -> (Boards, GameState)
    -> Maybe (BiMap Piece Coord)
onPieceInTheWay piece inTheWay target dir inMotion boardPieceExitedCoords (boards, state) =
    let
        newInMotion = Map.insert piece dir inMotion
        push = do
            newState <- movePiece inTheWay target dir newInMotion (boards, state)
            Just (BiMap.insert (piece, target) newState)
        transfer = do
            transferPiece piece inTheWay dir newInMotion boardPieceExitedCoords (boards, state)
        goInto = do
            movePieceIntoAnother piece inTheWay dir newInMotion (boards, state)
        eat = do
            newState <- movePieceIntoAnother inTheWay piece (opposite dir) newInMotion (boards, state)
            Just (BiMap.insert (piece, target) newState)
    in push <|> transfer <|> goInto <|> eat

movePieceIntoAnother :: Piece -> Piece -> Step -> InMotion -> (Boards, GameState) -> Maybe GameState
movePieceIntoAnother piece into dir inMotion (boards, state) = do
    boardChar <- case into of
        BoardPiece boardChar -> Just boardChar
        Clone boardChar _ -> Just boardChar
        _ -> Nothing
    let board = boards Map.! boardChar
    let (x, y) = getEntryCellXY dir board
    when (lookupCellByXY (x, y) board == Wall) Nothing
    let target = Coord boardChar (x, y)
    case BiMap.lookupB target state of
        Nothing -> Just (BiMap.insert (piece, target) state)
        Just inTheWay -> onPieceInTheWay piece inTheWay target dir inMotion [] (boards, state)

getEntryCellXY :: Step -> Board -> (Int, Int)
getEntryCellXY dir (Board w _) =
    case dir of
        Up -> (w `div` 2, w - 1)
        Down -> (w `div` 2, 0)
        Left -> (w - 1, w `div` 2)
        Right -> (0, w `div` 2)

transferPiece :: Piece -> Piece -> Step -> InMotion -> [(Int, Int)] -> (Boards, GameState) -> Maybe GameState
transferPiece piece into dir inMotion boardPieceExitedCoords (boards, state) = do
    (ourCoord, rest) <- uncons boardPieceExitedCoords
    boardChar <- case into of
        BoardPiece boardChar -> Just boardChar
        Clone boardChar _ -> Just boardChar
        _ -> Nothing
    let board = boards Map.! boardChar
    let (x, y) = getTransferCellXY dir board ourCoord
    when (lookupCellByXY (x, y) board == Wall) Nothing
    let target = Coord boardChar (x, y)
    case BiMap.lookupB target state of
        Nothing -> Just (BiMap.insert (piece, target) state)
        Just inTheWay -> onPieceInTheWay piece inTheWay target dir inMotion rest (boards, state)

getTransferCellXY :: Step -> Board -> (Int, Int) -> (Int, Int)
getTransferCellXY dir (Board w _) (x, y) =
    case dir of
        Up -> (x, w - 1)
        Down -> (x, 0)
        Left -> (w - 1, y)
        Right -> (0, y)

-- Int represents the original coordinate before each time we exit a board piece
type Target = (Coord, [(Int, Int)])

targetCell :: Step -> Coord -> (Boards, GameState) -> Maybe Target
targetCell dir coord (boards, state) =
    targetCell' coord Set.empty []
    where
        targetCell' (Coord board (x, y)) boardsExited exitFromCoords
          | outOfBounds (width (boards Map.! board)) newXY = do
                when (Set.member board boardsExited) Nothing
                boardPieceCoord <- findPiece board state
                targetCell' boardPieceCoord (Set.insert board boardsExited) newExitFromCoords
          | otherwise =
                Just (Coord board newXY, exitFromCoords)
            where
                newXY = delta dir (x, y)
                newExitFromCoords = (x, y) : exitFromCoords

outOfBounds :: Int -> (Int, Int) -> Bool
outOfBounds w (x, y) = x < 0 || y < 0 || x >= w || y >= w

findPiece :: Char -> GameState -> Maybe Coord
findPiece board =
    BiMap.lookupA (BoardPiece board)

indexOf :: Eq a => a -> [(Int, a)] -> Maybe Int
indexOf a [] = Nothing
indexOf a ((index, b):rest) = if a == b then Just index else indexOf a rest

delta :: Step -> (Int, Int) -> (Int, Int)
delta Up (x, y) = (x, y - 1)
delta Down (x, y) = (x, y + 1)
delta Left (x, y) = (x - 1, y)
delta Right (x, y) = (x + 1, y)

opposite :: Step -> Step
opposite Up = Down
opposite Down = Up
opposite Left = Right
opposite Right = Left

printSolution :: Solution -> IO ()
printSolution steps =
    for_ (zip [1..] steps) \(i, step) ->
        putStrLn $ show i ++ ". " ++ show step

type Solution = [Step]

data Step = Up | Down | Left | Right deriving (Eq, Show)

type Boards = Map Char Board

data Input = Input
    { boards :: Boards
    , initialState :: GameState
    , requirements :: Requirements
    }

type GameState = BiMap Piece Coord

canonicalGameStateStr :: GameState -> String
canonicalGameStateStr state =
    intercalate "|"
        [ show (BiMap.unsafeLookupA Player state)
        , intercalate ";" (sort $ map (show . snd) $ filter (isBlock . fst) $ BiMap.toList state)
        , intercalate ";" $ map (\(board, coord) -> board : (":" ++ show coord)) $ getBoardPieces $ BiMap.toList state
        , intercalate ";" $ map (\(board, coords) -> board : (":" ++ intercalate "&" (map show coords))) $ getClonePieces $ BiMap.toList state
        ]

isSolved :: Requirements -> GameState -> Bool
isSolved requirements state =
    all requirementMet (Map.toList requirements)
    where
        requirementMet (coord, req) =
            case (BiMap.lookupB coord state, req) of
                (Just (Block _), RequireNonPlayer) -> True
                (Just (BoardPiece _), RequireNonPlayer) -> True
                (Just (Clone _ _), RequireNonPlayer) -> True
                (Just Player, RequirePlayer) -> True
                _ -> False

getBoardPieces :: [(Piece, Coord)] -> [(Char, Coord)]
getBoardPieces =
    sortOn fst . mapMaybe (\case (BoardPiece board, coord) -> Just (board, coord); _ -> Nothing)

getClonePieces :: [(Piece, Coord)] -> [(Char, [Coord])]
getClonePieces =
    sortOn fst . joinByChar . mapMaybe (\case (Clone board _, coord) -> Just (board, coord); _ -> Nothing)
    where
        joinByChar :: [(Char, Coord)] -> [(Char, [Coord])]
        joinByChar =
            map (second sort)
            . Map.toList
            . foldl' (\map (c, coord) ->
                Map.alter (maybe (Just [coord]) (Just . (coord :))) c map
            ) Map.empty

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

isBlock :: Piece -> Bool
isBlock (Block _) = True
isBlock _ = False

data Coord = Coord
    { board :: Char
    , space :: (Int, Int)
    }
    deriving (Eq, Ord)

instance Show Coord where
    show (Coord board (x, y)) = board : (show x ++ "," ++ show y)

xyToIndex :: Int -> (Int, Int) -> Int
xyToIndex w (x, y) = w * y + x

data Cell = Wall | Space deriving (Eq, Ord)

lookupCellByCoord :: Coord -> Boards -> Cell
lookupCellByCoord (Coord board (x, y)) boards =
    lookupCellByXY (x, y) (boards Map.! board)

lookupCellByXY :: (Int, Int) -> Board -> Cell
lookupCellByXY (x, y) (Board w cells) =
    cells Arr.! xyToIndex w (x, y)
