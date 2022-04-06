{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Prelude hiding (log, Right, Left)
import qualified Data.Array as Arr
import Data.Array (Array)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import System.Exit (exitFailure)
import Data.List (intercalate, sort, sortOn, foldl')
import Data.Bifunctor (Bifunctor(first, second))
import Data.Maybe (mapMaybe, fromMaybe)

import BiMap (BiMap)
import qualified BiMap
import Control.Monad (when, MonadPlus (mzero))
import Control.Applicative ((<|>), Alternative (empty))
import qualified Data.Set as Set
import Data.Traversable (for)
import Control.Monad.State (execStateT, MonadState (get, put), StateT (runStateT), MonadIO (liftIO))
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Data.Foldable (for_)

main :: IO ()
main = do
    solution <- runMaybeT (solve input3)
    case solution of
        Nothing -> do
            log "No solutions were found!"
            exitFailure
        Just solution -> do
            printSolution solution

input :: Input
input = Input
    { boards = Map.fromList
        [ ('M', Board 3 (Arr.listArray (0, 8)
            [ Wall, Wall, Wall
            , Wall, Space, Space
            , Wall, Space, Space
            ]
          ))
        ]
    , initialState = BiMap.fromList [(Player, Coord 'M' (2, 2))]
    , requirements = Map.fromList [(Coord 'M' (1, 1), RequirePlayer)]
    }

input2 = Input
    { boards = Map.fromList
        [ ('A', Board 3 (Arr.listArray (0, 8)
            [ Space, Space, Wall
            , Wall, Wall, Wall
            , Wall, Wall, Wall
            ]
          ))
        , ('B', Board 3 (Arr.listArray (0, 8)
            [ Wall, Wall, Wall
            , Space, Space, Wall
            , Wall, Wall, Wall
            ]
          ))
        , ('C', Board 3 (Arr.listArray (0, 8)
            [ Wall, Wall, Wall
            , Wall, Wall, Space
            , Wall, Wall, Wall
            ]
          ))
        ]
      , initialState = BiMap.fromList
          [ (Player, Coord 'A' (0, 0))
          , (BoardPiece 'B', Coord 'A' (1, 0))
          , (BoardPiece 'C', Coord 'B' (0, 1))
          , (Block 1, Coord 'B' (1, 1))
          ]
      , requirements = Map.empty
    }

input3 = Input
    { boards = Map.fromList
        [ ('M', Board 9 (Arr.listArray (0, 80) (
            map (\case ' ' -> Space; 'X' -> Wall) $
                "XXXXXXXXX" ++
                "XX     XX" ++
                "XX     XX" ++
                "XX     XX" ++
                "XX     XX" ++
                "XX XXXXXX" ++
                "XX    XXX" ++
                "XX    XXX" ++
                "XXXXXXXXX"
          )))
        , ('G', Board 5 (Arr.listArray (0, 24) (
            map (\case ' ' -> Space; 'X' -> Wall) $
                "XXXXX" ++
                "XX   " ++
                "XX   " ++
                "XX  X" ++
                "XXXXX"
          )))
        , ('Y', Board 3 (Arr.listArray (0, 8) (
            map (\case ' ' -> Space; 'X' -> Wall) $
                "XXX" ++
                "  X" ++
                "X X"
          )))
        ]
    , initialState = BiMap.fromList
        [ (Player, Coord 'M' (6, 2))
        , (BoardPiece 'G', Coord 'M' (2, 2))
        , (BoardPiece 'Y', Coord 'M' (4, 3))
        , (Block 1, Coord 'M' (4, 2))
        ]
    , requirements = Map.fromList
        [ (Coord 'M' (4, 7), RequireNonPlayer)
        , (Coord 'M' (5, 7), RequireNonPlayer)
        , (Coord 'M' (5, 6), RequirePlayer)
        ]
    }

solve :: Input -> MaybeT IO Solution
solve Input { boards, initialState, requirements } =
    solve' [(initialState, [])] 0 (Set.singleton (canonicalGameStateStr initialState))
    where
        solve' states moves visitedStates = do
            liftIO . putStrLn $ "Moves: " ++ show moves ++ ", states: " ++ show (length states)
            let solutions = map (reverse . snd) $ filter (isSolved requirements . fst) states
            if not (null solutions) then
                return (head solutions)
            else if null states then
                mzero
            else do
                let newStates = concat do
                     (state, steps) <- states
                     for [Up, Down, Left, Right] \dir -> do
                         return (fromMaybe state (movePiece Player (BiMap.unsafeLookupA Player state) dir (boards, state)), dir : steps)
                let trulyNewStates = uniqueStates $ filter (not . (`Set.member` visitedStates) . canonicalGameStateStr . fst) newStates
                solve' trulyNewStates (moves + 1) (foldl' (flip (Set.insert . canonicalGameStateStr . fst)) visitedStates trulyNewStates)

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

movePiece :: Piece -> Coord -> Step -> (Boards, GameState) -> Maybe GameState
movePiece piece coord dir (boards, state) = do
    target <- targetCell dir coord (boards, state)
    when (lookupCellByCoord target boards == Wall) Nothing
    case BiMap.lookupB target state of
        Nothing -> Just (BiMap.insert (piece, target) state)
        Just inTheWay ->
            onPieceInTheWay piece inTheWay target dir (boards, state)

onPieceInTheWay
    :: Piece
    -> Piece
    -> Coord
    -> Step
    -> (Boards, GameState)
    -> Maybe (BiMap Piece Coord)
onPieceInTheWay piece inTheWay target dir (boards, state) =
    let
        push = do
            newState <- movePiece inTheWay target dir (boards, state)
            Just (BiMap.insert (piece, target) newState)
        goInto = do
            movePieceIntoAnother piece inTheWay dir (boards, state)
        eat = do
            newState <- movePieceIntoAnother inTheWay piece (opposite dir) (boards, state)
            Just (BiMap.insert (piece, target) newState)
    in push <|> goInto <|> eat

movePieceIntoAnother :: Piece -> Piece -> Step -> (Boards, GameState) -> Maybe GameState
movePieceIntoAnother piece into dir (boards, state) = do
    boardChar <- case into of
        BoardPiece boardChar -> Just boardChar
        _ -> Nothing
    let board = boards Map.! boardChar
    let (x, y) = getEntryCellXY dir board
    when (lookupCellByXY (x, y) board == Wall) Nothing
    let target = Coord boardChar (x, y)
    case BiMap.lookupB target state of
        Nothing -> Just (BiMap.insert (piece, target) state)
        Just inTheWay -> onPieceInTheWay piece inTheWay target dir (boards, state)


getEntryCellXY :: Step -> Board -> (Int, Int)
getEntryCellXY dir (Board w _) =
    case dir of
        Up -> (w `div` 2, w - 1)
        Down -> (w `div` 2, 0)
        Left -> (w - 1, w `div` 2)
        Right -> (0, w `div` 2)

targetCell :: Step -> Coord -> (Boards, GameState) -> Maybe Coord
targetCell dir coord (boards, state) =
    targetCell' coord Map.empty
    where
        targetCell' (Coord board (x, y)) visitedBoards
          | fromMaybe 0 (visitedBoards Map.!? board) > 1 = Nothing
          | outOfBounds (width (boards Map.! board)) newXY = do
                boardPieceCoord <- findPiece board state
                targetCell' boardPieceCoord (Map.alter (maybe (Just 1) (Just . (+1))) board visitedBoards)
          | otherwise =
                Just (Coord board newXY)
            where
                newXY = delta dir (x, y)

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
        ]

isSolved :: Requirements -> GameState -> Bool
isSolved requirements state =
    all requirementMet (Map.toList requirements)
    where
        requirementMet (coord, req) =
            case (BiMap.lookupB coord state, req) of
                (Just (Block _), RequireNonPlayer) -> True
                (Just (BoardPiece _), RequireNonPlayer) -> True
                (Just Player, RequirePlayer) -> True
                _ -> False

getBoardPieces :: [(Piece, Coord)] -> [(Char, Coord)]
getBoardPieces =
    sortOn fst . mapMaybe (\case (BoardPiece board, coord) -> Just (board, coord); _ -> Nothing)

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
    deriving (Eq, Ord)

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

indexToCoord :: Char -> Int -> Int -> Coord
indexToCoord board w i = Coord board (indexToXY w i)

xyToIndex :: Int -> (Int, Int) -> Int
xyToIndex w (x, y) = w * y + x

indexToXY :: Int -> Int -> (Int, Int)
indexToXY w i = (i `mod` w, i `div` w)

data Cell
    = Wall
    | Space
    deriving (Eq, Ord)

lookupCellByCoord :: Coord -> Boards -> Cell
lookupCellByCoord (Coord board (x, y)) boards =
    lookupCellByXY (x, y) (boards Map.! board)

lookupCellByXY :: (Int, Int) -> Board -> Cell
lookupCellByXY (x, y) (Board w cells) =
    cells Arr.! xyToIndex w (x, y)

class Monad m => CanLog m where
    log :: String -> m ()

instance CanLog IO where log = putStrLn
