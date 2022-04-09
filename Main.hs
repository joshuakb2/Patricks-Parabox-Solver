{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}

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
import Common
import Puzzles

main :: IO ()
main = do
    solution <- runMaybeT (solve (last puzzles))
    case solution of
        Nothing -> do
            putStrLn "No solutions were found!"
            exitFailure
        Just solution -> do
            printSolution solution

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
onPieceInTheWay piece inTheWay target dir inMotion boardPiecesExitedCoords (boards, state) =
    let
        newInMotion = Map.insert piece dir inMotion
        push = do
            newState <- movePiece inTheWay target dir newInMotion (boards, state)
            Just (BiMap.insert (piece, target) newState)
        goInto = do
            movePieceIntoAnother piece inTheWay dir newInMotion boardPiecesExitedCoords (boards, state)
        eat = do
            newState <- movePieceIntoAnother inTheWay piece (opposite dir) newInMotion [] (boards, state)
            Just (BiMap.insert (piece, target) newState)
    in push <|> goInto <|> eat

movePieceIntoAnother :: Piece -> Piece -> Step -> InMotion -> [(Int, Int)] -> (Boards, GameState) -> Maybe GameState
movePieceIntoAnother piece into dir inMotion boardPiecesExitedCoords (boards, state) = do
    let (getTargetCellXY, remainingBoardPiecesExitedCoords) =
         case uncons boardPiecesExitedCoords of
             Nothing -> (getEntryCellXY, [])
             Just (ourCoord, rest) -> (getTransferCellXY ourCoord, rest)
    boardChar <- case into of
        BoardPiece boardChar -> Just boardChar
        Clone boardChar _ -> Just boardChar
        _ -> Nothing
    let board = boards Map.! boardChar
    let (x, y) = getTargetCellXY dir board
    when (lookupCellByXY (x, y) board == Wall) Nothing
    let target = Coord boardChar (x, y)
    case BiMap.lookupB target state of
        Nothing -> Just (BiMap.insert (piece, target) state)
        Just inTheWay -> onPieceInTheWay piece inTheWay target dir inMotion remainingBoardPiecesExitedCoords (boards, state)

-- When we are shinking to go into another piece
getEntryCellXY :: Step -> Board -> (Int, Int)
getEntryCellXY dir (Board w _) =
    case dir of
        Up -> (w `div` 2, w - 1)
        Down -> (w `div` 2, 0)
        Left -> (w - 1, w `div` 2)
        Right -> (0, w `div` 2)

-- When we are growing or staying the same size to go into another piece
getTransferCellXY :: (Int, Int) -> Step -> Board -> (Int, Int)
getTransferCellXY (x, y) dir (Board w _) =
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

isBlock :: Piece -> Bool
isBlock (Block _) = True
isBlock _ = False

xyToIndex :: Int -> (Int, Int) -> Int
xyToIndex w (x, y) = w * y + x

lookupCellByCoord :: Coord -> Boards -> Cell
lookupCellByCoord (Coord board (x, y)) boards =
    lookupCellByXY (x, y) (boards Map.! board)

lookupCellByXY :: (Int, Int) -> Board -> Cell
lookupCellByXY (x, y) (Board w cells) =
    cells Arr.! xyToIndex w (x, y)
