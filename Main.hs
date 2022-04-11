{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Prelude hiding (Right, Left)
import qualified Data.Array as Arr
import Data.Array (Array)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import System.Exit (exitFailure, exitSuccess)
import Data.List (intercalate, sort, sortOn, foldl', uncons)
import Data.Bifunctor (Bifunctor(first, second))
import Data.Maybe (mapMaybe, fromMaybe)
import BiMap (BiMap)
import qualified BiMap
import Control.Monad (when, MonadPlus (mzero))
import Control.Applicative ((<|>), Alternative (empty))
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Traversable (for)
import Control.Monad.State (execStateT, MonadState (get, put), StateT (runStateT), MonadIO (liftIO), runState)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Data.Foldable (for_, Foldable (toList))
import ListT (ListT(ListT))
import qualified ListT
import Common
import Puzzles
import System.Environment (getArgs)
import System.IO (stdout, stderr, hPutStrLn, Handle)
import Data.Ratio (Ratio, (%))

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--help"] -> do
            help stdout
            exitSuccess
        [nth] -> do
            let n = read nth
            solvePuzzle (puzzles !! n)
        [] -> do
            solvePuzzle (last puzzles)
        _ -> do
            help stderr
            exitFailure

help :: Handle -> IO ()
help out =
    hPutStrLn out "Usage: PatricksParaboxSolver [nth puzzles to solve]"

solvePuzzle :: Input -> IO ()
solvePuzzle puzzle = do
    solution <- runMaybeT (solve puzzle)
    case solution of
        Nothing -> do
            putStrLn "No solutions were found!"
            exitFailure
        Just solution -> do
            printSolution solution

solve :: Input -> MaybeT IO Solution
solve Input { boards, initialState, requirements, flipped } =
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
                     newState <- ListT.fromFoldable (movePiece Player (BiMap.unsafeLookupA Player state) dir Map.empty (boards, flipped, state))
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

movePiece :: Piece -> Coord -> Step -> InMotion -> (Boards, FlippedPieces, GameState) -> Maybe GameState
movePiece piece coord dir inMotion (boards, flipped, state) = do
    case Map.lookup piece inMotion of
        Just moving ->
            if moving == dir then Just state -- Nothing needs to be done, non-conflicting looping movement
            else Nothing -- Conflicting loop found, fail
        Nothing -> do -- No loop found
            (target, relativeCoord) <- targetCell dir coord (boards, flipped, state)
            when (lookupCellByCoord target boards == Wall) Nothing
            case BiMap.lookupB target state of
                Nothing -> Just (BiMap.insert (piece, target) state)
                Just inTheWay ->
                    onPieceInTheWay piece inTheWay target dir inMotion relativeCoord Set.empty (boards, flipped, state)

onPieceInTheWay
    :: Piece
    -> Piece
    -> Coord
    -> Step
    -> InMotion
    -> RelativeCoord
    -> BeingEntered
    -> (Boards, FlippedPieces, GameState)
    -> Maybe (BiMap Piece Coord)
onPieceInTheWay piece inTheWay target dir inMotion relativeCoord beingEntered (boards, flipped, state) =
    let
        newInMotion = Map.insert piece dir inMotion
        push = do
            newState <- movePiece inTheWay target dir newInMotion (boards, flipped, state)
            Just (BiMap.insert (piece, target) newState)
        goInto = do
            movePieceIntoAnother piece inTheWay dir newInMotion beingEntered relativeCoord (boards, flipped, state)
        eat = do
            newState <- movePieceIntoAnother inTheWay piece (opposite dir) newInMotion Set.empty (1 % 2) (boards, flipped, state)
            Just (BiMap.insert (piece, target) newState)
    in push <|> goInto <|> eat

type BeingEntered = Set Piece

movePieceIntoAnother :: Piece -> Piece -> Step -> InMotion -> BeingEntered -> RelativeCoord -> (Boards, FlippedPieces, GameState) -> Maybe GameState
movePieceIntoAnother piece into dir inMotion beingEntered relativeCoord (boards, flipped, state) = do
    when (Set.member into beingEntered) Nothing
    boardChar <- case into of
        BoardPiece boardChar -> Just boardChar
        Clone boardChar _ -> Just boardChar
        _ -> Nothing
    let enterDir = flipIfNeeded flipped into dir
    let board = boards Map.! boardChar
    let ((x, y), newRelativeCoord) = getEntryCellXY enterDir board relativeCoord
    when (lookupCellByXY (x, y) board == Wall) Nothing
    let target = Coord boardChar (x, y)
    case BiMap.lookupB target state of
        Nothing -> Just (BiMap.insert (piece, target) state)
        Just inTheWay -> onPieceInTheWay piece inTheWay target enterDir inMotion newRelativeCoord newBeingEntered (boards, flipped, state)
    where
        newBeingEntered = Set.insert into beingEntered

-- Figure out which cell to go into when entering a board piece
getEntryCellXY :: Step -> Board -> RelativeCoord -> ((Int, Int), RelativeCoord)
getEntryCellXY dir (Board w _) relativeCoord =
    let (offset, remainder) = relativeCoord `ratioDivMod` (1 % w)
    in case dir of
        Up -> ((offset, w - 1), remainder * fromIntegral w)
        Down -> ((offset, 0), remainder * fromIntegral w)
        Left -> if remainder == 0
            then ((w - 1, offset - 1), 1)
            else ((w - 1, offset), remainder * fromIntegral w)
        Right -> if remainder == 0
            then ((0, offset - 1), 1)
            else ((0, offset), remainder * fromIntegral w)

flipIfNeeded :: FlippedPieces -> Piece -> Step -> Step
flipIfNeeded flipped piece dir =
    case Map.lookup piece flipped of
        Nothing -> dir
        Just FlippedBoth -> opposite dir
        Just FlippedHorizontal ->
            case dir of
                Left -> Right
                Right -> Left
                _ -> dir
        Just FlippedVertical ->
            case dir of
                Up -> Down
                Down -> Up
                _ -> dir


type Target = (Coord, RelativeCoord)

-- This should always be a value gte 0 and lte 1
-- Represents the location in the space being entered corresponding to the location of the center of the block being moved.
type RelativeCoord = Ratio Int

targetCell :: Step -> Coord -> (Boards, FlippedPieces, GameState) -> Maybe Target
targetCell dir coord (boards, flipped, state) =
    targetCell' dir coord Set.empty (1 % 2)
    where
        targetCell' dir (Coord boardChar (x, y)) boardsExited relativeCoord
          | outOfBounds (width board) newXY = do
                when (Set.member boardChar boardsExited) Nothing
                boardPieceCoord <- findPiece boardChar state
                targetCell' exitDir boardPieceCoord (Set.insert boardChar boardsExited) newRelativeCoord
          | otherwise =
                Just (Coord boardChar newXY, relativeCoord)
            where
                board = boards Map.! boardChar
                exitDir = flipIfNeeded flipped (BoardPiece boardChar) dir
                newXY = delta dir (x, y)
                newRelativeCoord = case dir of
                    Up -> f x
                    Down -> f x
                    Left -> f y
                    Right -> f y
                    where
                        f offset = (fromIntegral offset + relativeCoord) / fromIntegral (width (boards Map.! boardChar))


outOfBounds :: Int -> (Int, Int) -> Bool
outOfBounds w (x, y) = x < 0 || y < 0 || x >= w || y >= w

-- Like divMod but works with fractional
ratioDivMod :: RealFrac n => n -> n -> (Int, n)
ratioDivMod n m =
    let dividend = floor (n / m)
        remainder = n - fromIntegral dividend * m
    in (dividend, remainder)

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
                Map.alter (Just . maybe [coord] (coord :)) c map
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
