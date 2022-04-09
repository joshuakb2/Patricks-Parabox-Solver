{-# LANGUAGE LambdaCase #-}
module Puzzles (Input, puzzles) where

import Common
import qualified Data.Map.Strict as Map
import qualified BiMap
import qualified Data.Array as Arr

puzzles :: [Input]
puzzles =
    [ Input
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

    , Input
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

    , Input
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

    , Input
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

    , Input
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

    , Input
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

    , Input
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

    , Input
        { boards = Map.fromList
            [ ('B', makeBoard 9
                [ "XXXXXXXXX"
                , "  XXXXXXX"
                , "X       X"
                , "X       X"
                , "X       X"
                , "X       X"
                , "XXX     X"
                , "  X     X"
                , "XXXXXXXXX"
                ]
            )
            , ('G', makeBoard 9
                [ "XXXXXXXXX"
                , "XXXXXX   "
                , "XXXXXX   "
                , "XXXXXX   "
                , "XXXXXX   "
                , "XXXXXX   "
                , "XXXXXX   "
                , "XXXXXX   "
                , "XXXXXXXXX"
                ]
            )
            ]
        , initialState = BiMap.fromList
            [ (Player, Coord 'B' (4, 6))
            , (BoardPiece 'B', Coord 'B' (6, 4))
            , (BoardPiece 'G', Coord 'B' (4, 4))
            ]
        , requirements = Map.fromList [(Coord 'B' (1, 7), RequirePlayer)]
        }

    , Input
        { boards = Map.fromList
            [ ('M', makeBoard 7
                [ "XXXXXXX"
                , "X     X"
                , "X     X"
                , "X     X"
                , "X     X"
                , "X     X"
                , "XXXXXXX"
                ]
              )
            , ('B', makeBoard 5
                [ "XXXXX"
                , " XXXX"
                , " XXXX"
                , "XXXXX"
                , "XXXXX"
                ]
              )
            , ('G', makeBoard 5
                [ "XXXXX"
                , "XXX  "
                , "XXXXX"
                , "XXXXX"
                , "XXXXX"
                ]
              )
            ]
        , initialState = BiMap.fromList
            [ (Player, Coord 'M' (2, 2))
            , (BoardPiece 'B', Coord 'M' (4, 2))
            , (Clone 'B' 1, Coord 'M' (4, 4))
            , (BoardPiece 'G', Coord 'M' (2, 4))
            ]
        , requirements = Map.fromList
            [ (Coord 'G' (3, 1), RequirePlayer)
            ]
        }
    ]

charsToCells :: String -> [Cell]
charsToCells =
    map (\case ' ' -> Space; 'X' -> Wall; _ -> error "Invalid board string")

makeBoard :: Int -> [String] -> Board
makeBoard w rows =
    Board w (Arr.listArray (0, w ^ 2 - 1) (charsToCells (concat rows)))