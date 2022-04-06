{-# LANGUAGE NamedFieldPuns #-}

module BiMap (
    BiMap,
    null,
    size,
    memberA,
    memberB,
    lookupA,
    lookupB,
    unsafeLookupA,
    unsafeLookupB,
    empty,
    singleton,
    insert,
    deleteA,
    deleteB,
    toList,
    fromList
) where

import Prelude hiding (null)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Foldable (foldl')

data BiMap a b = BiMap { toA :: Map b a, toB :: Map a b }

null :: BiMap a b -> Bool
null BiMap { toA } = Map.null toA

size :: BiMap a b -> Int
size BiMap { toA } = Map.size toA

memberA :: Ord a => a -> BiMap a b -> Bool
memberA a BiMap { toB } = Map.member a toB

memberB :: Ord b => b -> BiMap a b -> Bool
memberB b BiMap { toA } = Map.member b toA

lookupA :: Ord a => a -> BiMap a b -> Maybe b
lookupA a BiMap { toB } = Map.lookup a toB

lookupB :: Ord b => b -> BiMap a b -> Maybe a
lookupB b BiMap { toA } = Map.lookup b toA

unsafeLookupA :: Ord a => a -> BiMap a b -> b
unsafeLookupA a BiMap { toB } = toB Map.! a

unsafeLookupB :: Ord b => b -> BiMap a b -> a
unsafeLookupB b BiMap { toA } = toA Map.! b

empty :: BiMap a b
empty = BiMap Map.empty Map.empty

singleton :: a -> b -> BiMap a b
singleton a b = BiMap (Map.singleton b a) (Map.singleton a b)

insert :: (Ord a, Ord b) => (a, b) -> BiMap a b -> BiMap a b
insert (a, b) BiMap { toA, toB } =
    BiMap newToA newToB
    where
        -- Delete any old mappings between a, b, and anything else, and add the new mapping
        newToA = Map.insert b a . maybe id Map.delete (toB Map.!? a) $ toA
        newToB = Map.insert a b . maybe id Map.delete (toA Map.!? b) $ toB

deleteA :: (Ord a, Ord b) => a -> BiMap a b -> BiMap a b
deleteA a BiMap { toA, toB } =
    BiMap newToA newToB
    where
        newToB = Map.delete a toB
        newToA = maybe toA (`Map.delete` toA) (Map.lookup a toB)

deleteB :: (Ord a, Ord b) => b -> BiMap a b -> BiMap a b
deleteB b BiMap { toA, toB } =
    BiMap newToA newToB
    where
        newToA = Map.delete b toA
        newToB = maybe toB (`Map.delete` toB) (Map.lookup b toA)

toList :: BiMap a b -> [(a, b)]
toList BiMap { toB } = Map.toList toB

fromList :: (Ord a, Ord b) => [(a, b)] -> BiMap a b
fromList =
    foldl' (flip insert) empty
