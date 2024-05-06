{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW2 where

import Data.List (find, foldl', or)
import Prelude (Bool (..), Bounded (..), Char, Either (..), Enum (..), Eq (..), Int, Integer, Maybe (..), Num (..), Ord (..), Show (..), String, all, and, any, concat, concatMap, const, curry, div, elem, error, even, filter, flip, foldl, foldr, fst, id, length, lines, lookup, map, mod, not, notElem, null, odd, otherwise, product, snd, sum, uncurry, undefined, unlines, unwords, words, (!!), ($), (&&), (++), (.), (||))

------------------------------------------------
-- DO NOT MODIFY ANYTHING ABOVE THIS LINE !!! --
------------------------------------------------

-- Section 1.1: Basic Maybes
concatMaybeMap :: (a -> Maybe b) -> Maybe a -> Maybe b
concatMaybeMap f (Just a)  =  f a
concatMaybeMap _ Nothing  =  Nothing
fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just  b) = b
fromMaybe a Nothing   = a
maybe :: b -> (a -> b) -> Maybe a -> b
maybe _ f (Just a) =  f a
maybe b _ Nothing  =  b
catMaybes :: [Maybe a] -> [a]
catMaybes [] = [] 
catMaybes (Just x : xs) = x : catMaybes xs
catMaybes (Nothing : xs) = catMaybes xs
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (x : xs) = case f x of
    Just y  -> y : mapMaybe f xs
    Nothing -> mapMaybe f xs


-- Section 1.2 Basic Eithers
concatEitherMap :: (a -> Either e b) -> Either e a -> Either e b
concatEitherMap f x = case x of
    Left e     -> Left e
    Right a   -> f a

either :: (a -> c) -> (b -> c) -> Either a b -> c
either f g x = case x of
    Left a -> f a
    Right b -> g b

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f x = case x of
    Left a -> Left (f a)
    Right b -> Right b

catEithers :: [Either e a] -> Either e [a]
catEithers []  = Right []
catEithers (x : xs) = case x of
    Left e -> Left e
    Right a -> case catEithers xs of
        Right as -> Right  (a : as)
        Left e -> Left e

mapEither :: (a -> Either e b) -> [a] -> Either e [b]
mapEither f xs =  catEithers $ map f xs

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers [] = ([], [])
partitionEithers (x : xs) = 
    let (as, bs) = partitionEithers xs
    in case x of
           Left a -> (a : as, bs)
           Right b -> (as, b : bs)

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe x = case x of
    Left _ -> Nothing
    Right b -> Just b

-- Section 2: Lists
take :: Int -> [a] -> [a]
take _ [] = []
take n (_ : _) | n <= 0 = []
take n (x : xs) = x : take (n-1) xs
  
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile f (x : xs) = if f x then x : takeWhile f xs else []

drop :: Int -> [a] -> [a]
drop _ [] = []
drop 0 xs = xs
drop n (_ : xs) = drop (n-1) xs

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile f (x : xs) = if f x then dropWhile f xs else x : xs

reverse :: [a] -> [a]
reverse [] = []
reverse xs = foldr (\c acc -> acc ++ [c]) [] xs

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs | n <= 0 = xs
rotate n xs = rotate (n-1) ((drop ((length xs) - 1) xs) ++ (take ((length xs) - 1) xs))

lotate :: Int -> [a] -> [a]
lotate _ [] = []
lotate n xs | n <= 0 = xs
lotate n xs = lotate (n-1) ((drop 1 xs) ++ (take 1 xs))

type Generator a = (a -> a, a -> Bool, a)
fromGenerator :: Generator a -> [a]
fromGenerator (f, p, a) = if p a then f a : fromGenerator(f, p, f a) else [] 

replicate :: Int -> a -> [a]
replicate n _ | n <= 0 = []
replicate n a = a : replicate (n-1) a 

inits :: [a] -> [[a]]
inits = undefined
tails :: [a] -> [[a]]
tails = undefined

-- Section 3: zips and products
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith = undefined
zip :: [a] -> [b] -> [(a, b)]
zip = undefined
zipFill :: a -> b -> [a] -> [b] -> [(a, b)]
zipFill = undefined
data ZipFail = ErrorFirst | ErrorSecond deriving (Eq, Show)
zipFail :: [a] -> [b] -> Either ZipFail [(a, b)]
zipFail = undefined
unzip :: [(a, b)] -> ([a], [b])
unzip = undefined

-- Section 4: Knight travels
data KnightPos = KnightPos {x :: Int, y :: Int} deriving (Show, Eq)
data KnightMove = TopLeft | TopRight | RightTop | RightBottom | BottomRight | BottomLeft | LeftBottom | LeftTop deriving (Enum, Bounded, Show, Eq)
allKnightMoves :: [KnightMove]
allKnightMoves = undefined
data Board = Board {width :: Int, height :: Int} deriving (Show, Eq)
tour :: Board -> KnightPos -> Maybe [KnightMove]
tour = undefined
newtype InvalidPosition = InvalidPosition KnightPos deriving (Show, Eq)
translate :: KnightPos -> [KnightMove] -> [KnightPos]
translate = undefined
translate' :: [KnightPos] -> Either InvalidPosition [KnightMove]
translate' = undefined

-- Bonus (10 points)
mark :: Board -> [KnightPos] -> Either InvalidPosition [[Int]]
mark = undefined
