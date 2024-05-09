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
inits xs = inits' xs []
  where
    inits' [] a = [a] 
    inits' (y:ys) a = a : inits' ys (a ++ [y])

tails :: [a] -> [[a]]
tails [] = [[]]  
tails xs = xs : tails (drop 1 xs)

-- Section 3: zips and products
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (a : as) (b : bs) = f a b : zipWith f as bs

zip :: [a] -> [b] -> [(a, b)]
zip _ [] = []
zip [] _ = []
zip (a : as) (b : bs) = (a,b) : zip as bs 

zipFill :: a -> b -> [a] -> [b] -> [(a, b)]
zipFill _ _ [] [] = []
zipFill x y (a : as) [] = (a, y) : zipFill x y as []
zipFill x y [] (b : bs) = (x,b) : zipFill x y [] bs
zipFill x y (a : as) (b : bs) = (a, b) : zipFill x y as bs

data ZipFail = ErrorFirst | ErrorSecond deriving (Eq, Show)
zipFail :: [a] -> [b] -> Either ZipFail [(a, b)]
zipFail [] [] = Right []
zipFail (_ : _) [] = Left ErrorSecond
zipFail [] (_ : _) = Left ErrorFirst
zipFail (x : xs) (y : ys) = 
    case zipFail xs ys of
        Left e -> Left e
        Right ps -> Right ((x, y) : ps)

unzip :: [(a, b)] -> ([a], [b])
unzip [] = ([],[])
unzip ((x,y) : xys) = 
    let (xs, ys) = unzip xys  
    in (x : xs, y : ys) 


-- Section 4: Knight travels
-- Position (0, 0) is the top-left corner.
data KnightPos = KnightPos {x :: Int, y :: Int} deriving (Show, Eq)
data KnightMove = TopLeft | TopRight | RightTop | RightBottom | BottomRight | BottomLeft | LeftBottom | LeftTop deriving (Enum, Bounded, Show, Eq)
-- Utility to get all knight moves. Don't worry about the implementation of this.
allKnightMoves :: [KnightMove]
allKnightMoves = [minBound .. maxBound]
data Board = Board {width :: Int, height :: Int} deriving (Show, Eq)
tour :: Board -> KnightPos -> Maybe [KnightMove]
tour board start = findTour board start [start] []

findTour :: Board -> KnightPos -> [KnightPos] -> [KnightMove] -> Maybe [KnightMove]
findTour board pos visited moves
    | length visited == (width board * height board) = Just $ reverse moves
    | otherwise = findFirstValidMove (possibleMoves board pos) visited moves
  where
    findFirstValidMove :: [KnightMove] -> [KnightPos] -> [KnightMove] -> Maybe [KnightMove]
    findFirstValidMove [] _ _ = Nothing 
    findFirstValidMove (m:ms) vsPos listMoves =
        let newPos = moveKnight pos m
        in if newPos `notElem` vsPos && isValidPosition board newPos
           then case findTour board newPos (newPos : vsPos) (m : listMoves) of
                Just l -> Just l
                Nothing -> findFirstValidMove ms vsPos listMoves
           else findFirstValidMove ms vsPos listMoves

newtype InvalidPosition = InvalidPosition KnightPos deriving (Show, Eq)
translate :: KnightPos -> [KnightMove] -> [KnightPos]
translate _ [] = []
translate knightPos (x : xs) = moveKnight knightPos x : translate (moveKnight knightPos x) xs
translate' :: [KnightPos] -> Either InvalidPosition [KnightMove]
translate' [] = Right []
translate' [_] = Right []
translate' (x1 : x2 : xs) = 
    case isValidMove x1 x2 of
        Just m -> case translate' (x2 : xs) of
            Left e -> Left e
            Right ms -> Right (m:ms)
        Nothing -> Left $ InvalidPosition x2

possibleMoves :: Board -> KnightPos -> [KnightMove]
possibleMoves board pos = filter (isValidPosition board . moveKnight pos) allKnightMoves

isValidPosition :: Board -> KnightPos -> Bool
isValidPosition (Board w h) (KnightPos x y) = x >= 0 && x < w && y >= 0 && y < h    

isValidMove :: KnightPos -> KnightPos -> Maybe KnightMove
isValidMove (KnightPos x1 y1) (KnightPos x2 y2) = 
    lookup (x2 - x1, y2 - y1) [((-2,-1) , TopLeft), ((2,-1) , TopRight), ((1,-2), RightTop), ((1,2), RightBottom), ((2,1), BottomRight), ((-2,1), BottomLeft), ((-1,2), LeftBottom), ((-1,-2), LeftTop)]

moveKnight :: KnightPos -> KnightMove -> KnightPos
moveKnight (KnightPos x y) m = case m of
    TopLeft      -> KnightPos (x - 2) (y - 1)
    TopRight     -> KnightPos (x + 2) (y - 1)
    RightTop     -> KnightPos (x + 1) (y - 2)
    RightBottom  -> KnightPos (x + 1) (y + 2)
    BottomRight  -> KnightPos (x + 2) (y + 1)
    BottomLeft   -> KnightPos (x - 2) (y + 1)
    LeftBottom   -> KnightPos (x - 1) (y + 2)
    LeftTop      -> KnightPos (x - 1) (y - 2)

-- Bonus (10 points)
mark :: Board -> [KnightPos] -> Either InvalidPosition [[Int]]
mark = undefined
