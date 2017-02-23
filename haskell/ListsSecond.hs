module ListsSecond
  (
    MaybePair(..)
  , myEncodeModified
  , myDecodeModified
  -- , myEncodeDirect
  , myDuplicate
  , myReplicate
  , myDropEvery
  , mySplit
  , mySlice
  -- , myRotate
  , myRemove
  , myRemove'
  ) where

import ListsFirst

data MaybePair a = Single a | Multiple Int a deriving Show
myEncodeModified :: (Eq a) => [a] -> [MaybePair a]
myEncodeModified = map convertMaybePair . myEncode
    where
      convertMaybePair (1, x) = Single x
      convertMaybePair (n, x) = Multiple n x

myDecodeModified :: (Eq a) => [MaybePair a] -> [a]
myDecodeModified = concatMap decodeHelp
    where
      decodeHelp (Single x) = [x]
      decodeHelp (Multiple n x) = replicate n x

-- myEncodeDirect :: (Eq a) => [a] -> [MaybePair a]

myDuplicate :: [a] -> [a]
myDuplicate = concatMap (replicate 2)

myReplicate :: [a] -> Int -> [a]
myReplicate xs n = concatMap (replicate n) xs

myDropEvery :: [a] -> Int -> [a]
myDropEvery xs n
  | length xs < n = xs
  | otherwise     = take (n - 1) xs ++ myDropEvery (drop n xs) n

mySplit :: [a] -> Int -> ([a], [a])
mySplit xs n
  | length xs < n = (xs, [])
  | otherwise     = (take n xs, drop n xs)

mySlice :: [a] -> Int -> Int -> [a]
mySlice xs n m = drop (n - 1) (take m xs)

-- myRotate :: [a] -> Int -> [a]
-- myRotate xs n =

myRemove :: [a] -> Int -> (a, [a])
myRemove xs n
  | length xs < n = error "Index too large"
  | otherwise     = (xs !! n, (take n xs) ++ (drop (n + 1) xs))

myRemove' :: [a] -> Int -> [a]
myRemove' xs n
  | length xs < n = error "Index too large"
  | otherwise     = (take n xs) ++ (drop (n + 1) xs)
