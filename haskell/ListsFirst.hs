module ListsFirst
  (
    myLast
  , myButLast
  , myElementAt
  , myLength
  , myReverse
  , isPalindrome
  , NestedList(..)
  , myFlatten
  , myCompress
  , myPack
  , myEncode
  ) where

myLast :: [a] -> a
myLast [] = error "myLast on empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

myButLast :: [a] -> a
myButLast = myLast . init

myElementAt :: [a] -> Int -> a
myElementAt xs n = xs !! (n - 1)

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [x, y] = x == y
isPalindrome [x, _, z] = x == z
isPalindrome xs = head xs == last xs && isPalindrome (init $ tail xs)

data NestedList a = Elem a | List [NestedList a] deriving Show
myFlatten :: NestedList a -> [a]
myFlatten (Elem x) = [x]
myFlatten (List (x:xs)) = myFlatten x ++ myFlatten (List xs)
myFlatten (List []) = []

myCompress :: (Eq a) => [a] -> [a]
myCompress [] = []
myCompress [x] = [x]
myCompress (x:xs)
    | x == head xs = myCompress xs
    | otherwise = x : myCompress xs

myPack :: (Eq a) => [a] -> [[a]]
myPack [] = []
myPack [x] = [[x]]
myPack (x:xs) = (x : takeWhile (== x) xs) : myPack (dropWhile (== x) xs)

myEncode :: (Eq a) => [a] -> [(Int, a)]
myEncode [] = []
myEncode xs = map (\x -> (length x, head x)) (myPack xs)
