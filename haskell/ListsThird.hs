module ListsThird
  (
    myInsertAt
  , myRange
  , rnd_selectIO
  ) where

import System.Random

import ListsFirst
import ListsSecond

myInsertAt :: a -> [a] -> Int -> [a]
myInsertAt x xs n
  | length xs < n = error "Too large index"
  | n == 0        = x : xs
  | otherwise     = head xs : myInsertAt x (tail xs) (n - 1)

myRange :: Int -> Int -> [Int]
myRange a b
  | a == b    = [a]
  | a > b     = a : myRange (a - 1) b
  | otherwise = a : myRange (a + 1) b

rnd_select :: RandomGen g => [a] -> Int -> g -> ([a], g)
rnd_select _ 0 gen = ([], gen)
rnd_select [] _ gen = ([], gen)
rnd_select l count gen
   | count == (length l) = (l, gen)
   | otherwise           =  rnd_select (myRemove' l (k+1)) count gen'
                            where (k, gen') =
                                    randomR (0, (length l) - 1) gen

rnd_selectIO :: [a] -> Int -> IO [a]
rnd_selectIO l count = getStdRandom $ rnd_select l count
