module ListsThird
  (
    myInsertAt
  , myRange
  , rndSelectIO
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

rndSelect :: RandomGen g => [a] -> Int -> g -> ([a], g)
rndSelect _ 0 gen = ([], gen)
rndSelect [] _ gen = ([], gen)
rndSelect l count gen
   | count == length l = (l, gen)
   | otherwise           =  rndSelect (myRemove' l (k+1)) count gen'
                            where (k, gen') =
                                    randomR (0, length l - 1) gen

rndSelectIO :: [a] -> Int -> IO [a]
rndSelectIO l count = getStdRandom $ rndSelect l count
