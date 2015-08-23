-- A permutation is an ordered arrangement of objects. For example, 3124 is one possible permutation of 
-- the digits 1, 2, 3 and 4. If all of the permutations are listed numerically or alphabetically, we call 
-- it lexicographic order. The lexicographic permutations of 0, 1 and 2 are:
--   012   021   102   120   201   210
-- What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
import Data.List (permutations, sort,delete)

psort :: [Integer]
psort = concat . take 1 . drop 999999 . sort $ permutations [0..9]

-- alternatively
answer :: [Integer]
answer = (!! 999999) $ sort $ permutations [0..9]

integerAnswer :: Integer
integerAnswer = read $ concatMap show answer

main :: IO ()
main = print integerAnswer 


-- The Haskell Wiki has this implementation which is clever but that I don't quite get yet.
-- It is also fast. Like really fast.
fac :: Int -> Int
fac n = product [1..n]

perms :: Eq a => [a] -> Int -> [a]
perms [] _= []
perms xs n= x : perms (delete x xs) (mod n m)
  where m = fac $ length xs - 1
        y = div n m
        x = xs!!y
 
problem24 :: String
problem24 = perms "0123456789" 999999
