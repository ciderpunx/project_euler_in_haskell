--In England the currency is made up of pound, £, and pence, p, and there are eight coins in general circulation:
--
--    1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
--
--It is possible to make £2 in the following way:
--
--    1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
--
--How many different ways can £2 be made using any number of coins?
import Data.List

-- Let's be fancy and declare a type synonym
type Coin = Int

main :: IO ()
main = print answer

answer :: Int
answer = length $ genPart coins 200 200 

-- These are the denominations of Coins
coins :: [Coin]
coins = [1,2,5,10,20,50,100,200]

-- Given a list of coins, a target amount and a size, juggle the coin stack round
-- and call recursively to make a target that is smaller by h
genPart :: [Coin] -> Int -> Int -> [[Coin]]
genPart _  0 _ = [[]]
genPart _  _ 0 = []
genPart cs target size = [h:s | t <- init (tails cs)
                                , let h = head t
                                , h <= target
                                , s <- genPart t (target-h) (size-1) ]

--- original approach
--ps = 
--    [] : map parts [1..200]
--  where 
--    parts n = [n] : [x : p | x <- [1..200], p <- ps !! (n - x), x <= head p]
--answers = map (filter (all (`elem` coins))) ps
--answer = length $ answers !! 200
