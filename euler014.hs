--The following iterative sequence is defined for the set of positive integers:
--
--n → n/2 (n is even)
--n → 3n + 1 (n is odd)
--
--Using the rule above and starting with 13, we generate the following sequence:
--13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
--
--It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.
--
--Which starting number, under one million, produces the longest chain?
--
-- 837799, 524 length chain
--
--NOTE: Once the chain starts the terms are allowed to go above one million.
import Data.List
import Data.Array
import Data.Ord (comparing)

main1 :: IO ()
main1 = print . head $ longestChainTo 1000000

-- fairly slow unless you compile. 
longestChainTo :: Int -> [Int]
longestChainTo n = 
		maximumBy (comparing length) $ map chainOf [1..n]

chainOf :: Int -> [Int]
chainOf n 
		| n == 1					= [1]
		| n `mod` 2 == 0	= n:chainOf (n `div` 2)
		| otherwise       = n:chainOf (3 * n + 1)


-- Could memoize with an array for performance (from the wiki)
syrs :: Int -> Array Int Int
syrs n = a
  where 
    a = listArray (1,n) $ 0:[1 + syr n x | x <- [2..n]]
    syr n' x = 
        if x' <= n' then a ! x' else 1 + syr n' x'
					where 
						x' = if even x then x `div` 2 else 3 * x + 1

main2 :: IO ()
main2 = 
    print $ maximumBy (comparing snd) $ assocs $ syrs 1000000

main = main2
