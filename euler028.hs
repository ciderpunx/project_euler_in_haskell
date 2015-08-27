--Starting with the number 1 and moving to the right in a clockwise direction a 5 by 5 spiral is formed as follows:
--
--21 22 23 24 25
--20  7  8  9 10
--19  6  1  2 11
--18  5  4  3 12
--17 16 15 14 13
--
--It can be verified that the sum of the numbers on the diagonals is 101.
--
--What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?
main :: IO()
main = print answer

answer :: Int
answer = sum $ pois gaps spiral 4

-- Construct our 'spiral' we know that it must contain 1001 rows and cols
-- Thus all numbers from 1..1001^2 
spiral :: [Int]
spiral = [1..1001^2]

-- Gaps between numbers are a simple progression 1,3,5,7...
-- maybe approach this with 
-- concatMap (take 4 . repeat) [1,3..1001-2]
-- (because we know that the longest side is 1001 and has 2 POIs on it)
-- If we could do maths on these then we could avoid having to construc 
-- the spiral at all
gaps :: [Int]
gaps   = [1,3..]

-- POIs (for points of interest) Go through the spiral taking 
-- first point then next 3 (with gaps of 1 between them), 
-- then next 4 with gaps of 3, 4 after that with gaps of 5
-- and so on
pois :: [Int] -> [Int] -> Int -> [Int]
pois [] _ _ = [] -- could never happen but lets avoid compiler warning!
pois _ [] _ = []
pois gss@(g:gs) xss@(x:xs) y =
  if y == 0 
    then pois gs xss 4
    else x : pois gss (drop g xs) (y-1)
