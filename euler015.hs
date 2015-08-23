-- Starting in the top left corner of a 2×2 grid, and only being able to move to the right 
-- and down, there are exactly 6 routes to the bottom right corner.

-- How many such routes are there through a 20×20 grid?

-- Well this is just combinatorics ... 
-- * All paths are 20x20 == 40 long, you have to go right 20 and down 20 however you go
-- * Think about the moves as a series of r and d moves
-- * Each possible series of moves must have a unique set of 20 positions of r and of d moves
-- * So this is a combination without repetition. The formula is
--		   n!
--		--------
--		r!(n-r!)
--   
--    With r as 20 (the positions of the down moves) and n as the total length of the series 
--    of moves -- 40
--
--    137846528820

moves :: Integer
moves = 
		(fac n) `div` ((fac r)*(fac (n-r)))
	where
		n = 40
		r = 20

fac :: Integer -> Integer
fac n = product [1..n]
