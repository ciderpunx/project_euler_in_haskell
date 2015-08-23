-- By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum 
-- of the even-valued terms.

-- linear time, and quite magical 
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

-- for some reason list comprehensions weren't playing nicely, so I used a takeWhile to make sure eval was lazy
sumEvenFibsLessThan4Million = sum . takeWhile ( < 4000000) $ [f | f <- fibs, even f]

-- gives:
-- *Main> sumEvenFibsLessThan4Million 
-- 4613732

-- Useful reference at: http://www.haskell.org/haskellwiki/The_Fibonacci_sequence
