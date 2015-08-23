-- 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without 
-- any remainder.

-- What is the smallest positive number that is evenly divisible by all of the numbers from 1 
-- to 20?

-- lcm is what we need for this
listLCM xs =  foldr (lcm) 2 xs 

ans = listLCM [3..20]
