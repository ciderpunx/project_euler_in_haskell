import Data.List

-- A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400
divides y x = 
		if x `mod` y == 0 then True else False
febLength yr = 
		if 400 `divides` yr || (4 `divides` yr && (not (100 `divides` yr))) 
		then 29
		else 28
monthLengths yr = [31, (febLength yr), 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
allMonthLengths = concatMap monthLengths [1900..2000]
firstDay x y    = (x + y) `mod` 7
firstDaysFrom1900       
								= scanl firstDay 1 allMonthLengths
firstDaysFrom1901 
								= drop 12 firstDaysFrom1900 
firstDaysOfMondayFrom1901
								= filter (==0) firstDaysFrom1901
answer          = length firstDaysOfMondayFrom1901
