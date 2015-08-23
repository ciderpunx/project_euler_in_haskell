-- If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 
-- 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
-- 
-- If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how 
-- many letters would be used?
-- 
-- NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 
-- 23 letters and 115 (one hundred and fifteen) contains 20 letters. 
-- The use of "and" when writing out numbers is in compliance with British usage.

-- I came up with a pretty boring solution using the following table. At least it is clear I 
-- guess.

-- one          3   -- ten          3   -- twenty       6
-- two          3   -- eleven       6   -- thirty       6
-- three        5   -- twelve       6   -- forty        5
-- four         4   -- thirteen     8   -- fifty        5
-- five         4   -- fourteen     8   -- sixty        5
-- six          3   -- fifteen      7   -- seventy      7
-- seven        5   -- sixteen      7   -- eighty       6
-- eight        5   -- seventeen    9   -- ninety       6
-- nine         4   -- eighteen     8   -- hundred      7
                    -- nineteen     8   -- hundred-and   10
                                        -- one-thousand  11
import Data.Char

main :: IO ()
main = print . sum $ map charsInIntAsWord [1..1000]

charsInIntAsWord :: Int -> Int
charsInIntAsWord = count . map digitToInt . show

teens :: Int -> Int
teens t
	| t==0             = 0			-- [] in 101 for example  
	| t==10            = 3			-- ten
	| t `elem` [11,12] = 6			-- eleven, twelve
	| t `elem` [15,16] = 7			-- fifteen, sixteen
	| t==17            = 9			-- seventeen, eighteen
	| otherwise        = 8			-- thirteen, fourteen, eighteen, nineteen

tens :: Int -> Int
tens t
	| t `elem` [9,8,3,2] = 6	 		  -- ninety, eighty, thirty, twenty
	| t `elem` [6,5,4]   = 5	 		  -- sixty, fifty, forty
	| t == 7             = 7			  -- seventy

digits :: Int -> Int 
digits d 
	| d `elem` [1,2,6] = 3 -- one, two, six
  | d `elem` [4,5,9] = 4 -- four, five, nine
	| d `elem` [3,7,8] = 5 -- three,seven,eight
	| d == 0           = 0

count :: [Int] -> Int
count xs = case xs of 
	(1:0:0:0:[])-> 11
	(h:0:0:[])	-> digits h + 7                       -- h hundred
	(h:t:d:[])  -> 
		case t of 
			0		-> digits h + 10 + digits d               -- h hundred and 0,1-d
			1		-> digits h + teens (10+d) + 10           -- h hundred and 0,1-d
			_		-> digits h + tens t + 10 + digits d      -- h hundred and t-d
	(1:d:[])    -> teens (10+d)                       -- ten..nineteen
	(t:0:[])    -> tens t                             -- twenty, thirty .. ninety
	(t:d:[])    -> tens t + digits d                  -- twenty-d, thirty-d .. ninety-d
	(0:[])      -> 4                                  -- 0
	(d:[])      -> digits d                           -- one..nine
	[]					-> 0
	_						-> error "Unimplemented number"
