--Find the sum of all the multiples of 3 or 5 below 1000.

-- set is a set of numbers i.e. [1..999]
-- divs is the numbers to test whether numbers from set can be divided by them, eg [3,5]
-- so nums [1..999] [3,5] will give the answer 233168 for [1..9] we do indeed get 23:

nums,euler001 :: [Int] -> [Int] -> Int
nums set divs = sum $ filter (\x -> fn x divs) set
  where fn _ []     = False
        fn x (y:ys) = x `mod` y == 0 || fn x ys

euler001 ds xs = sum $ filter (\x -> elem 0 $ map (x `mod`) ds) xs
