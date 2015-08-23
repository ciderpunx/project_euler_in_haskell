-- The Fibonacci sequence is defined by the recurrence relation:
--
--    Fn = Fn−1 + Fn−2, where F1 = 1 and F2 = 1.
-- 
-- Hence the first 12 terms will be:
-- 
--     F1 = 1
--     F2 = 1
--     F3 = 2
--     F4 = 3
--     F5 = 5
--     F6 = 8
--     F7 = 13
--     F8 = 21
--     F9 = 34
--     F10 = 55
--     F11 = 89
--     F12 = 144
-- 
-- The 12th term, F12, is the first term to contain three digits.
-- 
-- What is the first term in the Fibonacci sequence to contain 1000 digits?


fibs :: [Integer]
fibs = 0 : 1 : (zipWith (+) fibs (tail fibs))

first1000 :: Integer
first1000 = last (takeWhile (\x -> (length $ show x) <= 1000) fibs) 

-- Can be expressed somewhat more succintly (and seems quicker)
first1000' :: Integer
first1000' = last (takeWhile (<(10^1000)) fibs)

-- We actually want the F number. Like this.
lt1000 = "F" ++ (show $ length (takeWhile (<(10^999)) fibs))

main :: IO () 
main = do
    let x = first1000
    putStrLn $ (show x) ++ " is " ++ (show . length $ show x) ++ " digits long"
