-- The nth term of the sequence of triangle numbers is given by, tn = ½n(n+1); so the first ten triangle numbers are:
-- 
-- 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
-- 
-- By converting each letter in a word to a number corresponding to its alphabetical position and adding these values we form a word value. For example, the word value for SKY is 19 + 11 + 25 = 55 = t10. If the word value is a triangle number then we shall call the word a triangle word.
-- 
-- Using words.txt (right click and 'Save Link/Target As...'), a 16K text file containing nearly two-thousand common English words, how many are triangle words?
import Data.Char
import Data.List.Split

main :: IO ()
main =
    answer >>= putStrLn

answer :: IO String
answer = do
      ws <- readFile "p042_words.txt"
      return . show .length .filter isTriangle . splitOn "," $ filter (/='"') ws

-- Find the sum of all the alphabetical numbers of the letters in a word
wordVal :: String -> Int
wordVal =
    sum . map alnum

-- Given a character give its alphabetical number
alnum :: Char -> Int
alnum l =
    ord l - 64

-- List of all triangle numbers up to 100, which seems not to affect the score (tried 1000 as well)
triangles :: [Int]
triangles =
    map triangle [1..100]

-- Given a number n return the nth triangle number
triangle :: Double -> Int
triangle n =
    round $ 0.5*n*(n+1)

-- Is a string a triangle word
isTriangle :: String -> Bool
isTriangle w =
    wordVal w `elem` triangles
