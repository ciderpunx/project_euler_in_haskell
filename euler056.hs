-- A googol (10100) is a massive number: one followed by one-hundred zeros; 100100 is almost unimaginably large: one followed by two-hundred zeros. Despite their size, the sum of the digits in each number is only 1.
--
-- Considering natural numbers of the form, ab, where a, b < 100, what is the maximum digital sum?
import Digits

main :: IO ()
main =
    print answer

answer :: Integer
answer =
    maximum candidates

candidates :: [Integer]
candidates =
    [ z | x <- [1..99], y <-[1..99], let z = sum $ digitsLong(x^y)]

