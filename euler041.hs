-- We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital and is also prime.
--
-- What is the largest n-digit pandigital prime that exists?
import Data.List
import Prime

main :: IO ()
main = print answer

-- Pull out the largest prime; I think that the last will always be the largest, but you could do maximum
-- if I am wrong here
answer :: Integer
answer = last perms

-- Go through all the permutations of all lists of numbers [1..n] where n is prime and > 2
-- pull out the prime permutations
-- Slow but tolerable, just
perms :: [Integer]
perms = 
    [c | a <- ['3','5'..'9']
       , b <- permute ['1'..a]
       , let c = read b::Integer
       , prime c
    ]
  where
    permute "" = [""]
    permute str = [(x:xs)| x <- str, xs <- permute (delete x str)]
