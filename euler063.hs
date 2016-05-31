-- The 5-digit number, 16807=75, is also a fifth power. Similarly, the 9-digit number, 134217728=89, is a ninth power.
--
-- How many n-digit positive integers exist which are also an nth power?

main :: IO ()
main = print answer

answer :: Int
answer = length [(z,x,y) | x <- [1..100], y<-[1..100], let z = x^y, y == (length $ show z)]
