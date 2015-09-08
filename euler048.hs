--The series, 11 + 22 + 33 + ... + 1010 = 10405071317.
--
--Find the last ten digits of the series, 11 + 22 + 33 + ... + 10001000.
import Digits

main :: IO ()
main =
    print answer

-- Construct the big number with a foldl, convert to a list of ints
-- grab the last ten digits and convert that back to an int. Simples
answer :: Integer
answer =
    undigitsLong . takeLast 10 . digitsLong $ foldl powerUp 0 [1..1000]
  where
    takeLast n xs = drop (length xs - n) xs
    powerUp x y = y^y +x
