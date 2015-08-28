-- The fraction 49/98 is a curious fraction, as an inexperienced mathematician in attempting to simplify it may incorrectly believe that 49/98 = 4/8, which is correct, is obtained by cancelling the 9s.
-- 
-- We shall consider fractions like, 30/50 = 3/5, to be trivial examples.
-- 
-- There are exactly four non-trivial examples of this type of fraction, less than one in value, and containing two digits in the numerator and denominator.
-- 
-- If the product of these four fractions is given in its lowest common terms, find the value of the denominator.
import Data.Ratio

main :: IO ()
main = 
    print answer

answer :: Integer
answer = 
    denominator . product $ map third matches

-- List comprehensions FTW!
-- build up a list by getting some unique as and cs
-- for each a and c
--    call a/c d
--    multiply a by 10 and add b
--    multiply b by 10 and add c
-- i.e. a=4 b=9 c=8 40+9 == 49 90+8=98
-- Call that ratio e (49/98) and if it is equal to d, we got one!
-- Note the use of uncurry. The compiler told me to do it.
matches :: [((Integer, Integer), (Integer, Integer), Ratio Integer)]
matches = [ ((a,c),e,d) | a <- [1..9]
              , b <- [1..9]
              , c <- [1..9]
              , a /= b && a /= c
              , let d = a % c 
              , let e = (10 * a + b, 10 * b + c)
              , uncurry (%) e == d
          ]

third :: (a,a,b) -> b
third (_,_,d) = d
