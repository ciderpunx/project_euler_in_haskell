-- A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
-- a2 + b2 = c2

-- For example, 32 + 42 = 9 + 16 = 25 = 52.

-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc.

-- I thought at first we were to find a, b and c. i.e. like this:
first_try = [ (a,b,c) | a <- [1..500], b <- [a..500], let c = 1000 - a - b, (a^2 + b^2) == c^2 ]

-- But to find abc, we can just tweak a little thus:
proper = head $ [ (a * b * c) | a <- [1..500], b <- [a..500], let c = 1000 - a - b, (a^2 + b^2) == c^2 ]

-- And the haskell org solution was this unfeasibly more complicated looking one. I'm not sure why 
-- it makes sense to do something so complex, presumably so that one can scale the triplets part
triplets l = [[a,b,c] | m <- [2..limit],
                        n <- [1..(m-1)], 
                        let a = m^2 - n^2, 
                        let b = 2*m*n, 
                        let c = m^2 + n^2,
                        a+b+c==l]
    where limit = floor . sqrt . fromIntegral $ l
 
problem_9 = product . head $ triplets 1000
