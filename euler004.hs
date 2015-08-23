import Data.List
import Data.Ord

-- A palindromic number reads the same both ways. The largest palindrome 
-- made from the product of two 2-digit numbers is 9009 = 91 × 99.
--
-- Find the largest palindrome made from the product of two 3-digit numbers.

-- My implementation lets you specify a min and max if that's "your bag".

-- My first shot, written on the ferry from Holland to UK.
largestPalindromeProduct min max = 
    maximum [  a * b | a <- range, b <- range, a < b, isPalindrome . show $ a * b ]
  where range = reverse [min .. max]


-- More complex, so that we can see the values of a and b.
largestPalindromeProductWithFactors min max = 
    last $ sortBy (comparing fst) 
    [  (a * b,"a=" ++ show a ++ ", b=" ++ show b ) | 
         a <- range, 
         b <- range, 
         a < b, 
         isPalindrome . show $ a * b
    ]
  where range = reverse [min .. max]

-- haskell.org solution, note use of let in the comprehension and the z<-[y..999]
-- It doesn't seem any more efficient than mine apart from not doing the multiplication twice
-- cf: http://www.haskell.org/haskellwiki/Euler_problems/1_to_10
problem_4 =
  maximum [x | y<-[100..999], z<-[y..999], let x=y*z, let s=show x, s==reverse s]


-- a helper function to tell us when we have a palindrome (again note the more elegant
-- implementation from haskell.org. This function could have been written:
-- isPalindrome xs = xs == reverse xs
isPalindrome xs 
  | xs == reverse xs    = True
  | otherwise           = False

