-- If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, there are exactly three solutions for p = 120.
-- 
-- {20,48,52}, {24,45,51}, {30,40,50}
-- 
-- For which value of p ≤ 1000, is the number of solutions maximised?
import Data.Ord
import Data.List
import Data.Function

main :: IO ()
main = print (floor answer :: Int)

-- take the first mostFrequentP and extract p which is our answer
answer :: Double
answer = fourth . head $ mostFrequentP triangles

-- with a list of triangles with p<=1000 (325 elts)
-- sort the list by the length of p
-- group into lists with the same p length
-- return the longest of these lists
mostFrequentP :: [(Double,Double,Double,Double)] -> [(Double,Double,Double,Double)]
mostFrequentP = 
    maximumBy (comparing length) . groupBy ((==) `on` fourth) . sortBy (comparing fourth)

-- construct all triangles with p <= 1000
-- this is kind of dumb but we keep a and b strictly ordered so it is jsut about tolerable
triangles :: [(Double,Double,Double,Double)] 
triangles = 
    [(a,b,c,p) | a<-[1..1000]
               , b<-[(a+1)..1000]
               , let c=sqrt((a**2)+(b**2))
               , isWhole c
               , let p = a+b+c
               , p <=1000]
  where
    isWhole n = (floor n :: Int) == (ceiling n :: Int)

-- take the 4th elt out of a 4-tuple
fourth :: (a,b,c,d) -> d
fourth (_,_,_,p) = p
