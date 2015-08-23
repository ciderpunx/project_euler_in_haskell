-- find the difference between the sum of squares and the square of sums

sumSquare :: [Int] -> Int
sumSquare = sum . map ( ^ 2 )

squareSum :: [Int] -> Int
squareSum xs = 
    s ^ 2 
  where s = sum xs

-- ans 1..100
ans :: Int -> Int -> Int
ans x y
  | sumSquare [x..y] > squareSum [x..y] = sumSquare [x..y] - squareSum [x..y]
  | otherwise             = squareSum [x..y] - sumSquare [x..y]
