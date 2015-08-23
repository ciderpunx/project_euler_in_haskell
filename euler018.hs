-- represent triangle as a string with newlines between 'rows'
simpleTriangle="3\n7 4\n2 4 6\n8 5 9 3"
bigTriangle="75\n95 64\n17 47 82\n18 35 87 10\n20 04 82 47 65\n19 01 23 75 03 34\n88 02 77 73 07 63 67\n99 65 04 28 06 16 70 92\n41 41 26 56 83 40 80 70 33\n41 48 72 33 47 32 37 16 94 29\n53 71 44 65 25 43 91 52 97 51 14\n70 11 33 28 77 73 17 78 39 68 17 57\n91 71 52 38 17 14 91 43 58 50 27 29 48\n63 66 04 68 89 53 67 30 73 16 69 87 40 31\n04 62 98 27 23 09 70 98 73 93 38 53 60 04 23"
parse = map (map read . words) . lines

-- take the biggest of y or z and add to x
plusBiggest x y z = x + max y z

-- given 2 rows, for each elt of the first row find the largest sum of that elt
-- an elt of the second row adjacent to it. I am about 70% convinced this is correct
step xs ys = 
    zipWith3 plusBiggest xs ys zs
  where 
    zs = tail ys

-- working backwards take last 2 rows and call step on them
solve = foldr1 step

ans = solve . parse

-- just testing
ansSimple = ans simpleTriangle

-- actual result
answer = ans bigTriangle

main = print answer
