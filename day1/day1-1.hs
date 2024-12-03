import Data.List (sort)
-- Time for another Advent of Code!
-- The first puzzle is: 2 lists of numbers, each with random numbers
-- Compare the smallest number in list 1 with the smallest number in list 2,
-- compare the second smallest, etc., essentially make pairs
-- and then sum up the differences between each of those pairs,
-- and then sum up the total of those differences
-- Example:
-- 3   4
-- 2   5
-- 1   3
-- 3   9
-- Make pairs:
-- 1   3    diff = 2
-- 2   4    diff = 2
-- 3   5    diff = 2
-- 3   9    diff = 6
--          total = 12
-- The answer would be 12

-- We need a function that can handle input and turn it into a usable list
-- it's easier if we put the puzzle input into a file and have it read from that
main = do
    content <- readFile "input1.txt"
    print $ answer $ words content

answer :: [String] -> Integer
answer x = diff (listify x ([],[])) 0

-- First, we want to parse the input and put the numbers in 2 lists
-- Then, we want to sort both lists
-- Then, we want to loop over both lists, get the differences of each Xth number
-- and keep track of the total
listify :: [String] -> ([Integer], [Integer]) -> ([Integer], [Integer])
listify [] (xs, ys) = (sort xs, sort ys)
listify [x] (ys, zs) = (ys, zs)
listify (a:b:zs) (as, bs) = listify zs ((read a :: Integer) :as, (read b :: Integer):bs)

diff :: ([Integer], [Integer]) -> Integer -> Integer
diff ([], [])  x = x
diff ([x], [y]) z = abs(x-y) + z 
diff (x:xs, y:ys) z = diff (xs, ys) (abs(x-y) + z)