import Data.List (sort)
-- The second puzzle is: 2 lists of numbers, each with random numbers
-- Times each number in the first list with how often it occurs in the second,
-- and then sum up the total of those scores
-- Example:
-- 3   4
-- 2   4
-- 4   3
-- 3   1
-- Make pairs:
-- 3 * 1 = 3
-- 2 * 0 = 0
-- 4 * 2 = 8
-- 3 * 1 = 3
-- total = 14
-- The answer would be 14

-- We need a function that can handle input and turn it into a usable list
-- it's easier if we put the puzzle input into a file and have it read from that
main = do
    content <- readFile "input1.txt"
    print $ answer $ words content

answer :: [String] -> Integer
answer x = sim (listify x ([],[])) 0

-- First, we want to parse the input and put the numbers in 2 lists
-- Then, we want to sort both lists
-- Then, we want to loop over both lists, get the differences of each Xth number
-- and keep track of the total
listify :: [String] -> ([Integer], [Integer]) -> ([Integer], [Integer])
listify [] (xs, ys) = (sort xs, sort ys)
listify [x] (ys, zs) = (ys, zs)
listify (a:b:zs) (as, bs) = listify zs ((read a :: Integer) :as, (read b :: Integer):bs)

--      Left list  Right list    Acc       Output
sim :: ([Integer], [Integer]) -> Integer -> Integer
sim ([], [])  x = x
sim ([x], ys) z = occurrenceSum x ys z
sim (x:xs, ys) z = sim (xs, ys) (z + occurrenceSum x ys 0)

occurrenceSum :: Integer -> [Integer] -> Integer -> Integer
occurrenceSum _ [] z = z
occurrenceSum x (y:ys) z   | x == y = occurrenceSum x ys z + x
                        | otherwise = occurrenceSum x ys z