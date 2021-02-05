noElem :: [Integer]
noElem = [x | x <- [10..20], notElem x [13, 15, 19]]

replicate' :: Int -> a -> [a]
replicate' x y = take x (repeat y) 

sum3 :: Num t => [[t]] -> [[t]]
sum3 xxs = [[sum xs] | xs <- xxs]

-- :t ([1,2,3], "hello", ['a','b'], (1, 'c', True))

-- [2,2..10]
-- [2.1..10]
-- [1.0..10.0]
-- [1.0, 1.1..10.0]
-- [15,13..13]
