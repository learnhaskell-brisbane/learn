max' :: Ord a => a -> a -> a
max' x y = if x <= y then y else x

compareWithHundred :: Int -> Ordering
compareWithHundred = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs) = if p x then x : filter' p xs else filter' p xs

-- Question 1
yoghurt f (a, b) = f a b
hotCurry f a b = f (a, b)

--currying hotCurry fst 1 2
--partial application let a = hotCurry max 1 2
--a (1, 3)

-- Question 2
filterMap p f xs = [f x | x <- xs, p x]
filterMap' p f xs = (map f (filter p xs))
filterMap'' p f = map f . filter p

-- Question 3
length' :: [b] -> Integer
length' [] = 0
length' (x:xs) = foldr (\_ acc -> acc + 1) 1 xs
length'' (x:xs) = foldr (+1) 1 xs
length''' = foldl (const . succ) 0

append' :: [a] -> [a] -> [a]
-- foldr - r to l - start with y, go backwards through x and prepend x.
-- f x (foldr f acc xs)
append' x y = foldr (\xs' y' -> xs':y') y x
append'' x y = foldr (:) y x

-- foldl - l to r - start with y, reverse x - go forwards through x and append xs to y.
-- foldl f (f acc x) xs
badAppend x y = foldl (\y' x' -> x':y') y (reverse x)
badAppend' x y = foldl (flip(:)) y (reverse x)

flatten' :: [[a]] -> [a]
flatten' = foldr (\acc xs -> append' acc xs) []
flatten'' = foldr (append') []
--flatten'' x = map' (append') x

flatmap' :: (a -> [b]) -> [a] -> [b]
flatmap' f x = flatten' (foldr (\a b -> f a : b) [] x)
flatmap'' f = flatten' . map f

-- Question 4
dec2nat :: [Int] -> Int 
dec2nat = foldl (\acc x' -> (acc * 10) + x') 0

-- Question 5
--scanl (+) 1 [1..100]
--fib = scanl (+) 1 fib
fib = 0 : scanl (+) 1 fib

-- Question 6
f x = 5 + 8 / x
f' = (+5) . (8/)

-- Question 7
-- found by looking in prelude for a -> b -> a
fst' = yoghurt const
-- found by looking in prelude for a -> b -> b
snd' = yoghurt seq
snd'' = yoghurt $ seq