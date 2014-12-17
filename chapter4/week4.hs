calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w,h) <- xs]
  where bmi weight height = weight / height ^ 2

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = firstname
        (l:_) = lastname
        
initials' :: String -> String -> String
initials' (f:_) (l:_) = [f] ++ ". " ++ [l] ++ "."

mult :: Integer -> Integer -> Integer
mult x 0 = 0
mult x y = x + mult x (y - 1)

addi :: Integer -> Integer -> Integer
addi x 0 = x
addi x y = 1 + addi x (y - 1)

roots :: RealFloat a => a -> a -> a -> (a, a)
roots a b c =
     ((-b + sqrt (b*b - 4*a*c)) / (2*a),
      (-b - sqrt (b*b - 4*a*c)) / (2*a))
         
roots' :: RealFloat a => a -> a -> a -> (a, a)
roots' a b c =
    let twoA = 2*a
        notB = sqrt (b*b - 4*a*c)
    in ((-b + notB / twoA), (-b - notB / twoA))
    
sign :: Integer -> Integer
sign a
    | a > 0 = 1
    | a < 0 = -1
    | otherwise = a

three :: Integer -> Bool
three x = x `mod` 3 == 0

five :: Integer -> Bool
five x = x `mod` 5 == 0

five_or_three :: Integer -> Bool
five_or_three x = three x || five x

match_total (x:xs) = (if five_or_three x then x else 0) + match_total xs
match_total [] = 0

guard_total (x:xs)
    | null xs = 0
    | five_or_three x = x + guard_total xs
    | otherwise = guard_total xs

where_total (x:xs) = sum [acc x | x <- xs]
    where acc x = (if five_or_three x then x else 0)
    
let_total (x:xs) =
    let acc x = (if five_or_three x then x else 0)
    in sum [acc x | x <- xs]

case_total xs = 
    case xs of [] -> 0
               (x:xs) -> (if five_or_three x then x else 0) + case_total xs


