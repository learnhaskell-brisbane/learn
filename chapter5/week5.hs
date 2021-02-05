doubFact n = doubFact' n 1
doubFact' 0 a = a
doubFact' 1 a = a
doubFact' n a = doubFact' (n - 2) (n * a)

addi :: Integer -> Integer -> Integer
addi x 0 = x
addi x y = 1 + addi x (y - 1)
	
log2 1 = 0
log2 x = 1 + log2 (div x 2)

log2' n  = log2'' n 0
log2'' 1 a = a
log2'' n a = log2'' (div n 2) (a+1)

--move n−1 discs from A to C
--move disc n from A to B
--move n−1 discs from C to B.

hanoi :: Integer -> a -> a -> a -> [(a, a)]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a

--move n−1 discs from A to B. This leaves disc n alone on peg A
--move disc n from A to C
--move n−1 discs from B to C so they sit on disc n

hanoi' :: Integer -> a -> a -> a -> [(a, a)]
hanoi' 0 _ _ _ = []
hanoi' n a b c = hanoi' (n-1) a c b ++ [(a,c)] ++ hanoi' (n-1) b a c
