ixOfStr :: (Eq a, Num b) => [a] -> a -> b
ixOfStr lyst str = ixs' 0 lyst str where 
	ixs' i [] s = -1 
	ixs' i (z:zs) s 
		| (s == z) = i 
		| otherwise = ixs' (i+1) zs s

selectf :: Field -> (Value -> Bool) -> Table -> Table 
selectf field pred (Table n s v) = 
	if (i == (-1)) then (Table nn [] [[]]) 
	else Table nn s (filter (\z -> (pred (z !! i))) v)
	where 
		i = ixOfStr s field 
		nn = ("selectf (" ++ n ++ ")") 

maxWidths :: Table -> [Int]
maxWidths t@(Table n s v) = 
	maxWidths' ((fieldNameWidths t) : (columnWidths t)) where 
		maxWidths' :: [[Int]] -> [Int] 
		maxWidths' [] = [] 
		maxWidths' [una] = una
		maxWidths' (una:mas) = zipWith max una (maxWidths' mas) 
		columnWidths (Table n s v) = map (map length) v 
		fieldNameWidths (Table n s v) = map length s

showTable :: Table -> [Char]	
showTable t@(Table n s v) = 
	newline 
		++ "[" ++ show cd ++ "]:" ++ n ++ newline
		++ br 
		++ formedLine ms s ++ newline 
		++ br 
		++ concat (map (\s -> formedLine ms s ++ newline) v) 
		++ br 
		where 
			ms = maxWidths t
			ss = sum ms + 1 + length ms
			br = nChars '-' ss ++ newline
			cd = length v
			formedLine lengs stufs = 
				"|" ++ (concat 
				(zipWith (\l s -> capped (strRtPaddedTo l s)) lengs stufs))
			strRtPaddedTo n str = let ln = length str in 
				if (n >= ln) then str ++ nSpaces (n - ln) 
				else take n str
			nSpaces = nChars ' '
			nChars :: Char -> Int -> [Char] 
			nChars = flip replicate
			capped str = str ++ "|"

newline :: [Char]
newline = ['\r', '\n']

printTable :: Table -> IO()
printTable = putStr . showTable
