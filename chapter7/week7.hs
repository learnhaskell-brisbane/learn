module RelationalAlgebra ( 
	Table 
	, project
	, select
	, cross
	, join
	, thetaJoin
	, divideBy
	, intersect
	, union
	 -- set difference --
	, (\\)
	, renameTable
	, renameSchema
	, rename

	-- some special cases -- 
	, selectf
	, cross2
	, cross0
	
    -- some display utilities -- 
	, printTable
	, showTable

	-- some miscellaneous utilities -- 
	, assocL1
	, assocL2
	, oneBasedIndexLookup
	, adheresToSchema
	, countRecords
	) 
	where 
import qualified Data.List 
import Data.Maybe

-- ADT 
type Name = String 
type Field = String 
type Value = String 
type Schema = [Field] 
type Record = [Value] 
type Records = [Record]

data Table = Table 
	{ name :: Name         -- table name 
	, schema :: Schema     -- field names 
	, records :: Records } -- record contents 
	deriving (Eq, Show)

columnCount (Table n s v) = 
	let ls = length s in
		if (length v > 0) then 
			let rl = length (v!!0) in 
				if (rl /= ls) then Nothing else Just ls 
		else Just ls

account = Table 
	"account" 
	[ "acct-num", "branch-name", "balance"] 
	[ ["A-101" , "Downtown" , "500" ] 
	, ["A-102" , "Perryridge" , "400" ] 
	, ["A-201" , "Brighton" , "900" ] 
	, ["A-215" , "Mianus" , "700" ] 
	, ["A-217" , "Brighton" , "750" ] 
	, ["A-222" , "Redwood" , "700" ] 
	, ["A-305" , "Round Hill" , "350" ] ]

branch = Table 
	"branch" 
	[ "branch-name", "branch-city", "assets" ] 
	[ ["Brighton" , "Brooklyn" , "7100000" ] 
	, ["Downtown" , "Brooklyn" , "9000000" ] 
	, ["Mianus" , "Horseneck" , "400000" ] 
	, ["North Town" , "Rye" , "3700000" ] 
	, ["Perryridge" , "Horseneck" , "1700000" ] 
	, ["Pownal" , "Bennington" , "300000" ] 
	, ["Redwood" , "Palo Alto" , "2100000" ] 
	, ["Round Hill" , "Horseneck" , "8000000" ] ]

customer = Table 
	"customer" 
	[ "cstmr-name", "cstmr-street", "cstmr-city" ] 
	[ ["Adams" , "Spring" , "Pittsfield" ] 
	, ["Brooks" , "Senator" , "Brooklyn" ] 
	, ["Curry" , "North" , "Rye" ] 
	, ["Glenn" , "Sand Hill" , "Woodside" ] 
	, ["Green" , "Walnut" , "Stamford" ] 
	, ["Hayes" , "Main" , "Harrison" ] 
	, ["Johnson" , "Alma" , "Palo Alto" ] 
	, ["Jones" , "Main" , "Harrison" ] 
	, ["Lindsay" , "Park" , "Pittsfield" ] 
	, ["Smith" , "North" , "Rye" ] 
	, ["Turner" , "Putnam" , "Stamford" ] 
	, ["Williams" , "Nassau" , "Princeton" ] ]

depositor = Table 
	"depositor" 
	[ "cstmr-name", "acct-num" ] 
	[ ["Hayes" , "A-102" ] 
	, ["Johnson" , "A-101" ] 
	, ["Johnson" , "A-201" ] 
	, ["Jones" , "A-217" ] 
	, ["Lindsay" , "A-222" ] 
	, ["Smith" , "A-215" ] 
	, ["Turner" , "A-305" ] ]

loan = Table 
	"loan" 
	[ "loan-num", "branch-name", "amount" ] 
	[ ["L-11" , "Round Hill" , "900" ] 
	, ["L-14" , "Downtown" , "1500" ] 
	, ["L-15" , "Perryridge" , "1500" ] 
	, ["L-16" , "Perryridge" , "1300" ] 
	, ["L-17" , "Downtown" , "1000" ] 
	, ["L-23" , "Redwood" , "2000" ] 
	, ["L-93" , "Mianus" , "500" ] ]

borrower = Table 
	"borrower" 
	[ "cstmr-name", "loan-num" ] 
	[ ["Adams" , "L-16" ] 
	, ["Curry" , "L-93" ] 
	, ["Hayes" , "L-15" ] 
	, ["Jackson" , "L-14" ] 
	, ["Jones" , "L-17" ] 
	, ["Smith" , "L-11" ] 
	, ["Smith" , "L-23" ] 
	, ["Williams" , "L-17" ] ]

assocL1 :: Table -> Int -> Field -> Maybe Value 
assocL1 (Table n s v) whichTupleZeroBased field = 
	let tupleVar = v !! whichTupleZeroBased 
	in assocL2 s tupleVar field

assocL2 :: Schema -> Record -> Field -> Maybe Value 
assocL2 schema record field = 
	lookup field (zip schema record)

oneBasedIndexLookup :: Table -> Int -> Int -> Maybe Value 
oneBasedIndexLookup (Table n s v) whichTupleZeroBased iField = 
	if ((iField <= (length s)) && (iField >= 1)) then 
		Just ((v !! whichTupleZeroBased) !! (iField-1)) 
	else 
		Nothing

adheresToSchema :: Schema -> Table -> Bool 
adheresToSchema schema (Table n s v) = 
	schema == s

countRecords :: Table -> Int 
countRecords (Table n s v) = length v

-- Pretty print --
maxWidths t@(Table n s v) = 
	maxWidths' ((fieldNameWidths t) : (columnWidths t)) where 
		maxWidths' :: [[Int]] -> [Int] 
		maxWidths' [] = [] 
		maxWidths' [una] = una
		maxWidths' (una:mas) = zipWith max una (maxWidths' mas) 
		columnWidths (Table n s v) = map (map length) v 
		fieldNameWidths (Table n s v) = map length s

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

newline = ['\r', '\n']
printTable = putStr . showTable

--projection
ixOfStr lyst str = ixs' 0 lyst str where 
	ixs' i [] s = -1 
	ixs' i (z:zs) s 
		| (s == z) = i 
		| otherwise = ixs' (i+1) zs s

nonNeg = (>= (0::Int))

ixsOfColumns cols schema = filter nonNeg (map (ixOfStr schema) cols)
pickElementsByIxs iocs lyst = map (lyst !!) iocs

project :: Schema -> Table -> Table 
project cols (Table nym schema values) = 
	let is = ixsOfColumns cols schema in 
		Table ("proj (" ++ nym ++ ")") 
			(pickElementsByIxs is schema) 
			(Data.List.nub (map (pickElementsByIxs is) values))

--select
selectf :: Field -> (Value -> Bool) -> Table -> Table 
selectf field pred (Table n s v) = 
	if (i == (-1)) then (Table nn [] [[]]) 
	else Table nn s (filter (\z -> (pred (z !! i))) v)
	where 
		i = ixOfStr s field 
		nn = ("selectf (" ++ n ++ ")") 

select :: (Schema -> Record -> Bool) -> Table -> Table 
select pred t@(Table n s v) = 
	let nn = ("select (" ++ n ++ ")") in 
		Table nn s (filter (pred s) v)

fieldOpConstP field ordop const = 
	(\schema record -> 
		(fromJust 
			(assocL2 schema record field)) `ordop` const)

fieldMatchesConstant field value = 
	fieldOpConstP field (==) value

numFieldOpConstP field ordop const = 
	(\schema record -> 
		(read 
			(fromJust 
				(assocL2 schema record field))) `ordop` const)

fieldOpFieldP f1 op f2 = 
	(\schema record -> assocL2 schema record f1 `op` assocL2 schema record f2)

twoFieldsMatch f1 f2 = fieldOpFieldP f1 (==) f2

numFieldOpNumFieldP f1 op f2 = 
	(\schema record ->
		let 
			v1 = (read (fromJust (assocL2 schema record f1))) 
			v2 = (read (fromJust (assocL2 schema record f2))) 
		in
			((v1 > 0) || (v1 <= 0)) &&
			((v2 > 0) || (v2 <= 0)) && 
			v1 `op` v2)

combinePs p1 combop p2 = 
	(\schema record -> 
		((p1 schema record) `combop` (p2 schema record)))

-- Rest
assert bool failstring = if (not bool) then error failstring else True

union (Table n1 s1 v1) (Table n2 s2 v2) = 
	let x = assert (s1 == s2) "Schema mismatch" in x `seq`
		Table ("union (" ++ n1 ++ ", " ++ n2 ++ ")") s1 (Data.List.union v1 v2)

intersect (Table n1 s1 v1) (Table n2 s2 v2) = 
	let x = assert (s1 == s2) "Schema mismatch" in x `seq`
		Table ("intersect (" ++ n1 ++ ", " ++ n2 ++ ")") s1 (Data.List.intersect v1 v2)

(\\) (Table n1 s1 v1) (Table n2 s2 v2) = 
	let x = assert (s1 == s2) "Schema mismatch" in x `seq`
		Table ("diff (" ++ n1 ++ ", " ++ n2 ++ ")") s1 (v1 Data.List.\\ v2)

renameTable :: Name -> Table -> Table 
renameTable newName (Table n s v) = Table newName s v

renameSchema :: Schema -> Table -> Table 
renameSchema newSchema (Table n s v) = 
	let x = assert ((length s) == (length newSchema)) "Length mismatch" in x `seq` 
		Table n newSchema v

rename :: Name -> Schema -> Table -> Table 
rename n s t = renameTable n (renameSchema s t)

dot s t = (s ++ "." ++ t)

cross0 (Table n1 s1 v1) (Table n2 s2 v2) = 
	let 
		nf2 = fn2 n1 n2 
		sf1 = map (dot n1) s1 
		sf2 = map (dot nf2) s2 
		ncl = cross' v1 v2 
		x = assert ((Data.List.nub ncl) == ncl) "Unexpected duplicates" in x `seq`
		Table ("cross0 (" ++ n1 ++ ", " ++ nf2 ++ ")") (sf1 ++ sf2) ncl

mapWhen p f = map (\z -> if p z then f z else z)

fanc tblnym attrs a = mapWhen (a ==) (dot tblnym) attrs

fancs _ aLs [] = aLs 
fancs tn aLs (aR:aRs) = fancs tn (fanc tn aLs aR) aRs

fn2 n1 n2 = if (n1 == n2) then error "Rename tables first!" else n2

cross' al bl = concat (map (\x -> (map (x ++) bl)) al)

cross (Table n1 s1 v1) (Table n2 s2 v2) = 
	let 
		nf2 = fn2 n1 n2 
		sf1 = fancs n1 s1 s2 
		sf2 = fancs nf2 s2 s1 
		ncl = cross' v1 v2 
		x = assert ((Data.List.nub ncl) == ncl) "Unexpected duplicates" in x `seq`
		Table ("cross (" ++ n1 ++ ", " ++ nf2 ++ ")") (sf1 ++ sf2) ncl

cross2 (Table n1 s1 v1) (Table n2 s2 v2) = 
	let 
		nf2 = fn2 n1 n2 
		sf2 = fancs nf2 s2 s1 
		ncl = cross' v1 v2 
		x = assert ((Data.List.nub ncl) == ncl) "Unexpected duplicates" in x `seq`
		Table ("cross2 (" ++ n1 ++ ", " ++ nf2 ++ ")") (s1 ++ sf2) ncl

dropUntil p [] = [] 
dropUntil p (c:cs) 
	| (p c) = cs 
	| otherwise = dropUntil p cs

unDot s = dropUntil (== '.') s

fixedNames n1 s1 n2 s2 = 
	let 
		sf1 = fancs n1 s1 s2 
		sf2 = fancs n2 s2 s1 
		x = assert (n1 /= n2) "Tables must have different names here !" in x `seq`
			let 
				a = (sf1 Data.List.\\ s1) 
				b = (sf2 Data.List.\\ s2) 
			in if (not (null (a `Data.List.intersect` b))) then 
				error "Catastrophic failure: fixedNames" 
			else (map unDot a,b)

joinPred (s1,s2) schema record = 
	let 
		s1picks = map fromJust (map (assocL2 schema record) s1) 
		s2picks = map fromJust (map (assocL2 schema record) s2) 
		bools = zipWith (==) s1picks s2picks 
		final = foldl (&&) True bools in final

join t1@(Table n1 s1 v1) t2@(Table n2 s2 v2) = 
	let 
		nf2 = fn2 n1 n2 
		fns = fixedNames n1 s1 nf2 s2 
		t3 = cross2 t1 t2
		t4 = select (joinPred fns) t3 
		rus = s1 `Data.List.union` s2 
		pj = project rus t4 in renameTable ("join (" ++ n1 ++ ", " ++ nf2 ++ ")") pj

thetaJoin :: (Schema -> Record -> Bool) -> Table -> Table -> Table 
thetaJoin p t1 t2 = (select p (join t1 t2))

[] `subset` [] = True 
s1 `subset` s2 = and (map (\f -> f s2) (map elem s1))

divideBy r@(Table n1 s1 v1) s@(Table n2 s2 v2) = 
	let 
		x = assert (s1 `subset` s2) "Divide precondition S subset R" 
		rms = s1 Data.List.\\ s2 
		t1 = project (rms ++ s2) r 
		t2 = cross (project rms r) s 
		t3 = project rms (t2 \\ t1) 
		t4 = project rms r 
	in x `seq` renameTable ("divideBy (" ++ n1 ++ ", " ++ n2 ++ ")") (t4 \\ t3)

main = do 
	printTable (selectf "loan-num" (== "L-16") borrower)
	putStr "Does \"account\" adhere to the given schema?" 
	putStr newline 
	print (adheresToSchema ["acct-num", "branch-name", "balance"] account)

	print (assocL1 account 0 "acct-num") 
	print (oneBasedIndexLookup account 0 1)

	perryridgeP <- return (fieldMatchesConstant "branch-name" "Perryridge") 
	printTable (select perryridgeP loan)

	amt1400P <- return (numFieldOpConstP "amount" (>) 1400) 
	printTable (select amt1400P loan)

	printTable (select (combinePs perryridgeP (&&) amt1400P) loan)

	tc1 <- return (project ["cstmr-name"] borrower) 
	tc2 <- return (project ["cstmr-name"] depositor) 
	t3214 <- return (union tc1 tc2) 
	printTable t3214

	t3215 <- return (tc2 \\ tc1) 
	printTable t3215

	perryCustomers <- return (select perryridgeP (cross borrower loan)) 
	printTable perryCustomers

	loanMatch <- return (twoFieldsMatch "borrower.loan-num" "loan.loan-num") 
	perryBorrowers <- return (project ["cstmr-name"] (select loanMatch 
		perryCustomers))

	printTable (rename "Perryridge Borrowers" ["customer-name"] perryBorrowers)

	d <- return (renameTable "d" account) 
	ps5 <- return (project ["balance"] account) 
	a <- return (renameTable "a" account) 
	ps6 <- return (cross a d) 
	ps7 <- return (select (numFieldOpNumFieldP "a.balance" (<) "d.balance") ps6) 
	ps9 <- return (project ["a.balance"] ps7) 
	ps10 <- return (renameSchema ["balance"] ps9) 
	ps11 <- return (ps5 \\ ps10) 
	printTable ps11

	q1 <- return (selectf "cstmr-name" (== "Smith") customer) 
	q2 <- return (project ["cstmr-street", "cstmr-city"] q1) 
	q3 <- return (rename "smith-addr" ["street", "city"] q2) 
	q4 <- return (cross customer q3) 
	qP1 <- return (twoFieldsMatch "cstmr-street" "street") 
	qP2 <- return (twoFieldsMatch "cstmr-city" "city") 
	qP <- return (combinePs qP1 (&&) qP2) 
	q5 <- return (select qP q4) 
	printTable q5

	printTable 
		(project ["customer.cstmr-name"] 
			(select 
				(combinePs 
					(twoFieldsMatch "customer.cstmr-street" "smith-addr.street") (&&) 
					(twoFieldsMatch "customer.cstmr-city" "smith-addr.city")) 
				(cross0
					customer 
						(rename "smith-addr" ["street", "city"] 
							(project ["cstmr-street", "cstmr-city"] 
								(selectf "cstmr-name" (== "Smith") customer))))))

	printTable (join borrower loan) 

	printTable 
		(project ["branch-name"] 
			(selectf "cstmr-city" (== "Harrison") 
				(join customer (join account depositor))))

	printTable (join customer (join account depositor))

	printTable (project ["cstmr-name"] (join borrower depositor))

	r1 <- return (project ["branch-name"] 
		(selectf "branch-city" (== "Brooklyn") branch)) 
	r2 <- return (project ["cstmr-name", "branch-name"] 
		(join depositor account))

	printTable (r2 `divideBy` r1)