import RelationalAlgebra

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
	
l16 = printTable (selectf "loan-num" (== "L-16") borrower)
	

