import BlacklistData

{-    
Storycard 1
-----------
- Using BlacklistData.hs, write "blacklister", a function that returns the customers that are blacklisted (their postcode is one of BlacklistData.blacklistedPostcodes).
- Use the Customer type exported from BlacklistData.
- Use testBlacklister below to check your results.
-}

testBlacklister = blacklister == expected

{-
Storycard 2
-----------
1. Add a "main" method to blacklister.hs that outputs blacklister to stdout (as lines of CSV).
2. Run "ghc blacklister"
3. Run "blacklister > blacklisted.csv"
4. Compare blacklisted.csv with expected.csv
-}

{-
Storycard 3
-----------
Write "blacklisterPro", an app that reads customers from "all_customers.csv" and has two forms of output:
1. Displays the IDs of all the blacklisted customers and a summary like so:
    nnnn customers blacklisted: [10100027,10100205,<etc>]
2. Writes the full records of blacklisted customers to blacklisted.csv (ordering is not important).  Use BlacklistData.columnNames for header row.
-}

{-
Storycard 4
-----------
1. Write "blacklisterMapReduce", an app that retrofits "blacklisterPro" to process customers in at least 10 parallel threads/processes.
   - What difficulties are there in this approach?
   - What are the benefits of this approach?
-}
