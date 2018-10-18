# generalCode
This is a repo for R functions that will be shared across other National Deaf Center Repos

To clone a NDC repo, use the following command:
`git clone --recurse-submodules https://github.com/nationalDeafCenter/REPO-NAME`


Most (all?) of the tasks done by functions in this repo can be done using the `R` `survey` package instead.
Some of the code was copied from there (see [https://github.com/cran/survey])

However, these functions are pared down, so they work directly with dataframes (as opposed to survey design objects) 
With large ACS datasets, they are substantially faster than their `survey` counterparts.

In test cases, they have given identical results.
