# Shapiro: A Handsome Helper for R
# Copyright (C) 2019 D. Michael Parrish
# COPYRIGHT NOTICE CONTINUES AT ./COPYRIGHT2.txt
# 
# BRIEF TABLE OF CONTENTS
#
# ./COPYRIGHT1.R                     Copyright Notice (PART 1/2)
# ./COPYRIGHT2.R                     Copyright Notice (PART 2/2)
# ./LICESE.txt                                 License (Primary)
# ./LICENSE-Stack_Overflow.htm                 License for curry
# *.R                                   (Body / Primary Content)



matches <- function(x, Table, ...)
	Table[,2][match(x, Table[,1], ...)]

    Doc$matches <- '
        matches is similar to match. The Table argument has two
        columns. The first column corresponds to the table
        argument of match; matches applies match to the first
        column of Table and uses the result to extract values
        from the second column of Table, returning the extracted
        values.

        ... is passed to match.
    
        > matches(0:15, cbind(0:15, strsplit("0123456789ABCDEF", "")[[1]]))

        [1] "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F"'

comparr <- function ( #TAGS reflexive compare
        extract, store=catn, compar=`-`, nmax=Inf,
        compar.sentinel=identical, verbose=F) {
	if (nmax < 1L) return (invisible())
	SENTINEL <- extract()
	i <- 1L
	Prev <- i %|% extract
	if (compar.sentinel(Prev, SENTINEL))
			return (invisible())
	repeat {
		if (nmax < i) return (invisible())
		i <- i + 1L
        if (verbose) {
            catn("make.comparison (about to extract Next): i:", i)
        }
		Next <- i %|% extract
        if (verbose) {
            catn("make.comparison: i:", i)
            catn("make.comparison: kind(Next):---")
            Next %|% kind %|% print
            catn("make.comparison: kind(SENTINEL):---")
            SENTINEL %|% kind %|% print
            catn("make.comparison: length(SENTINEL):---")
            SENTINEL %|% length %|% print
        }
		if (compar.sentinel(Next, SENTINEL))
				return (invisible())
		compar(Next, Prev) %|% store
		Prev <- Next }
	invisible() }

    Doc$comparr <- '
        comparr---reflexive compare---makes repeated calls to
        extract (arg 1) with the arguments 1L, 2L, ...  and
        calls compar (arg 3) with the objects returned from
        successive calls.  The result of each compar-call is
        passed to store (arg 2).  The process stops when nmax
        comparrisons have been made or when the return of
        extract compares with the same object that is returned
        from a call of extract with no arguments.

        Originally intended for differencing successive time
        steps of gridded model results.

        EXAMPLES

        # The call
        
        make.comparison(function(k)	if (missing(k)) return (NULL) else if (1 <= k && k <= 5) 2^k else NULL)

        # writes the following to stdout(), one number per line:

        2 

        4 

        8 

        16 
        '
