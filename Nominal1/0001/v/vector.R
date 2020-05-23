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



b.index <- function(x) {
	Return <- x %|% `#` %|% na
	Return[x == 1] <- F
	Return[x == 2] <- T
	Return }

    Doc$b.index <- '
        b.index is somewhat the opposite of index.b.  The return
        is a logical vector.  Elements of the argument equal to
        1 are mapped to F in the return; 2 is mapped to T, and
        all other values are mapped to NA.'

flatline <- function(x)
        c(x %|% first, x, x %|% last)

    Doc$flatline <- '
        flatline returns a modified copy of the vector argument
        where the first and last arguments have been duplicated
        at the ends.

        Originally intended for use in an extrapolation
        algorithm.
        
        > 1 %|% flatline

        [1] 1 1 1

        > integer() %|% flatline

        integer(0)

        > 1:6 %|% flatline

        [1] 1 1 2 3 4 5 6 6'

pairr <- function(x) #TAGS pairs reflexive
        x %|% except.last %rbind% x[-1]

    Doc$pairr <- '
        pairr returns a matrix containing the pairs of
        subsequent values found in the vector argument, one
        column per pair.'

padding <- function(x, n) {
    len <- x %|% `#`
    vector(
        mode=x %|% typeof,
        length=ceiling_multiple(n, len) - len) }

pad <- function (x, n=2)
        x %,% padding(x, n)

    Doc$pad <- '
        pad returns a possibly longer copy of the primary
        argument vector or list. The length of the return is a
        multiple of n (arg 2).  The values of any additional
        elements are determined by the default for the vector
        function.'

