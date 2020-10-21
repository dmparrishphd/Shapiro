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



`%[along%` <- function(X, Y) #TAGS extract
	    X[seq_along(Y)]

    Doc$`%[along%` <- '
        `%[along%` extracts from arg 1 the first length(arg 2) values.'

extract.by.name <- function(X, NAMES)
        X[NAMES %in% names(X)]

    Doc$extract.by.name <- '
        extract.by.name returns a subvector of the vector or
        list (arg 1): only those elments having a name matching
        those in NAMES (arg 2) are found in the return.'

extract.by.pname <- placeholder #TODO SHOULD EXTRACT LIKE ARGUMENT MATCHING
'
function(X, NAMES)
	X[pmatch(NAMES, names(X)) %|% rmna]
'

        

`%[L%` <- function (x, l)
        do.call(`[`, x %|% list %,% l)

li.dim <- function (.dim) # array indices
        lapply(.dim, seq)

    Doc$li.dim <- '
        li.dim returns a list that contains, for each dimension
        of the dim vector argument, the corresponding natural
        indices.
        
        > li.dim(c(3, 2))

        [[1]]

        [1] 1 2 3

        [[2]]

        [1] 1 2'

slice.along <- function (x, i, dimNo=1) { #TAGS array extract
    l <- x %|% dim %|% li.dim
    l[[dimNo]] <- i
    x %[L% l }

    Doc$slice.along <- '
        slice.along extracts elements of arg 1 (an array) by
        taking only those elements whose indices match arg 2
        along dimension number arg 3.





        '
