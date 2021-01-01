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



dimensions.iranges <- function (iranges) #TAGS array indices
        capply(iranges, dimension.irange)

    Doc$dimensions.iranges <- '
        Multidimensional version of dimension.irange. The
        argument matrix has one column per dimension, with the
        "fastest" dimension first.' 

block.extract.a <- function(a, iranges)
    if (a %|% is.empty) a else
    extract.a(a, drop=F, lapply(
        a %|% ndim %|% seq,
        function(k) nice.i(k %th% dim(a), k %th% lcapply(iranges, i.seq.range) ) ) )

extract.a..bb <- block.extract.a

    Doc$block.extract.a <- '
        block.extract.a returns an array formed from the
        contiguous ranges of rows, columns, etc., as specified
        by iranges (arg 2)
    
        iranges is a bounding box (see bounding.box) for the
        elements (inclusive) to be extracted; the number of
        columns is equal to the number of dimensions of the
        array (arg 1).  Elements implied by iranges that are not
        in the array (arg 1) are mapped to NA-values in the
        return.

        > M9 <- matrix(1:9, nrow=3)

        > M9

            [,1] [,2] [,3]

        [1,]    1    4    7

        [2,]    2    5    8

        [3,]    3    6    9

        > block.extract.a(M9, matrix(c(2, 3,   2, 4), nrow=2))

        [,1] [,2] [,3]

        [1,]    5    8   NA

        [2,]    6    9   NA'

