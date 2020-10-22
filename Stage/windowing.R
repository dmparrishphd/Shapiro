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

i.disjoint.windows <- function(length, width=2) #TAGS windowing window function range
	    length %=>% n.round.down.n %:|% width %|% seq %=>% matrix %:|% width

    Doc$i.disjoint.windows <- '
        i.disjoint.windows returns a matrix of width (arg 2)
        rows whose columns are integer indices into the first
        few disjoint, contiguous intervals of a hypothetical
        vector of the specified length, rounded down to the
        nearest multiple of width (arg 2).

        Designed for use in sampling a raster at regular
        intervals (the columns of the return are interpreted as
        column or row numbers of the raster to be sampled.)

        i.disjoint.windows(11, 3)

             [,1] [,2] [,3]

        [1,]    1    4    7

        [2,]    2    5    8

        [3,]    3    6    9
        '


