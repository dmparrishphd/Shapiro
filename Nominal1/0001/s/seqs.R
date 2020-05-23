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



seq_fromto <- function(fromto, ...)
		seq(from=fromto[1], to=fromto[2], ...)

	Doc$seq_fromto <- '
        seq_fromto is the same as seq, except that the from and
        to arguments of seq are collapsed into a single fromto
        argument. The design is for fromto to be a 2-vector.'

seq_unif <- function (length.out, zero.include=T, one.include=T) {
    USE <- c(one.include, zero.include)
	n   <- length.out + sum(!USE) - 1L
	i   <- 0:n / n
    EXCEPT <- c(
        NULLif(USE[-1], -1L),
        NULLif(USE[ 1], -length(i) ) )
    if (EXCEPT %|% is.empty) i else i[EXCEPT] }

    Doc$seq_unif <- '
        seq_unif returns regularly spaced values from the unit
        interval [0, 1];
        
        length.out specifies the length of the return.
        
        The two (optional) args tell whether to include zero or
        one in the return. The optional args **** DO NOT ****
        affect the length of the return.

        EXAMPLES

        format. <- formatC %^% list(digits=3, format="f")

        cat. <- format. %O% function(...) cat(..1, "\n")

        seq_unif(9, T, T) %|% cat.

        # 0.000 0.125 0.250 0.375 0.500 0.625 0.750 0.875 1.000 

        seq_unif(9, T, F) %|% cat.

        # 0.000 0.111 0.222 0.333 0.444 0.556 0.667 0.778 0.889 

        seq_unif(9, F, F) %|% cat.

        # 0.100 0.200 0.300 0.400 0.500 0.600 0.700 0.800 0.900 

        seq_unif(9, F, T) %|% cat.

        # 0.111 0.222 0.333 0.444 0.556 0.667 0.778 0.889 1.000 
        '

seq_grid.centered <- function(ncell)
        seq(from=0, to=1, length.out=1 + ncell)

seq_cell.centered <- function (ncell)
        (-.5 / ncell + ncell %|% seq_grid.centered)[-1]

    Doc$seq_grid.centered <- '
        seq_grid.centered returns a regular sequence of values
        from zero to one. Conceptually, each pair of adjacent
        values in the return represents the boundaries of
        one-dimensional, regular grid cells. The arguments
        specifies the number
        of cells.

        See also: seq_cell.centered.'

    Doc$seq_cell.centered <- '
        seq_cell.centered returns a regular sequence of values
        completely within the unit interval. Conceptually, each
        pair of adjacent values in the return represents the
        centroids of one-dimensional, regular grid cells. The
        arguments specifies the number of cells.
        
        See also: seq_grid.centered.'

