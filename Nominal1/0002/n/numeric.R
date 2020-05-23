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



vapply.unif <- function( #TAGS unit interval
        n, FUN, FUN.VALUE,
        zero.include=T, one.include=T)
    vapply(
        seq_unif(
            n,
            zero.include=zero.include,
            one.include=one.include),
        FUN, FUN.VALUE, USE.NAMES=F)

    Doc$vapply.unif <- '
        vapply.unif is a special case of vapply.
    
        Internally, X argument of vapply is specified by n, the
        number of values.

        Elements of X are regularly spaced double-s on the unit
        interval [0, 1].

        FUN and FUN.VALUE are as in vapply.

        Whether the elements of X inlucde zero and/or one are
        controlled by the similarly named arguments.
        
        `...` and USE.NAMES options are not included: Conceptual
        `...` should be built into FUN; names may be assigned to
        the return.'


