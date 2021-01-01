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

.PCH.H <- 
       "CIRCLE
        TRIANGLE UP
        CROSS
        X
        DIAMOND
        TRIANGLE DOWN
        X SQUARE
        CROSS X
        CROSS DIAMOND
        CIRCLE CROSS
        TRIANGLE TRIANGLE
        CROSS SQUARE
        CIRCLE X
        TRIANGLE SQUARE
        SQUARE SOLID
        CIRCLE SOLID
        TRIANGLE SOLID
        DIAMOND SOLID
        CIRCLE BORDER
        CIRCLE SOLID SMALL
        CIRCLE FILLED
        SQUARE FILLED
        DIAMOND FILLED
        TRIANGLE UP FILLED
        TRIANGLE DOWN FILLED" %|% unformat.except.newline

pch.h <- function(description) #TAGS plot
        if (missing(description)) {
            cat(.PCH.H, sep=HNL)
        } else {
            matches(description, data.frame(.PCH.H, .PCH.H %|% seq_along)) }

    Doc$pch.h <- '
        pch.h returns an integer suitable for a value in
        specifying the pch argument of plot and related
        functions.  When called with a single argument, the
        argument is a character string that describes one of the
        first 25 pch shapes. A list of valid character string
        arguments is returned when the function is called with
        no arguments.'

