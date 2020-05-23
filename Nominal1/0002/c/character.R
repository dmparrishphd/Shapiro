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

unformat.except.newline <- function(h)
        vapply2(h %|% strsplitbynewline %|% unlist, unformat)

    Doc$unformat.except.newline <- '
        unformat.except.newline splits each element of the
        character vector argument by newline, consolidates the
        result into a single character vector, strips leading
        and trailing whitespace, and compresses all sequences of
        whitespace to a single space.'
