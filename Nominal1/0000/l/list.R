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



infill <- function(X, replacement=NA) {
    for (k in X %|% seq_along) if (X[[k]] %|% is.null) X[[k]] <- replacement
    X }

    Doc$infill <- '
        infill returns a modified copy of the list argument where
        all NULLs are replaced with replacement.'
