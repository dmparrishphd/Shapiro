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

.KIND.NAMES <- "CLASS MODE STORAGE.MODE TYPEOF " %|% words

.kind <- function(x)
        vapplyf(x, list(class, mode, storage.mode, typeof))

kind <- function(x)
        `names<-`(.kind(x), .KIND.NAMES)

    Doc$kind <- '
        returns a character vector with information about the
        kind of thing (e.g., class, etc.) the argument is.

        > kind(1)

        CLASS         MODE STORAGE.MODE       TYPEOF 
        "numeric"    "numeric"     "double"     "double" 
        
        '

kinds <- function (X)
        `colnames<-`(`row.names<-`(vapply2(X, kind), .KIND.NAMES), X %|% names)

    Doc$kinds <- '
        simlar to kind, but returns a matrix with kind
        information, one column per item of the compound object.

        > kinds(list(1, "A"))

                     [,1]      [,2]       
        CLASS        "numeric" "character"
        MODE         "numeric" "character"
        STORAGE.MODE "double"  "character"
        TYPEOF       "double"  "character"
        '
