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



catn <- function(...)
        cat(..., "\n")

    Doc$catn <- '
        catn has behavior identical to cat, except that newline
        is appended to the argument list. All arguments are
        passed to cat.
    
        catn() should write a blank line to stdout'


verbatim <- function(..., file="", sep="", fill=FALSE, labels=NULL, append=FALSE)
        cat(..., file=file, sep="", append=append)

    Doc$verbatim <- '
        verbatim is intended for processing character arguments.
    
        The ..., file, and append arguments are passsed to cat.

        The sep, fill, and labels arguments are **** IGNORED; ****
        **** sep="" is passed to cat. ****'

cat.with.names <- function(x, file="", sep=" ", end="\n", append=F)
		cat(rbind(x %|% names, sep, x %|% unlist, end), file=file, sep="", append=append)


    Doc$cat.with.names <- '
        cat.with.names is similar to cat, except at the names are prepended to each item.
        names are separated from is designed for named atomic vectors, including character vectors of short strings.'

    
