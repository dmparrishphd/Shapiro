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



strsplitn <- function(h, n) {
    #warning("strsplitn returns a value consistent with strsplit as of 2018-10-17.")
    dimension(
        substr(h, 1, n) %,% substr(h, succ(n), nchar(h)),
        c(h  %|%  `#`, 2))  %|%  rows.m }

    Doc$strsplitn <- '
        strsplitn returns a list of character vectors; the list
    elements correspond to the elements of the 
    character vector argument.
    Each of the elements of the return contains a character
    vector of length 2, where the first element contains a
    character string with the first n individual characters from
    the corresponding character vector argument, and the second
    element contains the remaining individual characters.'

.unright <- function (h, n) strrev(strsplitn(strrev(h), n)[[1]][2])

unright <- function (h, stop=1) vapply_(h, .unright, stop)
'#TODO recode more simply
unright <- function (h, stop=1) {
    substr(h, start=stop %|% succ, stop=h %|% nchar)
     }
'
    Doc$unright='
        Similar to right, but EXCLUDES the individual characters
        from the stop position onward. unright("RIGHT", 2) ==
        "RIG"'

substrs <- function(h, stops=nchar(h))
	capply(
		rbind(1L %,% succ(unrest(stops)), stops),
		substr2 %|% argswap,
		h,
		h)

substrs.regular <- function(h, .stop=1)
		substrs(h, .stop * nchar(h) %\% .stop %|% seqN)

Doc$substrs <- '
        Similar to substr, but allows a vector of stops rather
        than a single stop. The corresponding starts are
        computed such that the start for one substring is one
        more than the stop of the previous substring. The start
        of the first substring is 1.'

    Doc$substrs.regular <- '
        substrs.regular is similar to substrs, except that all
        of the substrings are of the same width. The .stop argument
        gives the location of the end of the first substring.'

        
