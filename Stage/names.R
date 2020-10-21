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
# Shapiro: A Handsome Helper for R
# Copyright (C) 2018 D. Michael Parrish


unique.names <- argswap(lapply) %<=% names %O% unlist %O% unique

    Doc$unique.names <- '
        unique.names returns the names found among all of the
        objects in the list argument.

        unique.names(list(
                            
        list(A=1, B=2),
        
        data.frame(C=5, B=7)))

        [1] "A" "B" "C"
        '

regular.names <- function(n) paste("X", 1:n, sep="")

    Doc$regular.names <- '
        regular.names returns a character vector of dummy names
        of the length specified.'

empty.names <- character

    Doc$empty.names <- '
        empty.names is **** DESIGNED **** t0 return a character
        vector of null strings, the lenght of which is specified
        by the argument.'

with_regular.names <- function(X) {
	names(X) <- X %|% `#` %|% regular.names
	X }

with_empty.names <- function(X) {
	names(X) <- X %|% `#` %|% empty.names
	X }

with_colnames <- function (X, .colnames) {
    colnames(X) <- .colnames
    X }

with_rownames <- function (X, .rownames) {
    rownames(X) <- .rownames
    X }

    Doc$with_colnames <- '
        with_colnames and with_rownames return a modified copy of the argument,
        where the names (colnames or rownames) have been specified by arg 2.'

    Doc$with_rownames <- Doc$with_colnames

with_dimnames <- function (X, dimnames.) {
    dimnames(X) <- dimnames.
    X }

    Doc$with_dimnames <- '
        with_dimnames is a functional version of dimnames.'

regular.dimnames <- function(.dim, prefixes=LETTERS[.dim %|% seq_along], sep=" ")
		lapply(
			.dim %|% seq_along,
			function(k) paste(prefixes[k], .dim[k] %|% seq, sep=sep))

    Doc$regular.dimnames <- '
        regular.dimnames returns a list that can be used as arg
        2 of dimnames<-. The names are produced by postfixing
        the first few natural numbers (as character strings) to
        the specified prefixes.

        > regular.dimnames(c(2, 3), sep=".")

        [[1]]

        [1] "A.1" "A.2"

        [[2]

        [1] "B.1" "B.2" "B.3"'

rename.all <- function (x, newnames=NULL) {
    names(x) <- newnames;   x }

rename.all.dims <- function (x, newnames=NULL) {
    dimnames(x) <- newnames;   x }

sans.row.names <- function (x) {
        row.names(x) <- NULL;   x }

