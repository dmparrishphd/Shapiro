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



cunbind.all <- function(x)
		lapply(x %|% colNos, col.m %<=% x)

runbind.all <- function(x)
		lapply(x %|% rowNos, row.m %<=% x)

    Doc$cunbind.all <- '
        cunbind.all and runbind.all return list-s of all the
        columns or all of the rows of the matrix or data.frame
        argument.'


capply  <- function (m, FUN, FUN.VALUE=NULL, ...)
        vapply(
            m %|% colNos,
            function (i, m, ...) FUN(m[, i], ...),
            if (FUN.VALUE %|% is.null) FUN(m[, 1], ...) else FUN.VALUE,
            m,
            ...)

    Doc$capply <- '
         capply is similar to vapply, but operates on the
         columns of a matrix.
         
         If FUN.VALUE is not speficied, it will be computed by
         applying FUN to the first column of the matrix argument
         (arg 1).'

capply1to1 <- function (m, FUNs, FUN.VALUE=NULL, ...)
        vapply(
            m %|% colNos,
            function (i, m, ...) (FUNs  %[[mod% i)(m[, i], ...),
            if (FUN.VALUE %|% is.null) FUNs[[1]](m[, 1], ...) else FUN.VALUE,
            m,
            ...)

    Doc$capply1to1 <- '
        capply1to1 is similar to capply, except that the second
        argument is a **** list of functions **** and FUNs[[k]]
        (modulo #FUNs) will be applied to column k of the
        primary matrix argument.
        
         If FUN.VALUE is not speficied, it will be computed by
         applying FUNs[[1]] to the first column of the matrix
         argument (arg 1).'

rowNos <- function(X) (
    # ORDER IS SIGNIFICANT...
    if (X %|% is.array) {
        X %|% dim %|% first
    } else if (X %|% is.data.frame) {
        X %|% nrow
    } else if (x %|% is.list) {
        # ... list-S THAT ARE NOT data.frame-S
        0L
    } else if (x %|% is.vector) {
        # ... vector-S THAT ARE NOT list-S
        X %|% `#`
    } else {
        0L } ) %|% seqN

rowNos.m <- rowNos # function DEPRECATED. Use rowNos

colNos <- function(X) (
    # ORDER IS SIGNIFICANT...
    if (X %|% is.array) {
        DIM <- X %|% dim
        if (1 < DIM %|% `#`) DIM[2] else 1L
    } else if (X %|% is.data.frame) {
        X %|% ncol
    } else if (X %|% is.list) {
        # ... list-S THAT ARE NOT data.frame-S
        X %|% `#`
    } else if (X %|% is.vector) {
        # ... vector-S THAT ARE NOT list-S
        1L
    } else {
        0L } ) %|% seqN

colNos.m <- colNos # function DEPRECATED. Use colNos

    Doc$rowNos <- '
        rowNos returns the integer indices corresponding to 1)
        the rows of an array or data.frame argument, 2) the
        elements of an atomic vector argument, or 3) integer()
        for all other cases.
        
        HISTORY

        2019-09-11: previously misbehaved for data.frames.'

    Doc$colNos <- '
        colNos returns the integer indices corresponding to 1)
        the columns of an array or data.frame argument, 2) 1L
        for the "columns" of an atomic vector, 3) the elements
        in a list that is not a data.frame, or 4) integer() for
        all other cases.
        
        HISTORY
        
        2019-09-12: reworked to be consistent with rowNos.'

cexcept.last <- function (m)
        if (m %|% is.empty) m else m[        , -ncol(m), drop=F]
rexcept.last <- function (m)
        if (m %|% is.empty) m else m[-nrow(m),         , drop=F]

    Doc$cexcept.last <- '
        cexcept.last returns a modified copy of the matrix
        argument where the last column has been left off.'

    Doc$rexcept.last <- '
        rexcept.last returns a modified copy of the matrix
        argument where the last row has been left off.'

reflexive.m <- function (FUN, m, byrow=F)
        if (byrow)
                FUN(m %|% rrest, m %|% rexcept.last) else
                FUN(m %|% crest, m %|% cexcept.last)

rreflexive <- list(byrow=T) %v% reflexive.m
creflexive <- list(byrow=F) %v% reflexive.m

    Doc$reflexive.m <- '
        reflexive.m is the matrix analog of reflexive. The
        additional, optional byrow parameter allows row-wise
        reflexivity vs. the default of column-wise reflexivity.'

`r/` <- `/` %|% argswap %=>% rreflexive

    Doc$`r/` <- '
        r/ accomplishes row-wise division. The order of
        arguments matches visually with typical representations
        of fractions (i.e. the upper row is divided by the lower
        row):

        > m <- matrix(1:2);   m

            [,1]

        [1,]    1

        [2,]    2

        > `/r`(m)

        [1] 0.5'

`r==` <- rreflexive %<=% `==` 

    Doc$`r==` <- '
        Compare matrix elements row-wise for equality.

        > `r==`(matrix(c(1,1,   1,2,   2,2), nrow=2))

        [,1]  [,2] [,3]

        [1,] TRUE FALSE TRUE'

crep <- function (x, times)
        matrix(x, nrow=x %|% `#`, ncol=times)

rrep <- function (x, times)
        matrix(x, ncol=x %|% `#`, nrow=times, byrow=T)

    Doc$crep <- '
        crep returns a matrix of times (arg 2) columns whose
        elements match x (arg 1).'

    Doc$rrep <- '
        rrep returns a matrix of times (arg 2) rows whose
        elements match x (arg 1).'

m.trace <- function(nrow, ndim=2)
        crep(1:nrow, ndim)

    Doc$m.trace <- '
        m.trace returns a matrix-index int an ndim (arg
        2)-dimensional array. For a matrix, the indices cover
        the trace of the matrix.'

m.identity <- function (n) {
    m <- matrix(0L, nrow=n, ncol=n)
    m[n %|% m.trace] <- 1L
    m }

    Doc$m.identity <- '
        m.identity returns an n x n identity matrix, where n is
    arg 1.'

crest <- function (m) reproduce(m,   , -1)
rrest <- function (m) reproduce(m, -1,   )

        Doc$crest <- '
            crest, "cee rest," returns a copy of the matrix
            argument, without the first column.'

        Doc$rrest <- '
            rrest, "are rest," returns a copy of the matrix
            argument, without the first row.'

cswap <- function (m)
        cols(m, m %|% colNos %|% swap)

        Doc$cswap <- '
            cswap, "cee swap," returns a modified copy of the
            matrix argument, where the first two columns have
            been swapped.'

rswap <- t %O% cswap %O% t

    Doc$rswap <- '
        rswap returns a copy of the matrix argument with its
        first two rows swapped.'

m.lines <- function(lines, byrow=T, ...) {
    strings <- lapply(lines, words)
    matrix(strings %|% unlist, nrow=strings %|% `#`, byrow=byrow) }

m.lines2 <- function(lines, .partition=words)
        vapply2(lines, .partition) %|% t

    Doc$m.lines <- '
        m.lines and m.lines convert lines of text (as a character vector)
        into a matrix of character strings. Arg 1 is the lines of text.
        m.lines splits words by whitespace. m.lines2 splits the lines
        according to .partition, which defaults to the same behavior as
        m.lines.'

m.h <- function(h, byrow=T, ...) matrix(h %|% words, byrow=byrow, ...)

xy.square. <- function() "0 0   1 0   1 1   0 1" %|% i.h %|% as_xy
xy.square <- function() {
    sq <- xy.square.();   rbind(sq, sq[1,]) }

    Doc$xy.square. <- '
        xy.square. returns a 4-row, xy matrix representing a
        square in the Cartesian plane with a corner at (0, 0)
        and remaining points proceeding counterclockwise.'

turn.m <- function (m) m[,rev(colNos.m(m)) ]
flip.m <- function (m) m[ rev(rowNos.m(m)),]

i.positive.space <- function (b) which1(b) %,% which1rev(b)

i.positive.space.y.m <- function (m)
    capply(m, is.na %O% all %O% `!`) % % i.positive.space

i.positive.space.x.m <- t %O% i.positive.space.y.m

ij.positive.space.m <- function (m)
    matrix(i.positive.space.x.m(m) %,% i.positive.space.y.m(m), ncol=2)

m.positive.space.x.m <- function (m) reproduce(m, 
        do.call(seq, m  % %  i.positive.space.x.m  % % as.list),)

m.positive.space.y.m <- t %O% m.positive.space.x.m %O% t

m.positive.space.m <- m.positive.space.x.m %O% m.positive.space.y.m


'
funciton turn.m : matrix reflection column

        Returns a copy of the argument with reversed columns.
        The name of the function alludes to the turning of a
        page of a book with binding on the one side.

funciton flip.m : matrix reflection row

        Returns a copy of the argument with reversed rows.
        The name of the function alludes to the flipping of a
        page of a book with binding on the top.

function xy.square : matrix plot

        Returns a matrix suitable for plotting a square.
        Example: plot(xy.square())

function xy.square. : matrix plot

        Returns a matrix suitable for plotting a square
        arrangement of dots (note the dot at the end of the name
        cf.  xy.square). Example: plot(xy.square.())

function m.h : character matrix

        Returns a character matrix given a character string.
        Note the default for byrow is opposite that of matrix.
        Example:
                > m.h("1 one 2 two 3 three", ncol=2)
                     [,1] [,2]   
                [1,] "1"  "one"  
                [2,] "2"  "two"  
                [3,] "3"  "three"

function m.lines : matrix character vector

        Returns a character matrix given a character vector.
        Each character string of the character vector
        corresponds to a row (if byrow) or a column (if !byrow)
        of the return.  Note the default for byrow is opposite
        that of matrix.  Example:
                > m.lines("A B" %,% "1 2") # OR: m.lines(lines.h1("A B\n1 2\n"))
                     [,1] [,2]
                [1,] "A"  "B" 
                [2,] "1"  "2"

function crest : matrix columns rest

        see Doc$crest

function cswap : matrix columns swap

        see Doc$cswap

function i.positive.space
function i.positive.space.y.m
function i.positive.space.x.m
function ij.positive.space.m
function m.positive.space.x.m
function m.positive.space.y.m
function m.positive.space.m

        Originally developed as a component for cropping image
        matrices to the data area.

function i.positive.space : image matrix vector

        Returns the indices of the first and last TRUE-values in
        the logical vector argument.

        Example:

                > i.positive.space(c(NA, F, T, F, NA))
                [1] 3 3
        
function i.positive.space.x.m
function i.positive.space.y.m

        See also i.positive.space

        Returns the row (x) or column (y) indices of the first
        and last columns or rows which contain at least one
        non-NA value.
 
function ij.positive.space.m

        Returns a matrix of indices. The indices tell the first
        and last row or column which contain at least one non-NA
        value.

function m.positive.space.x.m
function m.positive.space.y.m
function m.positive.space.m

        Returns the portion of the matrix argument that contains
        positive space (at least one non-NA value) in the x-, y-,
        or both directions. Example:

                > m
                     [,1] [,2] [,3]
                [1,]   NA   NA   NA
                [2,]   NA    1   NA
                [3,]   NA   NA   NA
                > m.positive.space.m(m)
                     [,1]
                [1,]    1

'

Doc$flip.m <- ("flip.m returns a copy of the argument with its columns reversed.")
