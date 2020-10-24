# Shapiro: A Handsome Helper for R
# Copyright (C) 2018 D. Michael Parrish
# 
# This program is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of
# the License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public
# License along with this program.  If not, see
# <https://www.gnu.org/licenses/>.
#
# END OF COPYRIGHT NOTICE

'
rapply #DELETED (AVOID CONFLICT WITH base::rapply). USE `rapply.`.
'

rapply.  <- function (m, FUN, FUN.VALUE=NULL, ...)
        vapply(
            m %|% rowNos,
            function (i, m, ...) FUN(m[i,], ...),
            if (FUN.VALUE %|% is.null) FUN(m[1,], ...) else FUN.VALUE,
            m,
            ...)

    Doc$rapply. <- '
        rapply. is similar to capply, but operates on rows of the
        primary, matrix argument, rather than columns'

arrayIndInv <- function(mi, .dim)
        rapply.(mi %|% pred %m*v% index.factors.dim(.dim), sum %O% succ)
        #REMINDER: pred AND succ MAKE THE ADJUSTMENT NECESSARY
        #       FOR INDEXING FROM 1 INSTEAD OF FROM 0.

    Doc$arrayIndInv <- '
        arrayIndInv is the inverse of arrayInd. That is, given
        an index matrix and the dimensions of a corresponding
        array, arrayIndInv returns the corresponding integer
        index vector.'

a.vapply.li <- function(X, FUN) {
    i <- vapply(X, `#`, 1 %|% integer) %|% ai.every
    dimension(
        rapply.(i, FUN %|% argswap, FUN.VALUE=(FUN %|% argswap)(i[1,], X), X),
        FUN(X, i[1,]) %|% dim. %,% nrow(i) %|% idrop) }

    Doc$a.vapply.li <- '
        a.vapply.li is similar to vapply except that
        X (arg 1) is a list whose items are indexed internally.
        Every combination of the elements of X are applied to FUN (arg 2), a function of
        X (arg 1 of a.vapply.li) and an integer vector index specifying particular components of X.

        It may be convenient to use multiple.extract within FUN.
        
        
        a.vapply.li(
            list(LETTERS[1:3], 1:2),
            function(X, i) c(X[[1]][i[1]], X[[2]][i[2]]))

                [,1] [,2] [,3] [,4] [,5] [,6]

            [1,] "A"  "B"  "C"  "A"  "B"  "C" 

            [2,] "1"  "1"  "1"  "2"  "2"  "2" 

        a.vapply.li(
            list(1:3, 1:3),
            function(X, i) c(X[[1]][i[1]], X[[2]][i[2]]))


        # A MULTIPLICATION TABLE
        m <- 3; n <- 3
        dimension(
                  a.vapply.li(
            list(1:m, 1:n),
            function(X, i) prod(c(X[[1]][i[1]], X[[2]][i[2]]))),
                  c(m, n))



            '

        

lrapply <- function(m, FUN, ...)
        lapply(
            m %|% rowNos,
            function(k) FUN(m[k,], ...) )

    Doc$lrapply <- '
        lrapply returns a list formed by the returns of FUN
        (arg 2), applied to each row of m (arg 1). `...` is
        passed to FUN, as with lapply.'
#234567890123456789012345678901234567890123456789012345678901234
