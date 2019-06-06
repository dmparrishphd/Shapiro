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
#
#


m. <- function() matrix(1:6, nrow=2)

    Doc$m. <- '
        m. returns a 2-row matrix with the elements 1:6.
        Intended for use in testing.'

matrix. <- function (data=NA, nrow=1) {
     }

det2 <- function (m)
        m[1] * m[4] - m[2] * m[3]

    Doc$det2 <- '
        det2 computes the determinant of a 2x2 matrix using a
        naive algorithm.
        
        For integer matrices, **** FAILS ON OVERFLOW ****.
        
        Originally intended to produce an exact
        result with integer matrices, provided that overflow
        does not occur at any (intermediate) stage.'

pow.1 <- function (n)
        1L - n %% 2L * 2L

    Doc$pow.1 <- '
        pow.1 returns powers of -1 for an integer vector
        argument. Intended for use with computing trucated
        series, determinants, etc.'

submatrix1 <- function (m, i) #TAGS strike determinant extract
        m[-i[1], -i[2]]

submatrix3 <- function (m, i, j) #TAGS strike determinant extract
        m[-i, -j]

submatrix2 <- function (m, ij) #TAGS strike determinant extract
        m[-ij[[1]], -ij[[2]]]

det3 <- function (m)
        sum(m[1,]   *   c(1L, -1L, 1L)   *   vapply(
            1:3, submatrix3 %<=% m %<=% 1 %O% det2,
                    m %|% typeof %|% vector1))

    Doc$det3 <- '
        det3 computes the determinant of a 3x3 matrix using a
        naive algorithm. Originally intended to produce an exact
        result with integer matrices, provided that overflow
        does not occur at any (intermediate) stage.'

matrix2  <- list() %,% 2 %=:% "ncol" %v% matrix
matrix2r <- list() %,% 2 %=:% "nrow" %v% matrix

    Doc$matrix2 <- '
        matrix2 is a curried version of matrix, where ncol is
        fixed at 2.'

    Doc$matrix2r <- '
        matrix2r is a curried version of matrix, where nrow is
        fixed at 2.'

m.empty <- function(nrow=0, ncol=0)
		matrix(nrow=nrow, ncol=ncol)

    Doc$m.empty <- '
        m.empty returns an empty matrix with the number rows or
        columns specified, provided that either nrow == 0 or ncol ==
        0, consistent with defaults.'

`4x4` <- matrix %^% list(nrow=4, ncol=4) #TAGS transformation matrix geometry afine linear
`4x4 t` <- `4x4` %O% t #TAGS transformation matrix geometry afine linear

cdiff <- t %O% diff %O% t

rest.m <- function(m) m[1, , drop=F]

m.FUN.m..v. <- function(FUN)
        function(m, v) FUN(m, v)

m.FUN.m..v <- function(FUN)
        function(m, v) FUN(m %|% t, v) %|% t
                # THIS WORKS BECAUSE OF RECYCLING

    Doc$m.FUN.m..v <- '
        m.FUN.m..v (m.FUN.m..v.) returns a function that takes a
        matrix as its primary argument and a vector whose length is
        equal to the number of columns (rows) of the matrix. The
        return of the returned function is a matrix of the same
        dimensions as the primary argument of the return. See
        m.add.m..v for an example.'

m.add.m..v <- m.FUN.m..v(`+`)

`%m<=v%` <- m.FUN.m..v(`<=`)
`%m>=v%` <- m.FUN.m..v(`>=`)

`%m+v%` <- m.FUN.m..v(`+`)
`%m-v%` <- m.FUN.m..v(`-`)
`%m*v%` <- m.FUN.m..v(`*`)
`%m/v%` <- m.FUN.m..v(`/`)

`%m==v.%` <- m.FUN.m..v.(`==`)
`%m+v.%` <- m.FUN.m..v.(`+`)



`%[ZZ%` <- function(m, j) m[cbind(seq(nrow(m)), j)]
'
`%[ZZ%` #TAGS indexing matrix zigzag christmas tree extract
'   
    Doc$`%[ZZ%` <- '
        The %[ZZ% infix operator takes a matrix m for the
        left-operand and a vector j for the right-operand.  The
        return is a vector of values m[1, j1], m[2, j2], ...
        m[n, jn].'


n.row.m <- nrow # function DEPRECATED. Use nrow
n.col.m <- ncol # function DEPRECATED. Use ncol

row.m <- function(m, n) m[n,]
col.m <- function(m, n) m[,n]

firstc  <- col.m %|% argswap %<=% 1L
secondc <- col.m %|% argswap %<=% 2L
first2c <- function(m)
        do.call(cbind, lapply1to1(
            m %|% list %|% rep2, firstc %,% secondc))

firstr  <- row.m %|% argswap %<=% 1L
secondr <- row.m %|% argswap %<=% 2L
first2r <- function(m)
        do.call(rbind, lapply1to1(
            m %|% list %|% rep2, firstr %,% secondr))

rename.all.columns.m <- function (m, newnames=NULL) {
    colnames(m) <- newnames;   m }

rename.all.rows.m <- function (m, newnames=NULL) {
    rownames(m) <- newnames;   m }

m.rep <- function (x=NA, ncol=1) #TAGS vector repeat columns matrix
        matrix(rep(x, ncol), ncol=ncol)
    Doc$m.rep <- '
        m.rep returns a matrix of ncol (arg 2) columns where the
        columns have values according to the vector x (arg 1).

        > m.rep(-1:1, 3)

                [,1] [,2] [,3]
        [1,]   -1   -1   -1
        [2,]    0    0    0
        [3,]    1    1    1'

rep_m <- function (m, times=1) matrix(
	rep(m %|% t, times), ncol=ncol(m), byrow=T)

seqnD <- function(dims, SEQ=integer()) {
	if (SEQ  % %  length) {
		if (dims  % %  length) seqnD(   # recurse with lower dimensionality
			dims[-1],
			cbind(
				rep_m(SEQ, dims[1]),
				rep(dims[1] % % seq,   SEQ  % %  nrow)  % %  sort)   # next iteration of SEQ
		) else SEQ
	} else   # SEQ has not yet begun to be built up.
		if (dims  % %  length) {
			seqnD(dims[-1],   dims[1]  % %  seq  % %  matrix)
		} else integer() }

as_xy <- function (xy) matrix(xy, ncol=2, byrow=T)

M1 <- matrix(1L)
MZ <- matrix()
MT <- matrix(T)
MF <- ! MT

'
function as_xy : matrix plot index

        Forms an nx2 matrix from the vector argument of length
        2n.  Example:

                > as_xy(1:4)
                     [,1] [,2]
                [1,]    1    2
                [2,]    3    4

        The resultant could represent a set of Cartesian ordered
        pairs accomodated by plot, or a set of two-dimensional
        indices accomodated by `[`.

function rep_m : rep matrix

        Returns a matrix containing times copies of the matrix
        argument (arg 1). Each additional copy is found "below"
        the original. Example:
                > rep_m(matrix(1), 2)
                     [,1]
                [1,]    1
                [2,]    1
'
