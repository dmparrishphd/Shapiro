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

rest.m <- function(m) m[1, , drop=F]

m.FUN.m..v <- function(FUN)
        function(m, v) FUN(m %|% t, v) %|% t

    Doc$m.FUN.m..v <- '
        m.FUN.m..v returns a function that takes a matrix as its
        primary argument and a vector whose length is equal to
        number of columns of the matrix. The return of the
        returned function is a matrix of the same dimensions as
        the primary argument of the return. See m.add.m..v for
        an example.'

m.add.m..v <- m.FUN.m..v(`+`)

`%m<=v%` <- m.FUN.m..v(`<=`)
`%m>=v%` <- m.FUN.m..v(`>=`)

`%m+v%` <- m.FUN.m..v(`+`)
`%m-v%` <- m.FUN.m..v(`-`)
`%m*v%` <- m.FUN.m..v(`*`)
`%m/v%` <- m.FUN.m..v(`/`)



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

firstr  <- row.m %|% argswap %<=% 1L
secondr <- row.m %|% argswap %<=% 2L

rename.all.columns.m <- function (m, newnames=NULL) {
    colnames(m) <- newnames;   m }

rename.all.rows.m <- function (m, newnames=NULL) {
    rownames(m) <- newnames;   m }

m.rep <- function (x=NA, ncol=1) matrix(rep(x, ncol), ncol=ncol)

rep_m <- function (m, times=1) matrix(
	rep(m  % %  t, times), ncol=ncol(m), byrow=T)

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

m.identity <- function (n)
        matrix(c(rep(c(1, rep(0L, n)), n - 1L), 1L), n)

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
        
function m.rep : vector repeat columns matrix

        Returns a matrix of ncol columns where the columns have
        values according to the vector x. Example:
                > m.rep(-1:1, 3)
                     [,1] [,2] [,3]
                [1,]   -1   -1   -1
                [2,]    0    0    0
                [3,]    1    1    1

'
