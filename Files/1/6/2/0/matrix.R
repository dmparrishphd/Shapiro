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


write.table.bare <-
        write.table %^% list(quote=F, row.names=F, col.names=F)

    Doc$write.table.bare <- '

        Specialization of write.table: no quoted strings, no row
        names, no column names.

        Originally intended to write data frames that consist of
        completely of character vectors (which may represent
        formatted numeric data).

        Typical usage: write.table.bare(dataframe1, filename)
        '

.mb.preplaid <- function(nrow, ncol) {
	XX <- F %,% T
	Mats <- array(F, c(nrow, ncol, 2L))
	Mats[,,1][  rep(XX, length.out=nrow), ] <- T
	Mats[,,2][, rep(XX, length.out=ncol)  ] <- T
	Mats }

.m.plaid <- function(preplaid) {
	Return <- array(F, preplaid[,,1] %|% dim)
	Return[    preplaid[,,1] & preplaid[,,2] ] <-  T
	Return[xor(preplaid[,,1],  preplaid[,,2])] <- NA
	Return }

plaid <- function(nrow, ncol=nrow)
		.mb.preplaid(nrow, ncol) %|% .m.plaid

checkered <- function(nrow, ncol=nrow) {
	Return <- plaid(nrow, ncol)
	Return[Return] <- F
	Return[Return %|% is.na] <- T
	Return }

    Doc$plaid <- '
        checkered and plaid return logical matrices of checkered or
        plaid patterns (checkered is 2-valued and plaid is 3-valued).
        
        Tip: `!` or swap.values can be used to rearrange values.
        
        originaly intended to be used as translucent image masks as
        an aid to the visualization of rasters.
        
        n <- 8
        image. <- image %^% list(x=0:n, y=0:n, useRaster=T, bty="n")
 pdf(file="T:/graf.pdf", width=8.5, height=11)
        par.mai.mutate(bottom=3.75, top=1.25, left=1.25, right=1.25)
        image.(asp=1,
               matrix(1:4, nrow=n, ncol=n),
               ylab="", main="When Obfuscation Reveals",
               xlab="An overlay of translucent grey reveals the structure of the underlying raster.",
               col=terrain.colors(4))
        image.(add=T, checkered(n), col=TRANSPARENT %,% TRANSLUCENT[1])
        box()
 dev.off()

        '



runbind <- function(x, i=0) {
	if (!i) i <- x %|% nrow %/% 2L
	list(x[seq(i),], x[-seq(i),]) }

cunbind <- function(x, i=0) {
	if (!i) i <- x %|% ncol %/% 2L
	list(x[,seq(i)], x[,-seq(i)]) }

	Doc$cunbind <- '
		cunbind and runbind are somewhat the opposite
		of cbind and rbind. The return is a list of
		two matrices which might be c- or r-bind-ed
		together to form a copy of the argument.'

	Doc$runbind <- Doc$cunbind

MB0 <- matrix(logical(), nrow=0, ncol=0)

    Doc$MB0 <- '
        M0X0 is a zero-by-zero logical matrix.

        Originally created to be used as a stand-in where a
        matrix is required, e.g., for the image function.'

specialize("rbind", n=1) #CREATES `%rbind%`
specialize("cbind", n=1) #CREATES `%cbind%`

m.indices <- function (dims) #TAGS index matrix array
        arrayInd(dims %|% prod %|% seq, dims)

m.indices.a <- function (a) #TAGS index matrix array
        m.indices(a %|% dim)

    Doc$m.indices <- '
        m.indices returns a matrix index whose rows contain
        every n-dimensional index of the array whose dimensions are
        specified by the argument.'

    Doc$m.indices.a <- '
        m.indices.a is similar to m.indices, except that the
        argument is an array rather than the dimensions of an
        array.'

m. <- function() matrix(1:6, nrow=2)

    Doc$m. <- '
        m. returns a 2-row matrix with the elements 1:6.
        Intended for use in testing.'

.det2 <- function(m)
        m[-(2:3)] %|% prod - m[2:3] %|% prod

det2 <- function (m) #TAGS determinant
        .det2(m[1:2, 1:2]) #REMINDER: ISING `[` THIS WAY SHOULD RAISE ERROR IF DIMENSIONS INCORRECT

    Doc$det2 <- '
        det2 computes the determinant of a 2x2 matrix using a
        naive algorithm.
        
        For integer matrices, **** FAILS ON OVERFLOW ****.
        
        Originally intended to produce an exact
        result with integer matrices (unline the built-in det),
        provided that overflow does not occur at any
        (intermediate) stage.'

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

'
rest.m #DELETED USE crest OR rrest
#WAS rest.m <- function(m) m[1, , drop=F]
'

m.FUN.m..v. <- function(FUN) #DEPRECATED USE FUN
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
        m.add.m..v for an example.
        
        **** WARNING **** Expect strange results if the number
        of elements of the vector argument is not aligned with
        the matrix argument.'

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
`%[ZZt%` <- function(m, i) m[cbind(i, seq(ncol(m)))]
'
`%[ZZ%` #TAGS indexing matrix zigzag christmas tree extract
'   
    Doc$`%[ZZ%` <- '
        The %[ZZ% infix operator takes a matrix m for the
        left-operand and a vector j for the right operand.  The
        return is a vector of values m[1, j1], m[2, j2], ...
        m[n, jn].'

    Doc$`%[ZZt%` <- '
        `%[ZZt%` is similar to `%[ZZ%`, except that the arg 2
        specifies the row rather than the column.  The return is
        a vector of values m[i1, 1], m[i2, 2], ...  m[in, n].'

n.row.m <- nrow # function DEPRECATED. Use nrow
n.col.m <- ncol # function DEPRECATED. Use ncol

row.m <- function(m, n) m[n,]
col.m <- function(m, n) m[,n]

firstc  <- col.m %|% argswap %<=% 1L
secondc <- col.m %|% argswap %<=% 2L
first2c <- function(m) (m %|% firstc) %cbind% (m %|% secondc)

lastc <- function (m) #TAGS column matrix data frame extract
        col.m(m, m %|% ncol)

firstr  <- row.m %|% argswap %<=% 1L
secondr <- row.m %|% argswap %<=% 2L
first2r <- function(m)
        (m %|% firstr) %rbind% (m %|% secondr)

lastr <- function (X) #TAGS row matrix data frame extract
        X[X %|% nrow,]

last.row <- lastr #DEPRECATED USE lastr

    Doc$lastr <- '
        lastr returns the last row of the matrix or data frame argument. '


    Doc$firstc <- '
        firstc returns the first column of the matrix argument.'

    Doc$secondc <- '
        secondc returns the second column of the matrix argument.'

outerr <- function (m)
        (m %|% firstr) %rbind% (m %|% lastr)

    Doc$outerr <- '
        outerr returns the first and last rows (the "outer"
        rows) of the matrix or data frame argument (the "outer"
        portion) as a matrix or data frame.'

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
