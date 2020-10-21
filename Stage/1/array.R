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



.corner <- function(a, .dim)
	    block.extract.a(a, rbind(L, .dim))

corner <- function(a, .dim=vapply2(a %|% dim, function(y) y %,% 6L %|% min)) {
        LIMLEN <- .dim %|% `#`
        NDIM <- a %|% ndim
        .dim <- if (LIMLEN == 1) {
            rep(.dim, NDIM)
        } else if (LIMLEN != NDIM) {
            extend_len(.dim, length.out=NDIM, values=1)
        } else {
            .dim }
        .corner(a, .dim) }

    Doc$corner <- '
        corner is similar to head. It returns the first few
        elements of an array in each direction according to .dim.'

'
stack #DELETED USE stack.
'
stack. <- function(arrays, FUN)
		data_frame %=>% do.call %:|% lapply(arrays, as.vector) %|%
		anon %=>% dfapply %:|% FUN %=>% dimension %:|% dim(arrays %|% first)

    Doc$stack. <- '
        stack applies FUN (arg 2) to the corresponding elements of the list of arrays (arg 1).
        The arrays must each have the same number of elements.
        The number of parameters of FUN must be equal to the number of arrays;
        the arguments applied to FUN are taken from the elments of the arrays, in the order list-ed.
        
        fn1 <- function(a, b) 2 * a + b

        m1 <- matrix(1:4, 2)

        m2 <- matrix(1:4/10, 2)

        m1

        #      [,1] [,2]

        # [1,]    1    3

        # [2,]    2    4

        m2

        # [,1] [,2]

        # [1,]  0.1  0.3

        # [2,]  0.2  0.4

        stack.(list(m1, m2), fn1)

        #      [,1] [,2]

        # [1,]  2.1  6.3

        # [2,]  4.2  8.4

        
        '




rcapply <- function(a, FUN, FUN.VALUE=NULL, ndim=2L) {
	DIMI <- dim(a)[ndim %|% seq]
	SPAN <- DIMI %|% iprod
	extract <- (
		function(a, pdim, k) dimension(
			a[k %|% pred * SPAN + SPAN %|% seq],
			pdim)) %<=% a %<=% DIMI
	if (FUN.VALUE %|% is.null)
			FUN.VALUE <- 1 %|% extract %|% FUN
	vapply(
		a %|% `#` %/% SPAN %|% seq,
		extract %O% FUN,
		FUN.VALUE) }

	Doc$rcapply <- '
		rcapply operates on each successive subarray
		of the array (arg 1). Conceptually, subarrays
		are contiguous and together form the original
		array. What constitutes a subarray is determined
		by the number of dimensions (arg 4, ndim) given.
		FUN (arg 2) and FUN.VALUE (arg 3) are analogous
		to the same-named parameters of vapply. If
		FUN.VALUE is not specified, it is assumed to be
		equal to the return of FUN, called with the first
		subarray.
        
        rcapply(Nbrs[,,1:6], identity)

        rcapply(Nbrs[,,1:6], t)

        dimension(rcapply(Nbrs[,,1:6], t), c(2, 24)) %|% t
        '

range.check.mi <- function(.dim, mi)
		rapply.(mi, between %<=% 1L %<=% .dim %O% all)

	Doc$range.check.mi <- '
		range.check.mi returns a logical vector that
		indicates whether each of the the matrix indices
		of the index matrix (arg 2) is valid for an array
		of the given dimensions (arg 1).'

range.checked.mi <- function(.dim, mi)
        mi[range.check.mi(.dim, mi), , drop=F]
 




arrayInd.corners. <- function (.dim)
        rapply.(
           .dim %|% `#` %|% rep2 %|% arrayInd.every,
           .dim %|% arrayInd.range %=>% `%[ZZt%`,
           .dim %|% `#` %|% integer) %|% t

    Doc$arrayInd.corners. <- '
        arrayInd.corners. is similar to arrayInd.corners, except
        for the **** ASSUMPTION **** that the dimensions are at
        least 2 in every direction. If this assumption does not
        hold, the return will contain duplicates.'

arrayInd.corners <- placeholder

    Doc$arrayInd.corners <- '
        arrayInd.corners (FUTURE) returns the indices of the
        corners of an array or vector.'

arrayInd1 <- arrayInd %<=% 1L

array.spec.contiguous <- function (li)
        lapply(li, function(x) swmatch %<=% x)

    Doc$array.spec.contiguous <- '
        array.spec.contiguous is designed to return a list of
        functions that may be used for array indexing:

        The argument is a vector or list of values.

        The return is a list of functions that each map the
        values of the corresponding argument element to the
        first few natural numbers.

        see also *iarray--proto--test.R

        > specs <- array.spec.contiguous(
            list(
                c("ZERO", "ONE", "TWO"),
                c(F, T, NA)))

        > specs[[2]](NA)

        [1] 3

        > specs[[2]](T)

        [1] 2

        > specs[[2]](T)> specs[[1]]("ZERO")

        [1] 1'

#APPEND ONLY! DESIGNED FOR CHARACTER ARRAYS ONLY!
write.a <- function(x, file=stdout(), ncolumns=1L, sep="") {
	N <- (if (x %|% is.array) nrow else `#`)(x)
	for (i in x %|% `#` %/% N %|% seq - 1L)
		write(
			x[N %|% seq + N * i],
			file=file, ncolumns=ncolumns, append=T, sep=sep) }

Doc$write.a <- '
    write.a **** APPENDS **** the array argument (arg1) to
    the file (arg2) specified. No more than ncolumns elements
    are written to each line of the text output. Blocks of text
    representing columns of the array will be separated by
    newline characters.

    write.a(letters, ncolumns=5, file("filename", open="w"))

    will result in the file filename being created / overwritten
    with the contents:

    abcde*fghij*klmno*pqrst*uvwxy*z*

    where * is an end-of-line sequence

    see also write.table.bare.
    '

strsplit2 <- strsplitbynewline %O% first %O% (lapply %|% argswap %<=% words)

    Doc$strsplit2 <- '
        strsplit2 returns a list of character vectors formed by
        splitting the string argument into lines (splitting by
        new-line), then splits each line by whitespace.'

unragged <- function (l)
        do.call(
            data.frame %O% anon,
            lapply(l, vapply2(l, `#`) %|% max %=:% "length.out" %v% extend_len))

    Doc$unragged <- '
        unragged returns a data frame formed from a list of
        atomic vectors. Columns of the return correspond with
        items of the argument. Atomic vectors shorter than the
        longest atomic vector are padded with NA-values.'

character.matrix <- strsplit2 %O% unragged %O% as.matrix %O% t

    Doc$character.matrix <- '
        character.matrix returns a character matrix formed from
        the words of the multiline string argument.

        character.matrix(
            "   NA  NA  NA  NA
                F   0L  0   A
                T   1L  1   B       ")'

populated <- un(is.na) %O% any

connection.open.mode <- function (options) { # text=T, read=T, write=F, append=F, truncate=F) {
    #stop("IN PROGRESS")
    if (missing(options)) return ("") # DEFAULT FOR file, url, ETC.
    #OPTIONS <- list(...) %|% unlist
    #pmatch. <- pmatch %<=% OPTIONS
    #if ("binary" %|% pmatch.)
    ##MODE <- c(text, read, write, append, truncate) 
    #MASTER.TABLE <- character.matrix(
        ##   TEXT    READ    WRITE   APPEND  TRUNC   MODE
        #'   T       T       F       F       F       rt
            #T       F       T       F       F       wt
            #T       F       F       T       F       at
            #F       T       F       F       F       rb
            #F       F       T       F       F       wb
            #F       F       F       T       F       ab
            #T       T       T       F       F       r+
            #F       T       T       F       F       r+b
            #T       T       T       F       T       w+
            #F       T       T       F       T       w+b
            #T       T       F       T       F       a+
            #F       T       F       T       F       a+b')
#print(MASTER.TABLE)
    #TABLE <- MASTER.TABLE %|% cexcept.last %|% as_logical.storage.mode %|% t %|% as.data.frame %|% as.list
    #names(TABLE) <- MASTER.TABLE %|% lastc
#print(TABLE)
}
#connection.open.mode()

#.NEW.write.a <- function (x, connection, ncolumns, sep) {
    #write(x, file=connection, ncolumns=ncolumns, sep=sep)
     #}
#
#.NEW.write.a <- function(x, file=NULL, ncolumns=NULL, append=T, sep="") {
#
    #if (!append) write(vector(), file=file)
#
	#for (i in x %|% ncol %|% seq)
		    #write(x[,i], file=con, ncolumns=ncolumns, sep=sep)



offset. <- function(X, shift=0L)
	lapply(
        if (X %|% is.list) X %|% seq_along else 1L,
		function(i) shift %[mod% i + enlist(X)[[i]])

    Doc$offset. <- '
        offset. returns a list containing modified versions of
        arg1, which is a (list of) numeric vector(s).  The
        modification is to add (each of) the value(s) specified
        in the shift vector to the corresponding vector element
        of arg1. The shifts are recycled to obtain an effective
        shift vector that is the same length as arg1. Originally
        intended to compute global array indices given local
        indices and an offset.'

replace.a.at <- function (a, b, where) {
    if (a %|% dim %|% is.null) dim(a) <- a %|% `#`
    if (b %|% dim %|% is.null) dim(b) <- b %|% `#`
    INCOMPATIBLE.DIMENSIONS <- a %|% ndim != b %|% ndim
    if (INCOMPATIBLE.DIMENSIONS)
            'Number of dimensions of primary and secondary
            arguments are incompatible.' %|% crunch.h %|%
            warning
    if (
        INCOMPATIBLE.DIMENSIONS |
        b %|% is.empty | # b HAS NO ELMENTS
        any(a %|% dim < where) # a DOES NOT EXTEND TO where
    ) {
        a 
    } else {
        replace.a(
            a,
            list.=lapply(
                where %|% seq_along,
                function(i)
                        where[i]:min(
                            dim(a)[i],
                                    # LIMIT BY DIMENSIONS OF a
                            where[i] + dim(b)[i] - 1L
                                    # LIMIT BY DIMENSIONS OF b
                        ) ),
            values=extract.a(
                b,
                lapply(
                    b %|% dim %|% seq_along,
                    function(i)
                        min(
                            dim(a)[i] - where[i] + 1L,
                                    # LIMIT BY DIMENSIONS OF a
                            dim(b)[i]
                                    # LIMIT BY DIMENSIONS OF b
                        ) %|% seq)
                    ) ) } }

dimension.irange <- function (irange) #TAGS vector index indices
        1L + irange[2] - irange[1]

    Doc$dimension.irange <- '
        Given an integer range, c(m, n), m <= n, returns the
        number of elements in a corresponding hypothetical
        vector.'

as_array_of <- function(a, mode="logical")
        dimension(as.vector(a, mode=mode), a  % %  dim)

'
function as_array_of : convert

        Similar to as.vector, but the result has the same
        dimensions as the primary argument

'

