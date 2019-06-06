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
### R Script

as.vector0 <- function (...)
    lapply(lapply(list(...), typeof), vector) %|% unlist

    Doc$as.vector0 <- '
        as.vector0 returns a zero-length vector formed from the
        arguments. Intended to produce information that may be
        needed in order to combine vectors of different types.'

max_typeof <- as.vector0 %O% typeof

    Doc$max_typeof <- '
        max_typeof returns a character string indicating the
        type of vector needed to store the combinatin of the
        arguments.'

tc <- c %O% t #TAGS matrix row vector

    Doc$tc <- '
        tc returns a 1-row matrix formed from the vector argument.'

swmatch <- match %|% argswap

    Doc$swmatch <- '
        swmatch is inteded for currying. swmatch is equilvalent
        to match, except that the table argument comes first and
        the x argument comes second.

        > swmatch %<=% 0:9

'

`%==%` <- `==` %O% all

    Doc$`%==%` <- '
        `%==%` returns a single logical value indicating whether
        all of the corresponding elements of two vectors are
        equal.  Internally, recyling behavior of `==` may
        apply.'

`%!=%` <- `!=` %O% any

    Doc$`%!=%` <- '
        `%!=%` returns a single logical value indicating whether
        any of the corresponding elements of two vectors are
        unequal.  Internally, recyling behavior of `!=` may
        apply.'

`%!in%` <- `%in%` %O% `!` #TAGS not in

`#` <- length # function. cardinality, the number of items in an object

is.empty <- `#` %O% `!`

    Doc$is.empty <- '
        is.empty returns a logical value indicating whether the
        vector or list argument has zero elements.'

uniques <- unique %O% (list() %,% F %=:% "na.last" %v% sort)

found <- function () { #TAGS search names environment objects list
    NAMES <- search() %|% rev
    rename.all(lapply(NAMES, list(all=T) %v% ls), NAMES)
}

is_populated <- is.na %O% `!` #TAGS nullable empty complete record

    Doc$is_populated <- '
        is_populated returns a modified copy of the argument.
        Each element of the argument tells whether the
        corresponding element of the argument has been populated
        (i.e., is not NA)'

sans <- seq %O% `-` #TAGS indexing indices

    Doc$sans <- '
        sans is a wrapper for seq. The return is the negative of
        the corresponding return from seq. Originally intended
        to produce indices of vectors that will be used to
        specify every element EXCEPT those indicated by the
        return of sans. Example: (1:9)[sans(3)] returns the
        equivalent of 4:9.'

ibookends <- function (X)
        X %|% `#` %,% 1L %|% rev

    Doc$ibookends <- '
        ibookends returns the indices of the first and last
        elements of the vector or list argument.'

extend <- function(x, n=0, values=NA) x %,% rep_len(values, n)

    Doc$extend <- '
        extend returns a modified copy of the vector where n
        additional elements are added to the end. The values of
        the additional elements are taken from the values
        argument; values are recycled if necessary to yield
        n elements.'

extend_len <- function(x, length.out=x %|% `#`, values=NA, quiet=F) {
    lx <- x %|% `#`
    if (length.out < lx) {
        if (!quiet) warning("returning truncated version of input.")
        x[0:length.out]
    } else {
        extend(x, length.out - lx, values=values) } }

extend.to.length <- extend_len #DEPRECATED USE extend_len

    Doc$extend_len <- '
        extend.to.length is similar to extend, except that the
        length.out is specified rather than the number of items
        to be added. The return will be a truncated copy of the
        primary argument if the length.out specified is less
        than the length of the primary argument.'

    Doc$extend.to.length <- '
        extend.to.length is **** DEPRECATED. **** Use.
        extend_len.'

smooth.l <- function (X, pad=NA) #TAGS ragged array
        lapply(
            X,
            extend.to.length %^%
                    list(
                        length.out=vapply_(X, `#`) %,% 0L %|% max,
                        values=pad))

    Doc$smooth.l <- '
        smooth.l returns a modified copy of the list of vectors
        argument. The elements of the return match those of the
        input, except that they have each been extended to the
        length of the longest argument-element by appending
        values from pad (arg 2).'

m.l <- function (X, pad=NA) #TAGS matrix from list
    do.call(cbind, smooth.l %^% list(pad=pad) %-|% X)

    Doc$m.l <- '
        m.l returns a matrix formed from the list-of-vector
        argument. Vectors of the argument correspond with
        columns of the return. If necessary, values are taken
        from pad (arg 2) in order to ensure that all columns are
        of the same length.'

iNA <- NA_integer_

`!!` <- as.logical

    Doc$`!!` <- '
        The !! function is an alias for as.logical. Mnemonic:
        the ! operator takes the logical negative of the
        argument; applying ! twice would produce the logical
        identity.'

cbind_ <- function(...) cbind(..., deparse.level=0)
rbind_ <- function(...) rbind(..., deparse.level=0)

    Doc$cbind_ <- '
        cbind_ is a simplified version of cbind (deparse.level
        fixed at 0).'

    Doc$rbind_ <- '
        rbind_ is a simplified version of rbind (deparse.level
        fixed at 0).'

cbind_l <- function (l) do.call(cbind_, l)
rbind_l <- function (l) do.call(rbind_, l)

    Doc$cbind_l <- '
        cbind_l takes a list of items and cbinds them with
        deparse.level=0.'

    Doc$rbind_l <- '
        rbind_l takes a list of items and rbinds them with
        deparse.level=0.'

vector1 <- function(mode="logical", length=1L)
        vector(mode=mode, length=length)

    Doc$vector1 <- '
        vector1 is the same as vector, EXCEPT that the default
        for the length argument is 1L instead of 0L. Originally
        intended for use with vapply.'

vector2 <- function(mode="logical", length=2L)
        vector(mode=mode, length=length)

    Doc$vector2 <- '
        vector2 is the same as vector, EXCEPT that the default
        for the length argument is 2L instead of 0L. Originally
        intended for use with .first2.'

singleton <- function(x) vector1(mode=typeof(x))

    Doc$singleton <- '
        singleton returns a vector of length 1 that has the same
        mode (typeof) as the argument. Originally intended for
        use with vapply.'


NULLs <- function (n) rep(list(NULL), n)

restore <- function (singleton, alternate=NA) #TAGS NULL
        if (singleton %|% is.null) alternate else singleton

    Doc$restore <- '
        restore conditionally returns its primary argument
        unless that argument is NULL, in which case the
        alternate is returned. Reminder: "restore" and "annul"
        are antonymns.'



vapply_ <- function (X, FUN, ...) vapply(
    X, FUN,
    if (X %|% `#`) { lapply(X[1], FUN, ...)                 %|% first
    } else         { lapply(vector(typeof(X), 1), FUN, ...) %|% first },
    ..., USE.NAMES=F) 
                                           
    Doc$vapply_ <- '
        vapply_ is a simplified version of vapply, intended for
        FUN-s that do not have side efffects. FUN is evaluated
        TWICE for the first element of X---if X has elements---or
        ONCE for the empty vector if X is empty. In the latter
        case, a dummy vector of length 1 of type matching X and
        default value is used. In the underlying vapply, the
        USE.NAMES argument is fixed to FALSE and the FUN.VALUE
        is determined from applying X[1] to FUN, or, in the case
        where X is an empty vector, FUN.VALUE is determined by
        applying FUN to vector(typeof(X), 1).'

unnullify.l  <- function (list_, default=NA)
        lapply(list_, function(x) if (x %|% is.null) default else x)

default <- function (x, default) {
    x[x %|% is.na] <- default;   x }

# integer arithmetic

pred <- `+` %<=% -1L #TAGS predecessor subtract 1L integer -1 1-
succ <- `+` %<=% +1L #TAGS successor        add 1L integer +1 1+

diff_inclusive <- diff %O% succ

    Doc$diff_inclusive <- '
        diff_inclusive returns one more than the difference
        between the parts of the vector or matrix argument.

        > 1:10 %|% range %|% diff_inclusive
        [1] 10'

`%[-]%` <- `-` %O% succ

    Doc$`%[-]%` <- '
        %[-]% is similar to `-`, except that it returns one more
        than the difference. Intended for integer-valued
        arguments. May be useful when computing indices when
        indexing from 1.

        The symbolism is borrowed from interval notation, where
        the square bracket indicates an included value.
        
        cf. diff_inclusive. See also
        https://en.wikipedia.org/wiki/Counting#Inclusive_counting'.

# rearrangement

interlace <- function (...) rbind(...)  % %  as.vector

interlace1 <- function (list.of.parallel.vectors)
        do.call(rbind, list.of.parallel.vectors) %|% as.vector

unlace <- function(v, ncol=2) matrix(v, ncol=ncol, byrow=T)


'
unname #DELETED use base:unname
'


l.unlace <- function(l, ncol=2) lapply(
		ncol %|% seq,
		function(offset) as.list(l)[
            seq(l %|% `#` %/% ncol) %|% pred * ncol + offset])

l.unlace.l <- l.unlace

diff.abs <- `-` %O% abs

shuffle <- runif %O% order


is.singleton <- function (x) x  % %  `#` == 1L

.repx <- rep %^% list(length.out=NA, each=NA)
rep2 <- .repx %^% list(times=2)
rep3 <- .repx %^% list(times=3)
rep4 <- .repx %^% list(times=4)

rep_along <- function (x=F, along.with=1:2)
        rep_len(x, along.with %|% `#`)

    Doc$rep_along <- '
        rep_along returns a vector the same length as along.with
        (arg 2) whose elements are recycled from x (arg 1).'

b.rep <- rep %<=% F

    Doc$brep <- '
        b.rep is a curried version of rep where the value to be
        repeated is the logical (boolean) FALSE.'

b.sparse <- function (n, length.out=max(n)) {
        value <- length.out %|% b.rep
        value[n] <- T
        value }

    Doc$b.sparse <- '
        b.sparse returns a logical vector whose elements are all
        FALSE, except for the elements located at the indices
        specified by the primary argument, which are TRUE.'

centroids <- function (m) m - .5

ic <- function (...) c(...)  % %  as.integer

.lo <- -1:0 # MNEMONIC -10
ol   <-  0:1 # MNEMONIC:   01
lo <- ol %|% rev #MNEMONIC 10
.lol <- -1:1 # MNEMONIC: -101
.ll  <- c(-1, 1) # MNEMONIC: -11
l.l <- .ll %|% rev

absorb <- function(...) NULL

d.trunc <- function(d, precision=1024) #TAGS rounding
        trunc(precision * d) / precision

parallelize <- function(...)
        do.call(cbind, lapply(list(...), as.vector))

i.partition.n <- function (n) { #TAGS index indices
        if (n %|% is.numeric %|% `!`) return (3 %|% NULLs)
        if (n  < 1) return (list(integer(), integer(), NULL))
        if (n == 1) return (list(integer(),        1L,  -1L))
         list(
            n %|% pred %|% seq,
            n,
            n %|% sans) }

    Doc$i.partition.n <- '
        returns a list containing vector indices that can be
        used to partition a vector. See partition for an
        example.'

partition.proper  <- function(X, i) {
        if (        i < 1) return (list(NULL, NULL,    X))
        if (X %|% `#` < i) return (list(   X, NULL, NULL))
        lapply(i %|% i.partition.n, `[` %|% argswap,  X) }

    Doc$partition.proper <- '
        partition.proper returns a list containing the first
        part of the vector or list argument (arg 1), followed by
        the i-th (arg 2-th) elmeent, followed by the rest of arg 1.'

partition <- function(what, sep)
        partition.proper(what, match(sep[[1]], what, nomatch=0L))

    Doc$partition <- '
        partition. Provided that sep occurs in what, partition
        returns a list containing the portion of what that
        occurs before the firs ocurrence of sep in what,
        folllowed by sep, followed by the portion of what that
        occurs after the first occurrence of sep.
        
        EXAMPLE

        partition(c(F, T, F, T), T) returns the equivalent of
        list(F, T, c(F, T))'

squish <- function(v, from=0, to=1) from + (to - from) * v

#EDIT 2019-05-3: SWITCHED ORDER OF ARGUMENTS
na <- function (length=1L, mode="logical")
        as.vector(rep(NA, length), mode=mode)

brand <- function (n, compar=`<`, threshold=.5)
        compar(threshold, runif(n))

iunif <- function(n, min=1L, max=2L)
        min + as.integer(
            (1L + max - min) * vapply(
                runif(n),
                base::min %<=% (1 - .Machine$double.eps),
                1))

as.disjoint.segments <- function(v) # plot
        rbind(matrix(v, nrow=2), NA)  % %  as.vector

as.disjoint.segments.right <- function(v) # plot
        as.disjoint.segments(rbind(v, v))

### Functions Dependent on R only

evens <- function(n) # sequence
        2L * seq(n)

b.nas <- function (v) is.na(v)

which.na <- is.na %O% which

which0 <- `!` %O% which

which.zero <- which0 # function alias

which.nonzero <- function (v) which(!!v)

#naming

dub <- function(X, dfnames) {
	if (X %|% names %|% is.null)
		names(X) <- rep_along(HNULL, X)
	names(X)[dfnames[[1]]] <- dfnames[[2]]
	X }

are.named <- function (v) {
    noms <- v  % %  names
    if (noms  % %  is.null) vector(length=v  % %  `#`) else
            noms % %  nchar  % %  as.logical }

which.named <- function (v) v  % %  are.named  % %  which

which.unnamed <- function (v) v  % %  are.named  % %  `!`  % %  which

items.named <- function (v) v[v  % %  which.named]

items.unnamed <- function (v) v[v  % %  which.unnamed]


### anonymize

anonymize.v <- function (x) {
        names(x) <- NULL
    x }

anonymize.m <- function (x) {
    colnames(x) <- NULL
    rownames(x) <- NULL
    x }

anonymize.df <- function (x) {
        names(x) <- NULL
    row.names(x) <- NULL
    x }

.anonymize <- function(x)
        if (is.data.frame(x)) anonymize.df else
        if (is.list      (x)) anonymize.v  else
        if (is.vector    (x)) anonymize.v  else
        if (is.array     (x)) anonymize.m  else
                              identity

anonymize <- function(x) .anonymize(x)(x)

left <- function (character_, stop=1)
        substr(character_, 1, stop)

    Doc$left <- '
        left returns a character vector containing only the left
        substrings of the character vector argument. The number
        of individual characters in each element of the return
        is equal to the lesser of stop (arg 2) and the nchar of
        the corresponding character string of the primary
        argument.'

unleft <- function (character_, stop=1)
        substr(
            character_,
            start=1L + stop,
            stop=character_ %|% nchar)

    Doc$unleft <- '
        unleft is somewhat the opposite of left. The return is
        a modified copy of the character vector argument, with
        the left portion of each element REMOVED.'

initials <- function (h) vapply(h, left, "")  % %  anonymize

rest <- `[` %|% argswap %<=% -1
unrest <- function (x) x[-length(x)]
second <- rest %O% first
#second  <- function (x) x[[2]] #OLD  pre 2018-10-19
last  <- rev %O% first
last.row <- function (X)
        X[X %|% nrow,]
penultimate  <- rev %O% second
penult <- penultimate

# sequences

i.seq.range <- function (from_to) #TAGS sequence
        seq(from=from_to %|% first, to=from_to %|% second)

    Doc$i.seq.range <-
            "i.seq.range returns an integer sequence given an
            integer vector spanning the range (min and max)
            given.  Example: all(iseq.range(c(1, 7)) == 1:7) ==
            TRUE"

#

separate <- function(x, seps=HSPACE) interlace(x, rep_along(seps, x))  % %  unrest

fndefault <- function(x) function(y) if (y  % %  is.null) x else y

is_empty <- function (x) ! length(x)

identical.picky <- function (x, y) identical(x, y, F, F, F, F)

between  <- function (x, y, v) x <= v & v <= y
between2 <- function (interval, x) interval[1] <= x & x <= interval[2]
beyond2 <- function (interval, x) x < interval[1] | interval[2] < x

    Doc$between <- '
        between returns a logical vector that, for each element
        of v (arg 3), tells whether that element is between x
        (arg 1) and y (arg 2). The interval is considered to be
        **** INCLUSIVE AT BOTH ENDS. ****'

    Doc$between2 <- '
        between2 returns a logical vector that, for each element
        of x (arg 2), tells whether that element is in the
        interval (arg 1). The interval is considered to be
        **** INCLUSIVE AT BOTH ENDS. ****'

    Doc$beyond2 <- '
        beyond2 is analogous to between2, but returns the
        logically opposite result.'

among.indices.of <- function (v, i) between(1L, v  % %  `#`, i)

range.check <- function (dims, i)
        all(vapply(
				i %|% seq_along,
				function(k) between(1, dims[k], i[k]),
				T))

    Doc$range.check <- '
        range.check returns a logical value indicating whether
        the n-dimensional index (arg 2) is in the range of an
        array with dimensions dims (arg 1).'

extract_rev <- function(v, i, ...) rev(rev(v)[i, ...]) 
`%]%` <- extract_rev

extract_along <- function(x, i, na=NA) {
	y <- rep(na, length(i))
	tmp <- among.indices.of(x, i)
	y[which(tmp)] <- x[i[tmp]]
	y }

block.extract <- function(x, from, to, na=NA) {
	y <- rep(na, 1L + to - from)
    bt <- between(1L, length(x), from:to)
    for (i in seq_along(bt)) if (bt[i]) y[i] <- x[i + from - 1L]
    y }


###

n.nas <- function (v) v  % %  b.nas  % %  sum

postfix <- function(h, p) prefix(p, h)

enclose.h <- function (h, pre=HQUOTES, post=pre) postfix(prefix(h, pre), post)
bracket.h <- enclose.h %^% list(pre="[", post="]")

sift <- function (x, b.FUN=as.logical, ...) x[b.FUN(x, ...)]

clip_sift <- function(x, min_=0, max_=1, include.min=T, include.max=T)
        sift(x, function(x.)
                (if (include.min) `<=` else `<`)(min_, x) &
                (if (include.max) `<=` else `<`)(x, max_))

is_ascii <- function (h) vapply(
    h,
    function(h)
            if (h  % %  nchar) h  % %  utf8ToInt < 128L else F,
    T)

`%//%` <- function(h, ...) paste0(h, ..., collapse="")

l.characters <- function (h) strsplit(h, "")

characters <- function (h) h  % %  l.characters  % %  unlist

character.sift.h <- function(h, b.FUN=is_ascii) vapply(
	lapply(h  % %  l.characters, sift %^% list(b.FUN=b.FUN)),
			`%//%`, "")

character.count <- function(h, b.FUN=is_ascii)
    character.sift.h(h, b.FUN) % % nchar

nlines <- character.count %^% list( #TAGS character lines
    b.FUN=function(h) h  % %  characters == HEOL)

    Doc$nlines <- '
        nlines returns an integer vector indicating the number
        of newline characters found in the corresponding
        elements of the character vector argument.'

digits <- function () 0:9  % %  as.character

is_digit <- function(h) h %in% digits()

dec.sift.h <- character.sift.h %^% list(b.FUN=is_digit)

SPACE <- " "

substr2 <- function(x, extents) substr(x, extents[1], extents[2])

right <- function(character_, start=1)
        substr(
            character_,
            character_ %|% nchar - start + 1L,
            character_ %|% nchar)

    Doc$right <- '
        right returns substrings in a manner analogous to left,
        but returns the right portion of the strings rather than
        the left portion; see documentation for left.'

left.right <- function(character_, stop.left=1) {
    unleft %<=% character_ %-|% stop.left -> rights
      left %<=% character_ %-|% stop.left -> lefts
    rbind_ %<=% lefts %-|% rights }

    Doc$left.right <- '
        left.right returns a two-row character matrix. The first
        row contains the left-s of the character vector argument
        (arg 1) and the second row contains the corresponding
        right-s. The number of individual characters requested
        for the left-s is specified by stop.left (arg 2).

        EXAMPLE (underscores added to work around shortcoming in
        doc funciton):

        left.right(c("AS", "YOU", "WISH"), 2)

        _____[,1]_[,2]_[,3]

        [1,]_"AS"_"YO"_"WI"

        [2,]_""___"U"__"SH"
        ' 

recycle <- function(x, FUN) {
    result = x
    while (!FUN(result)) result <- c(result, x);   result }

    Doc$recycle <- '
        recycle returns the contatenatinon of the vector
        argument (arg 1) with itself. Internally, concatenation
        continues while FUN(<intermediate result>) returns
        FALSE; where FUN is arg 2. Example: recycle(1:3,
        function(x) 5 < length(x)) returns the equivalent of
        c(1:3, 1:3)'

recycle_len <- function (x, length.out)
        recycle(x, function(y) y %|% `#` >= length.out)[1:length.out]
        
chop <- function(character_string, lengths=1) { #TAGS substr
    if (
        lengths %|% length %|% `!` ||
        any(lengths < 0)           ||
        lengths %|% `!` %|% all    ||
        !nchar(character_string)) return (character_string)
    lengths <- recycle( # EXTEND LENGTHS SO THAT ALL CHARACTERS ARE PROCESSED.
        lengths,
        function(x) character_string %|% nchar   <   x %|% sum)
    1L %,% lengths %|% accum -> limits
    result <- vapply(
        seq_along(limits[-1]),
        function(i) substr(character_string, limits[i], limits[1+i] - 1),
        "")
    result[0:(result %|% nchar %|% `!!` %|% which1rev)] }

    Doc$chop <- '
        chop returns a character vector whose elements are
        contiguous substrings of the character_string argument.

        The lengths of the elements of the return are specified
        by lengths (arg 2). The lengths are recycled such that
        the total length is at least nchar(character_string).

        Trailing null strings will be absent from the return,
        unless the result is a single null string.

        EXAMPLE

        > chop("AS YOU WISH")

        [1] "A" "S" " " "Y" "O" "U" " " "W" "I" "S" "H"

        > chop("AS YOU WISH", c(2, 1, 3, 1, 4, 99)) # NO TRAILING ""

        [1] "AS"   " "    "YOU"  " "    "WISH"

        Originally developed for parsing fixed-formatted records
        from text, such as might be found in the input and
        output files of some Fortran programs.'

trimx <- function(x, left=1L, right=1L)
        substr(x, 1L + left, nchar(x) - right)

.strrev <- function (h) h  % %  characters  % %  rev  % %  `%//%`

strrev <- function (h) vapply(h, .strrev, "", USE.NAMES=F)

whichletter <- function (h) {
    w <- which(left(h[1]) == letters)
    if (w  % %  `#`) w else integer(0) }

words <- function (h) strsplit(unformat(h), " ") %|% first
words.h <- words #DEPRECATED use words
hh <- words #TAGS alias
#OLDNAME words once referred to what is now (2018-10-26) rwords

unwords <- function (h) paste(h, collapse= " ")
unwords.h <- unwords #DEPRECATED use unwords

crunch.h <- words %O% unwords.h
chh <- crunch.h #TAGS alias

abut.h <- function (h) paste(h, collapse= "")

# CONVERSION OF SPACE-DELIMITED VECTOR DATA

b.h <- words %O% as.logical
i.h <- words %O% as.integer
d.h <- words %O% as.double

r.h <- i.h %O% as.raw



# indexing

n.index.sign <- function (n) {
    if (n < 0) return (1L)
    if (n) 3L else 2L }

    Doc$n.index.sign <-
            "Let s be the sign of the numeric argument. Returns
            1L if s is negative, 2L if s is zero, and 3L if s is
            positive.  Can be used it index into a tripple."

n.index.h <- function (X, character_indices)
        match(character_indices, X %|% names)

n.index <- function (X, indices) {
	i  <- rep_along(NA_integer_, indices)
	bh <- vapply(indices, is.character, T)
	i[!bh] <- indices[!bh] %|% unlist %|% as.integer
	i[ bh] <- n.index.h(X, indices[bh]) %|% as.integer
	i }

index.b <- function (b) 1L + !!b

index123.b <- function (b)
    vapply(index.b(b),
            function(x) if (x  % %  is.na) 3L else x,
            FUN.VALUE=integer(1))

`%[b%` <- function (x, b) x[index.b(b)]
`%[[b%` <- function (x, b) x[[index.b(b)]]

# sequences

seq0 <- seq %<=% 0

seqN <- function (n)
        seq(length.out=n %|% abs %|% floor)

    Doc$seqN <- '
        seqN returns a sequence of the first few natural
        numbers. The argument specifies the length of the
        output. A negative argument has the same effect as its
        opposite (e.g.  argument 2 has same effect as -2). A
        double argument is rounded toward zero.'

make.sequence <- function(FUN, seed=0L, length.out=2L, ref=integer()) {
    for (i in seq(seed  % %  `#`  % %  succ, to=length.out)) seed[i] <- FUN(i, seed, ref)
    seed }

iseqs <- function (s, offsets=0L) {
    s <- s[1:(s  % %  `#` - max(offsets))]
    matrix(vapply(offsets, function (ofs) ofs + s, s + offsets[1]), ncol=offsets  % %  `#`) }

index.factors.dim <- function (dims)
        if (dims %|% `#` < 2) {
            1L
        } else {
            make.sequence(
                function(i, seed, ref) seed[i %|% pred] * ref[i %|% pred],
                seed=1L,
                length.out=length(dims),
                ref=dims) }

n.index.i.factors <- index.factors.dim # function DEPRECATED: use index.factors.dim

index.factors.m <- function(m) m  % %  dim  % %  index.factors.dim

n.index.i <- function (factors, i) succ(sum(pred(i) * factors)) %|% as.integer

i.index.n <- function(factors, n) {
    n   <- n - 1L # internally, index from zero
    lgf <- length(factors)
    i   <- integer(lgf)
    for (m in rev(1:lgf)) {
        i[m] <- n      %/% factors[m]
        n    <- n - i[m] * factors[m] }
    # adjust back to indexing from one
    1L + i }

i2.index.n <- function(m, n) {
    nr <- nrow(m);   n1 <- n - 1L;   1L + c(n1 %% nr, n1 %/% nr) }

is.nonnull <- function (x) !is.null(x)

dim_as <- function (x, archetype)
{   dim(x) <- dim(archetype);   x }

### Functions Dependent on R and shapir.R only

finites <- function (x) x[x %|% is.finite]

which.finite   <- is.finite %O% which
which.infinite <- is.infinite %O% which
is.Inf <- `==` %<=% Inf
is..Inf <- `==` %<=% -Inf #TAGS negative
which.Inf <- is.Inf %O% which
which..Inf <- is..Inf %O% which
which.NaN <- is.nan %O% which

finite.count <- function(v) v  % %  finites  % %  `#`

jump.n <- function (n, FUNs, ...) FUNs[[n]](...)

    Doc$jump.n <- '
        jump.n calls the n-th function in FUNs with ...'

as_logical_strict <- function (x, na=F)
{   y <- as.logical(x)
    y[y  % %  is.na] <- na
    y }

constant <- function(x) function(...) x

rmna <- function(v) v[!is.na(v)]

### Functions dependent on other functions in this file

jump.b <- function (b, l, ...)
        jump.n(b %|% index.b, l, ...)

    Doc$jump.b <- '
        jump.b behaves as jump.n, except that the index is a
        single logical value.'

jump.fn <- function (FUN, l, x, ...)
{   warning("jump.fn untested")
    jump.n(FUN(x), l, ...) }

divisions <- function(n) #TAGS divide bins unit interval
        n %|% seqN / n

    Doc$divisions <- '
        Returns a double vector that evenly divides the unit
        interval into n (arg 1) equal parts, or a degenerate
        double vector in the case where n < 1L. The return
        contains the UPPER bound of each division.
        
        Originally developed to aid in the classification of
        scaled values.

        > divisions(0)

        numeric(0)

        > divisions(3)

        [1] 0.3333333 0.6666667 1.0000000'

divisions_symmetric <- function (n)
        (n %|% divisions %|% `-` %|% rev %,% 0 %,% divisions(n))[-1]

#divisions_symmetric.odd <- function (n)
        #(n %|% divisions %|% `-` %|% rev %,% 0 %,% divisions(n))[-1]

#quiet <- function (fn, FUN=identity) function (x, ...)
        #if (!`#`(x)) return(FUN(x)) else fn(x, ...)

element.l <- function(l, i=integer()) {
	if (i  % %  `#` == 0L) return ( NULL )
	if (i  % %  `#` == 1L) return (l[[i]])
	element.l(l[[i  % %  first]], i[-1]) }

spaces <- function (length=1, nchar=1) rep(`%//%`(rep(" ", nchar)), length)

widespace <- spaces %<=% 1

which1 <- match %<=% T

    Doc$which1 <- '
        which1 returns a 1-vector, the index of the first TRUE
        value in the logical vector argument. If there is no
        such value, the return is NA_integer_. '

which1rev <- function(b) 1L + b  % %  `#` - match(T, rev(b))

firstAvailable <- function(v)
        v[v  % %  is.na  % %  `!`  % %  which1]

'
classify #SEE ALSO histogram
'

.n.classify.discrete <- function (v, x) which(x==v)
n.classify.discrete <- function (v, x, null=NA)
{   cl <- .n.classify.discrete(v, x)
    if (!`#`(cl)) return (null)
    cl }

iterate.simple <- function(post, FUN, fnbreak)
{   while (!fnbreak(post)) {
        pre  <- post;   post <- FUN(pre) }
    post }


is.Pairlist <- function (x) ! is.null(x) && is.pairlist(x)

is.Leaf <- function (x) ! is.list(x) # reminder package::stats defines is.leaf

b.leaves <- function(l) unlist(lapply(l, is.Leaf))

defoliate <- function(l, unleaf=".") {
	ll <- l
	for (i in seq_along(l)) {
		if (is.list(l[[i]])) ll[[i]] <- defoliate(l[[i]], unleaf=unleaf) else
		ll[[i]] <- unleaf } 
	ll }

i.leaves.t <- function(l, depth=NULL) {
	ll <- l
    n <- 0L
    names(ll) <- NULL # FUTURE: SHOULD KEEPING NAMES BE AN OPTION?
	for (i in seq_along(l)) {
		if (l[[i]]  % %  is.Leaf) {
            c(depth, i) -> ll[[i]]
            succ(n) -> n } else
        {   lv <- i.leaves.t(ll[[i]], depth=c(depth, i))
            lv[[2]] -> ll[[i]]
            lv[[1]] + n -> n
        } }
	list(n, ll) }

l.sansNULLs.l <- function (l)
{   ll <- NULLs(l  % %  `#`  % %  succ) # succ: want one more element for which1
    i <- 1L
    for (item in l) if (!(is.null(item))) { item -> ll[[i]];   succ(i) -> i }
    ret <- ll[1:pred(lapply(ll, is.null)  % %  unlist  % %  which1)]
    if (ret[[1]]  % %  is.null) ret <- list()
    ret }

is.list.compound <- function (x) is.list(x) && any(unlist(lapply(x, is.list)))

is.list.simple <- function (x) is.list(x) && ! is.list.compound(x)

nicenames <- function (x)
{   nom <- names(x)
    if (nom  % %  is.null) return(rep("", x  % %  length))
    nom }

h.labels.h <- function (h, prefix="", suffix=":", unlabel="")
{   if (!length(h)) return (character())
    unlabel -> h[!nchar(h)]
    prefix(prefix(suffix, h), prefix) }


# COLORS

colors.like <- function (h) colors()[grep(h, colors())]

colors.random <- function (
        n=1, patterns.exclude=words.h("black gray grey white")) {
    acolors <- colors()[lapply(patterns.exclude, function(h) grep(h, colors()))  % %
            unlist  % %  unique  % %  `-`]
    acolors[ceiling(`#`(acolors) *  runif(n))] }

TRANSPARENT <- "#00000000" #value was: transparent
BLK <- "#000000" #value black
WHT <- "#FFFFFF" #value white

.hclh64 = i.h("2 1 0 13 11 8 5 3") * 360/16
.hcll64 = seq(from=25, to=75, length.out=length(.hclh64))
.hclc64 = 203/256 * .hcll64

Colors64 <- hcl(.hclh64, c=.hclc64, l=.hcll64, fixup=F)
colors64 <- c(TRANSPARENT, Colors64)
        # HISTORY 2018-04-23: changed first color from NA

Color64 <- function(h)
{   hh <- "BRN ORG RED PUR BLU CYN GRN YEL"  % %  words.h
    Colors64[which(h == hh)] }

BRN <- Colors64[1]
ORG <- Colors64[2]
RED <- Colors64[3]
PUR <- Colors64[4]
BLU <- Colors64[5]
CYN <- Colors64[6]
GRN <- Colors64[7]
YEL <- Colors64[8]

# list

.sublist.h <- function(l, h) l[[h]]
sublist.h <- function(l, h) {
    if (length(h) == 1) return (.sublist.h(l, h))
    sublist.h(l[[h[1]]], h[-1]) }

# ARITHMETIC

`%mod1%` <- function (m, n)
        m %|% pred %% n %|% succ

    Doc$`%mod1%` <- '
        %mod1% is similar to %%, except that the base is one
        (1L) rather than zero (0L). **** DESIGN: **** arg 1 is a
        vector of integer values and arg 2 is a single integer
        value.

        Example: A sequence of hours of the clock might be
        generated by "-23:24 %mod1% 12":

        > matrix(-23:24 %mod1% 12, nrow=12)

                 [,1] [,2] [,3] [,4]

            [1,]    1    1    1    1

            [2,]    2    2    2    2

            [3,]    3    3    3    3

            [4,]    4    4    4    4

            [5,]    5    5    5    5

            [6,]    6    6    6    6

            [7,]    7    7    7    7

            [8,]    8    8    8    8

            [9,]    9    9    9    9

            [10,]   10   10   10   10

            [11,]   11   11   11   11

            [12,]   12   12   12   12'

# vector or list

indices_modulo <- function (x, n) n %mod1% length(x)
extract_modulo <- function (v, n) v[indices_modulo(v, n)]
`%[mod%` <- extract_modulo # function NEEDS WORK. Idea, e.g.,    v <- seq(9);   v %[mod% 10:12 <- 9;   # v == c(9,9,9,4,5,6,7,8,9)

`%[[mod%` <- `%[mod%` %O% first #TAGS list extract

    Doc$`%[[mod%` <- '
        `%[[mod%` returns a single elment from the vector or
        list argument.'

reprep <- function(x, times.each=1L)
        lapply(
            x %|% seq_along,
            function(i) rep(x[[i]], times.each %[mod% i)) %|% unlist

    Doc$reprep <- '
        reprep returns a vector consisting of the elements of
        arg 1 repeated the number of times specified by the
        corresponding element of arg 2. Elements of arg 2 are
        recycled if the length of arg 2 is less than that of arg 1.'

Nos.  <- seq_along
Nos.r <- Nos. %O% rev

l.last  <- function (x) rev(x)[1] # cf. tail function

# vector

degenerate <- function(v) vector(typeof(v))



count.v <- function(v) v  % %  as.logical  % %  which  % %  length

mask <- function(b, v, maskvalue=NA) { v[b  % %  which] <- maskvalue;   v }

# numeric vector

i.sign <- function (v) v  % %  sign  % %  as.integer

.max_abs <- function (v) v  % %  range  % %  abs  % %  max

purge.FUN <- function (v, FUN) v[v != FUN(v)]


# integer

ones <- function (n) rep(1L, n)
dieroll <- function (m=1L, n=6L) 1L + as.integer(runif(n=m, max=n))
cointoss <- function (n=1L) runif(n, max=2)  % %  as.integer  % %  as.logical


# list


lists <- function (n) rep(list(list()), n)

        #Doc$lists <- '
            #lists returns a new, empty, compound list: a list of
            #n empty lists.'

is.NULLs <- function (x)
        x  % %  is.list &&
        lapply(x, is.null)  % %  unlist  % %  all

NULL1 <- list(NULL)

is.NULL1 <- function (x)
        x  % %  is.singleton && x  % %  is.NULLs


ancestry <- function(l) {
    ll <- l  % %  length  % %  NULLs
    nom <- nicenames(l)
    names(ll) <- nom
    for (i in seq_along(l))
            if (!is.list(l[[i]])) ll[[i]] <- nom[i] else {
            ll[[i]] <- ancestry(l[[i]]) }
    ll }


# integer or double

`%**%` <- function (x, y)
{   x ** y -> xy
    xy  % %  floor  % %  as.integer  ->  ixy
    if (!(ixy  % %  is.na  % %  all) && any(xy != ixy)) warning("Loss of precision.")
    ixy }

whichletters <- function (h)
    characters(h) %in% letters  % %  which

l.whichletters.h <- function (h) lapply(h, whichletters)

H.i <- function (i) # TO DO: vectorize further with individual.characters() %in% letters
    vapply(
        seq_along(i),
        function (ii) if (i[[ii]] %in% 1:(LETTERS  % %  `#`))
                LETTERS[[i[[ii]]]] else as.character(NA),
        FUN.VALUE=character(1))

# vector

n.look.d <- function (d, target)
        if (target < d %|% first) {
            1L
        } else {
            (d <= target) %|% which %|% last }

    Doc$n.look.d <- '
        The numeric vector arg1 is **** ASSUMED **** to be
        sorted.  Returns an index of arg1.  Beyond the index,
        elements of arg1 are strictly greater than arg2, a
        singleton.'

look  <- `==` %O% which

look1 <- function (v, w) (v == w) %|% which1

locate <- function (v, w) #TAGS search
        vapply(v, function(x, y) look(x, y)[1], integer(1), w)

.vector_classes <- function () "character complex double integer logical raw"  % %  words.h
.vector_class_abbr <- function () "hcdilr"  % %  characters
vector_class <- function (h="") if (h=="abbr")
(.vector_class_abbr()) else .vector_classes()

what.h <- function (h) # lapply()
    vector_class()[match(h  % %  characters, vector_class("abbr")) % %  rmna]

colClasses.h <- what.h # function

# character

is_digitstr <- function(h)
        vapply(characters(h), is_digit, T)  % %  all

l.words.h <- function (h) lapply(h, words.h)

`%/ /%` <- function(g, h) g %//% " " %//%  h
`% //%` <- `%/ /%` #DEPRECATED Use `%/ /%`

pad.h <- function(h, n=max(nchar(h)), hpad=HSPACE)
        prefix(h, rep(hpad, n) % %  `%//%`)  % %
                (right %^% list(start=max(h % % nchar, n)))

# printing

cat.eol <- function() cat("\n")  # TO DO: use Curry, etc. to allow for named parameter passing, etc.
 
.h.TF.b <- function (b) if (b) return ("T") else "F"
h.TF.b <- function(b) lapply(b, .h.TF.b)  % %  unlist

print_x <- function(FUN, x)
{   lapply(x, function (X) X  % %  FUN  % %  pad.h  % %  cat)  % %  invisible
    cat.eol() }

'

funciton sublist.h

        Returns the element of the list (arg 1) specified by the
        character vector (arg 2). Example:

        > sublist.h(list(I=list(A=list(
          "1."="Hello", "2."="World!"))), c("I", "A", "2."))
        [1] "World!"

function rest 
function unrest

        rest (unrest) returns a vector of all but the first
        (last) element of the argument.

        Works for list-s, too.

function look : search find lookup

        Returns the indices of the vector (arg 1 or arg 2) for
        which the single value (arg 2 or arg 1) compares equal
        with the corresponding vector element.

        **** WARNING **** Expect strange results if neither of
        the arguments is a single value.
        
        See also: look1

        EXAMPLES

                > look(seq(5), -1)
                integer(0)

                > look(1, 1)
                [1] 1

                > look(1, 0)
                integer(0)

                > look(40+seq(9), 42)
                [1] 2

                > look(c(2,3,3,4,4,5), 4)
                [1] 4 5

        HISTORY
        
        2019-01-04  Revised documentation.
        2018-03-19  formerly returned only the index of
                    only the last item found.

function look1 : search find lookup

        Similar to look, but returns only the first index. The
        return will be NA_integer_ if the item is not found.

value .hclh64

        approximate hues of C64 colors

value .hcll64

        approximate range of C64 luminances

value .hclc64

        hcl-c a linear funciton of l; a numerator greater than 203 yields out-of-gamut color when applied in .hcl64
'

. = function(x){}


qes <- function (n) rev(seqN(n))

accum = function (x) {
    "Returns the accumulated sum of vector x. Example:
    accum(seq(10)) returns the first ten triangular numbers.
    
    TO DO: this is a specialization of reduce. refactor as such.
    "
    l = length(x)
    if (l < 2) return (x)
    y = x
    for (i in Nos.(x)[2:l]) { y[i] = y[i] + y[i-1] }
    y
}

.primate   = function (tf) { .("~first among equals~");   for (i in Nos.(tf)) if (tf[i]) break;   i }
primate    = function (tf) { .("returns index of first T value in a logical
                               vector");   .primate(c(tf,T)) %% (1+length(tf)) }

.primate.r = function (tf) { 1 + length(tf) - primate(rev(tf)) }
primate.r  = function (tf) { .primate.r(c(tf)) %% (1+length(tf)) }

# #

band = function(x, lo=0, hi=0, lin=T, hin=T) {
    tf = x[x >= lo];   tf = tf & x[x <= hi]
    if (!lin) tf = tf & x[x > lo] # conditionally exclude lo values
    if (!hin) tf = tf & x[x < hi] # conditionally exclude hi values
    tf
}

#### printing, output, debugging, testing

.hline <- function (pattern, n=getOption("width") - 1L)
        rep(pattern, length.out=n)  % %  `%//%`

dashes  <- function (n=getOption("width") - 1L) .hline("-", n)
dashes2 <- function (n=getOption("width") - 1L) .hline("=", n)
hrule   <- function (n=getOption("width") - 1L) .hline('_', n) %//% HEOL

cat.h <- function(h) h %//% heol  % %  cat

caterr = function (..., file=stderr(), sep=" ", fill=F, labels=NULL, append=F)
        cat(..., file=file, sep=sep, fill=fill, labels=labels, append=append)
cater  = function (...) cat(..., file=stderr(), sep=heol) # cat to stderr, one line per item
caterrhline <- function() caterr("----------\n")
pprint = function (x) { caterr("--------" %//% x %//% "\n");   print(eval(x)) }




 'COMMENTS AND DOCUMENTATION

function make.sequence

        Generates an integer sequence.

        Arguments
        
                1:      A funciton FUN

                seed:   a seed (initial values of the sequence
                        to be generated)

                length.out:

                        The desired length of the sequence
                        output

                ref:    reference data that may be used by FUN

        FUN is a function of three arguments:

                1:      The index of the current value of the
                        sequence, which will be returned by FUN

                2:      The previous sequence, before the current
                        value is determined

                3:      Reference data


function n.index.i.factors

        Given the dimensions of an array, returns an integor
        vector of factors which may be used for linear (one-
        dimensional) indexing or for the production of a
        scalar classificaiton given a vector classificatoin.
        Example:

                > dim(a)
                [1] 2 2 3

                > n.index.i.factors(dim(a))
                [1] 1 2 6

        see also the n.index.i function

function n.index.i : array multi factors

        Intended for array indexing. Given an integer vector of
        factors and a corresponding multidimensional array
        index, returns the corresponding linear
        (one-dimensional) array index. 

        Can also be used to produce a single natural number
        classification from multiple natural number classifi-
        cations---the initial reason for the development
        of the present function.

        Examples:

                > a
                , , 1

                     [,1] [,2]
                [1,]    1    3
                [2,]    2    4

                , , 2

                     [,1] [,2]
                [1,]    5    7
                [2,]    6    8

                , , 3

                     [,1] [,2]
                [1,]    9   11
                [2,]   10   12

                > p <- Curry(n.index.i, n.index.i.factors(dim(a)))

                > p(c(1,1,1))
                [1] 1

                > p(c(1,1,3))
                [1] 9

                > p(c(1,2,3))
                [1] 11

function `%//%` : string concatenation

        Example: "a" %//% "b" == "ab"

        Can also be used as a function, i.e., for multiple
        arguments; example:

                `%//%`("a", "b", "c") == "abc"

        cf. Fortran //


function join : REMOVED. Use %//% or paste0 instead. : character format

        Originally:

                function(vec, C="") { paste(vec, collapse=C) }

`%//%` function : string character concatenate (now defined in shapir.R)

multisource <- function (h) invisible(lapply(h, source)) #REMOVED Use localsys$source

function dieroll  : random integer

        dierol(2, 12) corresponds to 2D12

index.b function : logical

        returns 1 if argument is FALSE, otherwise 2.  Can be
        used to select one of two options (e.g., function
        calls) depending upon the value of a Boolean expression.


function index.factors.dim
function n.index.i
function i.index.n

        In this family of functions, factors refers to an
        integer vector where each element contains the number of
        elements contained by the corresponding "level" of a
        hypothetical array. For example, a matrix of 3 columns
        and 2 rows would have factors ic(1, 2) and a 3 x 3 x 3
        "cube" array would have factors of ic(1, 3, 9).

function index.factors.dim

        Given the dimensions of an array, returns the
        corresponding factors.
        

function n.index.i : array indexing

        Given the dimensions of an array (arg 1) and an integer
        vector (arg 2) representing a set of multidimensional
        indices into that array, returns the equivalent
        one-dimensional index. Examples:

function i.index.n

        Given an integer vector of factors

.hclh64 .hcll64 .hclc64 .hcl64 function : (not actual functions) color C64

        .hclh64 # approximate hues of C64 colors
        .hcll64 # approximate range of C64-s l-s
        .hclc64 # hcl-c a linear funciton of l; a numerator greater than 203 yields out-of-gamut color when applied in .hcl64
        .hcl64 # approximate C64 colors

.hcll .hclc .hclh .hclv .hcla .hclaimage function : (not actual functions) color

        # ARE THESE USED ANYWHERE?
        .hcll = 17L * seq(5)
        .hclc = as.integer(c(13, 27, 40, 54, 35))
        .hclh = pred(seq(16)) * 360 / 16
        .hclv = function (n) hcl(.hclh, c=.hclc[n], l=.hcll[n], # fixup=F)
        .hcla = c(.hclv(1),.hclv(2),.hclv(3),.hclv(4),.hclv(5))
        .hclaimage image(m(seq(16*5),ncol=5), col=(c(.hcl(1),.hcl(2),.hcl(3),.hcl(4),.hcl(5))))

`colors64-t` function : (not actual function) colors c64

        REMOVED. use Colors64

Colors64 colors64 : (not actual funciton) c64

        Colors64 are approximatoins of the proper 8 colors.

        colors64: same as Colors64, but with NA in posiiton 1,
        representing transparency.

colors.im function : image test

        Produces a test image given a vector of colors.

v.rmna.v function,
sans.na function : NA remove filter

        REMOVED. Use rmna

count.v function : number nonzero

        Returns number of nonzero values. C.f. Fortran COUNT.

ch.dec function : filter decimal character

        REMOVED. Use dec.sift.h instead.

dec.sift.h function : filter decimal character

        Argument is a singleton character vector.
        Returns a modified version of the input where all
        non-decimal characters have been removed.

rowNos.m function

        returns a vector of row-numbers of a matrix

colNos.m function

        returns a vector of column-numbers of a matrix

seqZp function

        Removed. use seqN.

colply function
rowply function 

        similar to lapply, but for rows and columns of matrices

function h.words.h : DELETED. Use words.h instead.

function words.h : character format split

        Returns a character vector comprised the words of the
        character string argument. Example:

                > words.h("Britain, an island in the Atlantic")
                [1] "Britain," "an" "island" "in" "the" "Atlantic,"

function words.h : character format join

        Returns a character vector comprised the words of the
        character string argument. Example:

                > words.h("Britain, an island in the Atlantic")
                [1] "Britain," "an" "island" "in" "the" "Atlantic,"

function unwords.h : character format join

        Returns a character string whose individual characters
        are taken from the character vector argument. Words are
        separated by space:

                > unwords.h(c("Britain,", "an", "island", "in"))
                [1] "Britain, an island in"

function max_abs

        Returns the magnitude of the numeric quantity having the
        largest magnitude among the elements of the vector
        argument.

        Could be used in scaling.

function constant

        Returns a function that returns the argument of
        constant. Example:

        > e <- constant(exp(1))
        > e()
        [1] 2.718282


function individual.characters RENAMED to characters
function characters

        Returns a character vector of individual characters
        found in the vector or list argument. Unexpected results
        may be returned if the argument is a tree.

function vector_class
function what.h
function colClasses.h

        vector_class

                When called with no arguments, returns the a
                character vector equivalent to c("character",
                "complex", "double", "integer", "logical",
                "raw"), corresponding to the atomic vector
                classes.

                When called with the argument "abbr", returns
                a character vector equivalent to c("h", "c",
                "d", "i", "l", "r"), corresponding to
                abbreviaitons for the atomic vector classes used
                herein.

        what.h

                Given an abbreviation composed of characters
                returned from the call vector_class("abbr"),
                returns a character vector suitable for the what
                argument of the scan function or the colClasses
                argument of the read.table function. Example:

                        > what.h("hi")
                        [1] "character" "integer"

function finites : subset select finite values

        Returns a vector containing only the finite values found
        in the argument. Example:

        > finites(c(-Inf, NA, Inf, 0, 1))
        [1] 0 1

function l.sansNULLs.l : pack remove NULLs clean

        Returns a modified copy of the list argument where the
        NULL values of the argument have been left out.

function b.leaves : list tree

        Returns a logical vector of the same lenght as the list
        argument. Each elmeent of the return indicates whether
        the corresponding element of the argument is a leaf.

function i.leaves.t : index tree count

        Returns a compound list where:
            The first element is a single integer giving the
        number of leaves in the list (or tree) argument.
            And the second is a kind of summary of the agument:
        each leaf is the (possibly compound) index of the
        corresponding leaf of the list artgument, in the form of
        a vector of natural nubers (integer vector). Example:

                > i.leaves.t(list(1, seq(25), list(letters)))
                [[1]]
                [1] 3

                [[2]]
                [[2]][[1]]
                [1] 1

                [[2]][[2]]
                [1] 2

                [[2]][[3]]
                [[2]][[3]][[1]]
                [1] 3 1

function element.l : indexing vector tree list item

        Returns the elment of the list (arg 1; may be a tree),
        indicated by the optional argument i, an integer vector
        of natural numbers. Example:

                > element.l(list(
                    1,
                    list(
                        21,
                        list(
                            221),
                        23),
                    3), # end of first argment
                    c(2, 2, 1)) # second argument
                [1] 221

function sift : filter

        Returns the elements of arg 1 for which the optional
        second argument (a logical function) return T.

function dim_as : redimension shape

        Returns the data of arg 1 in an array of the same shape
        as arg 2. Can be used to reshape values returned as
        vectors.

function colors.im : REDEFINED

        REMOVED. not needed after reworking of .image.im

        Old Code:

                colors.im = function (col=colors64)
                {   n <- length(col);   c( as.character( rbind( rep(NA, n), col ) ), NA) }

function identical.picky : compare equal

        See also identical function (base).

        Returns a logical value indicating whether the two
        arguments are strictly identical.

        Examples:

                > identical.picky(NA, NA)
                [1] TRUE

                > lapply(list(1:2, 3:4, 5:6), identical.picky,
                +       3:4) % % unlist
                [1] FALSE  TRUE FALSE

function fndefault

        Returns a function of one variable that returns the
        single argument of fndefault if the argument of the
        function is NULL and the argument otherwise. Example:

                > default <- fndefault(0)
                > default(NULL)
                [1] 0
                > l <- list(); l[[2]] <- 2; default(l[[1]])
                [1] 0
        
function glss : grep ls ShapirEnv find

        Applies grep to the listing of objects in ShapirEnv.
        Returns the corresponding character vector, which is a
        possibly empty subset the return from ls(ShapirEnv)

function anonymize.v  : remove names
function anonymize.m  : remove names
function anonymize.df : remove names

        Returns a modified copy of its argument. The copy does
        not have names. Otherwise should be the same.

function index123.b : index logical

        Given a logical vector, returns an integer vector of the
        same length. The pair of inputs and outputs are a
        one-to-one mapping: (F, T, NA) --> 1:3

        note: can be transformed to (NA, F, T) --> c(0, 1, 2)
        using %% 3.

function whichletter

        Returns zero if the first individual character of the
        first character string of the argument is not a lower
        case letter (in letters). Otherwise, returns the index
        of that letter. whichletter("a") == 1; whichletter("z")
        == 26.

function as.disjoint.segments : lines plot

        Intended for use with a sequence of scalars representing the x-
        or y- coords of a sequence of disjoint line segments.

        Returns a vector containing the same scalars as input, where an
        NA-value is placed after each pair.

function as.disjoint.segments.right : lines plot

        Intended for use in building data that can represent horizontal or
vertical lines.

        Similar to as.disjoint.segments, however, each value of the
argument represents a pair of values of the same value

function brand

        Returns a logical vector of n (arg 1) random values.

        Conceptually, FALSE maps to 0 and TRUE maps to 1,
        therefore FALSE < TRUE. Internally, brand applies runif
        to get values in [0, 1]. If the default compar argument
        is used, the values more toward the left of this
        interval are assigned F and the values more toard the
        right are assigned T. Quantification of "more left" or
        "more right" is accomplished via the threshold argument.

        The optional compar and threshold arguments determine
        the parameters of the distribution. Examples:

                sum(brand(100, t=.9))

        is expected to return a value much less than 50, as
        there is about a 90% chance that a given value returned
        by brand is FALSE.

                sum(brand(42))

        is expected to return a value equal to 21, on average.


function extract_along

        The behavior of this function should be identical to
        `[`, for linear indices (e.g., x[i]) provided that all
        members of i are positive integers and the optional na
        argument is the default value of NA.

        For each negative value in i, the return contains the
        scalar value specified by the optional na argument.

        Originally intended to select data representing a
        geographic extent, where a NA could represent "off sheet."

        Examples:

                > extract_along(1:3, -5:5)
                 [1] NA NA NA NA NA NA  1  2  3 NA NA

function squish : scale

        Returns a rescaled version of the primary argument,
        which is assumed to be a double vector.

        The function is designed such that meaningful returns
        are produced when:
        
                - elements of v are in [0, 1],
                - from < to
                - 0 <= from < 1
                - 0 < to <= 1

        Originally developed for experimenting with colors,
        e.g., to remap a sequence of values in [0, 1] into a
        narrower range that, in turn, maps to a color gamut. One
        may wish to map [0, 1] to [grey(.25), grey(.75)].

        Examples:

                > squish(0:5/5, 0, 1)
                [1] 0.0 0.2 0.4 0.6 0.8 1.0
                > squish(0:5/5, .5, 1)
                [1] 0.5 0.6 0.7 0.8 0.9 1.0

                > squish(0:5/5, .25, .75)
                [1] 0.25 0.35 0.45 0.55 0.65 0.75

function substr2

        A 2-parameter version of substr. Arg 1 matches
arg 1 of substr. Arg 2 is a vector containing the start and stop 
                
function trimx

        Similar to trimws or substr. Returns a modified copy of
        the primary character string argument, with the first
        left and last right characters removed. Example:
        trimx("`string`") returns "string" (i.e., withoug the
        backquotes).

function partition : search split parse

        Similar to the Python 3 str member function partition,
        but applies to any R-vector, including lists.

        The intended use case is when v (arg 1) is a vector
        and x (arg 2) is a singleton vector.

        Finds the first member of v that match-es x. (for this
        purpose, T == 1L == 1 == 1+0i == "1", etc.).

        Returns a three-element list. The first element of the
        return contains the initial portion of v (i.e., before
        the first match), the second element contains the match,
        and the last element contains the remainder of v (i.e.,
        after the first match).

        Examples

                The call partition(words.h(
                        "Britain, an island in the Atlantic"),
                        "island")

                returns the equivalent of

                list(
                    c("Britain,", "an"),
                    "island",
                    c("in", "the", "Atlantic"))

function firstAvailable

        Returns the first non-NA (i.e., "available" or "not not
        available") element from the vector argument. Returns
        NA if all elements are NA.

function parallelize

        Reorganizes any number of atomic vectors or arrays into
        a matrix where each column corresponds to one argument,
        and each row corresponds to corresponding elements of
        the arguments.  Arguments should all be of the same
        length.


function initials

        Returns a character vector of initial characters of the
        character vector argument. Example:

                > initials(words.h("rodents of unusual size"))
                [1] "r" "o" "u" "s"

function iseqs : sequences index indices

        The primary argument is intended to be a sequence of
        indices (typically 1:n )into another sequence.  Returns
        a matrix of parallel sequences (one per column), one for
        each of the offsets (arg 2), where the base sequence is
        given by the primary argument. Originally intended to
        produce indices which may be used in finite differencing
        and similar applications. Exmaples:

                > iseqs(1:5, ic(0, 2))
                     [,1] [,2]
                [1,]    1    3
                [2,]    2    4
                [3,]    3    5

function ic : c combine integer

        Combines the arguments with c, converts the result to
        integer. Exmaple:

                > ic(F, T)
                [1] 0 1
                > ic(0, 1)
                [1] 0 1
                > typeof(ic(0, 1))
                [1] "integer"
                
function block.extract

        Similar to `[`. However, the returned vector contains an
        element for each integer in the sequence from:to, even
        if one or more of those values are outside the range of
        indices of the vector argument x, in which case the
        value will be na.

                > block.extract(1, -1, -1)
                [1] NA
                > block.extract(1:9, -1, 10)
                [1] NA NA  1  2  3  4  5  6  7  8  9 NA

function is_ascii

        Returns a logical vector indicating whether each
        individual character of the UTF-8 character string
        argument is an ASCII character.


function centroids : finite difference

        Returns the argument, less 0.5.

        Given a matrix whose columns represent the indices of
        cells of a rectangular finite difference grid, returns
        the coordinates of the centroids, assuming unit
        dimensions for each cell. Similarly for a vector or
        other array.

        The return may be scaled to achieve coordinates in
        another system.

        HISTORY
        
                2018-10-12  formerly located in 0005-fin-diff.R
        
function strrev : string reverse character vector

        Returns a character vector where each of the elements
        has been reversed.

                strrev(words.h("NAC  UOY  EES   EM ?WON"))
                   NAC    UOY    EES     EM   ?WON 
                 "CAN"  "YOU"  "SEE"   "ME" "NOW?" 

function lists : compound list

        see Doc

function iunif : random integer

        Similar to runif, but returns an integer vector. If the
        optional argument min is kept at the default value of
        1L, and the optional argument max is set to the length
        of some vector, iunif can be used to produce random
        indices into that vector. For nonrepeating random
        indices, see shuffle.

function shuffle : random indices

        Returns a random integer vector of length specified by
        the single argument. Elements have values that do not repeat,
        and which range from one to the length specified.

function `%]%` : extract

        Performs `[`, but with indexing relative to the END of
        the vector argument.

        Example:

                > MONTHS %]% 1 # LAST MONTH OF THE YEAR
                [1] DECEMBER
                > MONTHS %]% -9:-1 # ALL BUT THE LAST NINE MONTHS
                [1] "JANUARY"  "FEBRUARY" "MARCH"
                

        FUTURE (maybe): argument may be an array of any
        dimension.

function pad.h

        Returns a modified copy of the primary character vector
        argument. Each element of the return is the same width;
        hpad character strings are used to pad out to this
        width. The width is the maximum of the optional argument
        n and the maximum number of characters of the primary
        argument. Examples:

                > pad.h(words("AS YOU WISH"))
                [1] "  AS" " YOU" "WISH"
                > pad.h(42 %,% "SIX TIMES SEVEN", hpad="0")
                [1] "000000000000042" "SIX TIMES SEVEN"

function n.index.h : character indexing

        Returns the integer indices of arg 1 that correspond
        with the character indices given by arg 2.

        EXAMPLES

                > D6 <- 1:6
                > n.index.h(D6, "A")
                [1] NA
                > n.index.h(D6, words("A B"))
                [1] NA NA
                > n.index.h(rename.all(D6, words("A B C D E F")), words("A B"))
                [1] 1 2
                > n.index.h(rename.all(D6, rep("A", 6)), words("A B"))
                [1]  1 NA

function index.h : mixed type indexing

        Designed for mixed type indices. See also n.index.h.

        Returns the integer indices of arg 1 that correspond
        with the indices given by arg 2, which is expected to be
        a list containing single values of logical, integer,
        double, or character type.

function interlace

        Returns a vector formed from the elements of the given
        list of vectors. The return groups elements of the
        arguments together.  Example:

                > interlace(1:3, 4:6, 7:9)
                [1] 1 4 7 2 5 8 3 6 9

function interlace1

        Same as interlace, but accepts a list of vectors rather
than an arbitrary number of vectors. Example:
                > interlace1(list(1:3, 4:6, 7:9))

function l.unlace : interlace
function l.unlace.l : interlace

        Returns a list of parallel lists composed of elements
        from the vector or list argument.

        For a list argument, unlace.l and l.unlace.l are
        inverses of l.interlace.l

        Example: l.unlace(1:4) and l.unlace(as.list(1:4)) both
        return the equivalent of list(list(1, 3), list(2, 4))

function dub : names rename

        Returns a modified copy of the vector or list argument.
        (arg 1). The names of the return are either the original
        names or as specified by arg 2, a data frame containing
        numeric indices in the first column and new names
        (vector convertible to character) in the second column.
        Example:

                dub(
                    list(1, 2, 3),
                    data.frame(2, "TWO", stringsAsFactors=F))

                returns the equivalent of

                list(1, TWO=2, 3)

function d.trunc : rounding

        Returns a double vector of rounded values. The argument
        is a numeric vector. Rounding is by truncation. If
        precision is a power of two, the rounded values retain
        precision log_2 precision binary digits of precision.

'

NULL
