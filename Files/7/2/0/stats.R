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


binomialdist <- function(n, k=0:n, p=0.5)
        choose(n, k) * p ^ k * (1 - p) ^ (n - k)

    Doc$binomialdist <- '
        binomialdist returns values from the binomial
        distribution given n (arg 1), the number of trials, k
        (arg 2), the number of successes, and p, the probability
        of success for a single trial.'

irange <- function (x) list( #TAGS classify index
      FINITE=x %|% which.finite,
      .INF=x %|% which..Inf,
      INF=x %|% which.Inf,
      NAN=x %|% which.NaN,
      NA.=x %|% which.na)

    Doc$irange <- '
        irange returns a list of index vectors of the finite,
        -Inf, Inf, Nan, and NA values found in the atomic vector
        argument.'

nrange <- function (x)
        vapply(x %|% irange, `#`, 1L)

    Doc$nrange <- '
        nrange returns a list of singleton vectors giving the
        count of the finite, -Inf, Inf, Nan, and NA values found
        in the atomic vector argument.'

range. <- function (x) {
    BRANGE <- x %|% nrange
    NAMES  <- BRANGE %|% names
    BRANGE <- rename.all(!!BRANGE, NAMES)
    rename.all(
        c(  
            { if (BRANGE[["FINITE"]]) {
                x %|% finites %|% range %|% as.list
            } else {
                x[0] %|% list %|% rep2 } },
            BRANGE[-1] %|% as.list),
        "MIN MAX" %|% words %,% NAMES[-1]) }

    Doc$range. <- '
        range. returns a list whose first two elements are the
        minimum and maximum values found among the finite values
        of the vector argument. These first two elements will be
        empty vectors if the argument has no finite values. The
        remaining elements contain logical singletons indicating
        whether there were any Inf, -Inf, NaN, or NA values in
        the argument.'

range.finite <- function (x)
        range.(x)["MIN MAX" %|% words] %|% unlist %|% anon

    Doc$range.finite <- '
        range.finite returns the range of the finite values of
        the vector argument. The return is an empty vector if
        there were no finite values.'

.ranges.m <- function (m)
    lapply(
        m %|% colNos,
        `[` %<=% m %<=% rowNos(m) %O% range.finite)

.ranges.l <- function (list.)
        lapply(list., range.finite)

ranges <- function(X)
    enlist(
        if (X %|% is.vector) {
            if (X %|% is.list) {
                X %|% .ranges.l
            } else {
                X %|% range.finite
            }
        } else if (X %|% is.matrix) {
            X %|% .ranges.m
        } else if (X %|% is.data.frame) {
            X %|% .ranges.m
        } else {
            list() } )

    '# WHY DON-T EITHER #1 OR #2 WORK INSIDE ranges BUT WORK OUTSIDE?
    #1
    Y <- X %|% `#` %|% NULLs
    for (i in X %|% seq_along) Y[[i]] <- X[[i]] %|% range.finite
    Y
    #2
    lapply(X, range.finite)'

    Doc$ranges <- '
        ranges returns the range of finite values for each
        column of the argument. An atomic vector argument counts
        as a single column.
        
        HISTORY

        2019-05-21: Changed to always return a list of vectors.
         '

iranges <- function (X)
        lapply(X %|% ranges, as.integer) %|% cbind_l

    Doc$iranges <- '
        iranges is similar to ranges, but **** ASSUMES **** all
        range-values are integers and returns a 2-row matrix,
        with the same number of columns as the agument.'

.bounding.box <- function(X) {
    if (X %|% is.vector) return (range.finite %O% enlist %-|% X)
    if (X %|% is.matrix) return (X %|% ranges)
    if (X %|% is.data.frame) return (X %|% ranges)
}

bounding.box <- placeholder

    Doc$bounding.box <- '
        A bounding box is a two-row matrix. Each column contains
        the min (row 1) and max (row 2) values found among the
        coordinates of some points in n-dimensional space.
        
        In 2-D, the first column of the return represents the
        "bottom-left" corner, and the second column represents
        the "upper-right" corner.'

as_bb <- function(X) #TAGS bounding box
    matrix(
        lapply(
            .bounding.box(X),
            function(Y) if (Y %|% is.empty) -Inf %,% Inf else Y) %|%
        unlist,
        nrow=2)

    Doc$as_bb <- '
        xy.bounding.box returns a bounding box (see
        documentation for bounding.box) The argument is a vector
        (counts as one dimensional matrix), matrix, or
        data.frame where each row represents a point in
        n-dimensional space.'

.ix.interval.xi <- function (shift, i)
    if (i %|% is.matrix || i %|% is.data.frame) {
        i %m+v% shift
    } else if (i %|% is.vector) {
        i[1:2] %|% as.matrix %|% t %|% x.interval.i }

x.interval.i <- .ix.interval.xi %<=% .lo
i.interval.x <- .ix.interval.xi %<=%  lo %O% ceiling

    Doc$x.interval.i <- '
        x.interval.i returns the (co)ords (one-dimensional) of
        the "spatial" interval(s) corresponding to the "raster"
        interval argument (a vector of length 2, 2-columnn
        matrix or data.frame). Example: raster cells from 1 to 1 (a
        single raster cell) have a spatial interval from 0 to 1:
        x.interval.i(c(1, 1)) == 0:1.'

   Doc$i.interval.x <- '
        i.interval.x is the reverse transform of x.interval.i'

xybb.ijbb <- function (ijbb) {
    ijbb %|% t %|% x.interval.i %|% t
     }

    Doc$xybb.ijbb <- '
        xybb.ijbb returns the "spatial interval" bounding box
        that corresponds with the "raster interval" bounding box
        argument.'

'


ranges. <- range %^% list(na.rm=T)
'


mean_abs <- abs %O% mean

squares <- `^` %|% argswap %<=% 2

rsquared <- cor %O% squares

ssq <- squares %O% sum #TODO: USE arith.R INSTEAD

sum.of.squares <- ssq #DEPRECATED use ssq

reciprocal <- `^` %|% argswap %<=% -1

mean_square <- squares %O% mean

rms <- mean_square %O% sqrt

    Doc$rms <- "rms returns the root mean square."

root.mean.square <- rms

square.diffs <- `-` %O% squares
specialize("square.diffs", n=1) # CREATES `%square.diffs%`

    Doc$square.diffs <- '
        square.diffs returns the square of the difference
        between the two arguments. Originally intended for
        aligned vectors or matrices.'

    Doc$`%square.diffs%` <- Doc$square.diffs

sum.of.square.diffs <- square.diffs %O% sum

nse <- function(sim, obs)
    1 - sum.of.square.diffs(obs, sim) / sum.of.square.diffs(obs, obs % %  mean)

    Doc$nse <- "nse returns the Nash-Sutcliffe efficiency."


mae <- function (sim, obs) (obs - sim) %|% mean_abs

rmse <- function (sim, obs) (obs - sim) %|% rms

summary_stats.model <- function (sim, obs, FUNs)
        vapply(FUNs, function(FUN, sim., obs.) FUN(sim., obs.), 1, sim, obs)




