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
# character

strsplitby <- strsplit % % argswap #function

newline.affix.h <- postfix %|% argswap %<=% HEOL

    Doc$newline.affix.h <- '
        newline.affix.h returns a modified copy of the character
        vector. Each element of the return contains an ****
        ADDITIONAL *** newline character at the end.'

lines.h <- strsplit %|% argswap %<=% HEOL #TAGS split newline end-of-line character string

    Doc$lines.h <- '
        lines.h returns the content of the character vector
        argument, split by newline. Each character string in the
        character vector argument corresponds to one item of the
        return (a list of character vectors).  Each element of
        the return is a character vector containing the
        characters found between the newline characters of each
        element of the argument.

        > lines.h(c("A B\n1 2\n3 4", "a\nb\nc\n"))

        [[1]]

        [1] "A B" "1 2" "3 4"

        [[2]]

        [1] "a" "b" "c"
        '

lines.h1 <- function(h, ...) lines.h(h[1], ...)[[1]]

strsplitbycomma <- strsplitby %<=% "," #function


# colors

colors.d <- function(FUN, n, black=T, white=T) {
	n <- n + (!black) + (!white)
	vapply(0:(n-1)/(n-1), FUN, "")[(1 + !black):(n - !white)]
}

greys <- colors.d %<=% grey #function
greens <- colors.d %<=% (rgb %<=% 0 % % argswap %<=% 0) #function

zeroes <- integer

smash.h <- words.h %O% `%//%`

odds <- # function sequences
        curry(`+`, -1) %O% evens

purge.max <- max %=>% argswap(purge.FUN) # function
purge.min <- min %=>% argswap(purge.FUN) # functionn
purge.extremes <- purge.min %O% purge.max # function

l.mask.l <- function (b, l, maskvalue=NA)
        lapply(l, mask %^% list(b, maskvalue=maskvalue))

`?rmna` <- function(rmna, v) if (rmna) rmna(v) else v

max_abs <- function(v, na.rm=F)
        `?rmna`(na.rm, v)  % %
        (function (v) v  % %  range  % %  abs  % %  max)

scale_unit <- function (v) {
    scale <- max_abs(v  % %  finites)
    if (scale  % %  as_logical_strict) v / scale else v }

scale_unit_nonnegative <- function (v) { warning("placeholder") }

scaler <- function (v)
        if (v  % %  finite.count)
                scale_unit(v - v  % %  finites  % %  min) else v

min_finite <- function(x) {
    fin <- x  % %  finites
    if (fin  % %  `#`  % %  `!`) return (x  % %  typeof  % % vector)
    min(fin) }

minfin <- min_finite

.x.shift_euclid <- function (v) {
    x <- v  % %  minfin
    if (x  % %  `#`) x else 0 }

shift_euclid <- function (v, x=NULL) {
    if (is.null(x)) x <- .x.shift_euclid(v)
    v - x }
	
shift_euclid.l <- function (lcoords, shifts)
        lapply(seq_along(lcoords),
	            function (i) shift_euclid(lcoords[[i]],
                        rep(shifts, length.out=`#`(lcoords))[[i]]))
	
fit <- function (x) x %|% shift_euclid %|% scale_unit

    Doc$fit <- '
        fit Returns a transformation of the numeric vector
        argument elments by shifting and scaling such that the
        transformed finite values of the resultant are all in
        [0, 1].

        > fit(c(-Inf, NA, -100, -16, 100, Inf))

        [1] -Inf   NA 0.00 0.42 1.00  Inf'

fit.to <- function (x, from, to)
        fit(from %,% x %,% to)[-(x %|% `#` + 2) %,% -1]

.subst <- function (x, FUN, replacement)
        if (x  % %  FUN) replacement else x

l.subst <- function (FUN, replacement, X) # HISTORY 2018-06-01 name was subst
        lapply(X, .subst, FUN, replacement)

subst <- function (X, FUN=`!`, replacement=NA, ...)
        # HISTORY 2018-06-01: FUN result must produce vector, rearranged args
        # HISTORY 2018-07-19: corrected arg arrangment of inner
    # call, removed FUN.VALUE argument
        vapply(X, .subst, X[1], FUN, replacement, ...)

subst.a <- function (X, ...) dimension(subst(X, ...), dim(X))

        Doc$subst <- "
                
                Returns a modified copy of the atomic vector
                argument (arg 1): where FUN returns T for original
                values, replacement is found instead. The
                replacement must be of the same type as the
                atomic vector, and must be of length 1."

        Doc$subst.a <- crunch.h("
            subst.a is the same as subst, except that the array
            dimensions of the return are the same as the
            argument.")

fn.subst.fX <- function (FUN1, replace.na=T)
        function(X, ...) subst(X,
                FUN=(function(x)
                        if (x  % %  is.na) ! replace.na else (x == FUN1(X))), ...)

subst.max <- fn.subst.fX("na.rm" %=% T %=>% max) # function
subst.min <- fn.subst.fX("na.rm" %=% T %=>% min) # function
subst.extreme <- subst.min %O% subst.max




l.look <- function (v, targets) lapply(targets, argswap(look), v)

'
function l.look : search find lookup

        Returns a list containing the results of look for each
        element of arg 2. Examples:

                > l.look(seq(9), c(1, -5, 3))
                [[1]]
                [1] 1

                [[2]]
                integer(0)

                [[3]]
                [1] 3


funciton l.mask.l

        Applies the same mask (arg 1) to each of the vectors in
        the list (arg 2). The returned list is aligned with the
        input list.

function `?rmna` : na.rm NA remove

        Returns arg 2 if arg 1 is F. Otherwise returns a
        modified version of arg 2, where NA-s have been removed.
        Example:

                > `?rmna`(T, NA)
                logical(0)

function max_abs

        Returns the magnitude of the element in arg 1 having
        the greatest magnitude.  Optionally removes NA-s
        according to the logical arg 2.  Returns a value even
        when arg 1 is of zero length, or when arg 1 with NA-s
        removed (if that is specified) is of zero length.
        Originally intended to be used for scaling. See scale

function scale_unit

        Returns a scaled version of the logical, integer, or
        double vector argument. The absolute value of the max of
        the return is equal to one, except in the case where the
        max of the finite values of argument is zero in which
        case the return is equal to a the argument. Examples:

                > scale_unit(0)
                [1] 0
                > scale_unit(c(-2,-1,0,1))
                [1] -1.0 -0.5  0.0  0.5

function scale_unit_nonnegative

        Similar to scale_unit, except that the range of the
        return is forced to [0, 1], 

function minfin : minimum finite
function min_finite : minimum

        Returns the minimum finite value of the vector argument.
        If the argument has no finite elements, returns an empty
        vector of the same type as the argument.

function shift_euclid : euclidean geometric shift

        Returns the vector arg 1 - arg 2. In the case where arg 2
        is unspecified, the corresponding value is set to the
        minumum finite value of arg 1, so that the min of the
        return is zero. Example:

        > shift_euclid(c(-Inf, NA, seq(10), Inf) - 5)
        [1] -Inf   NA    0    1    2    3    4    5    6    7    8    9  Inf

        Reference: http://mathworld.wolfram.com/Shift.html

function crunch.h : character format
function smash.h : character format

        crunch.h returns the input character string, modified by
        removing all insignificant whitespace (space and
        end-of-line have been tested). smash.h removes all
        whitespace. Examples:

                > crunch.h("As   you
                + wish.")
                [1] "As you wish."
                > smash.h("As   you
                + wish.")
                [1] "Asyouwish."

function zeroes

        Returns an integer vector of zeroes, the length of which
        is determined by the argument.

function colors.d

        Originally inteded to be applied via partial funcitons
        wherein the primary argument is baked in.

        The primary argument (FUN) is a function that returns a
        color given a double value in [0, 1]. It is anticipated
        that FUN will return low-luminance colors for lower
        values and higher-luminance colors for higher values.

        The optional arguments black and white tell whether to
        include the least an most intense colors. This can be
        useful when displaying greys on a black or white
        background, for example.

        Returns a vector of n (arg 2) colors which map to evenly
        spaced values in [0, 1] and are ordered.

        For exaples, see greys and greens.

function greys : color

        Returns a sequence of n (arg 1) grey shades which
        gradually increase in luminous intensity. Black and/or
        white may be excluded by specifying black=F and/or
        white=F.

function greens : color

        Similar to greys, except that the return is a vector of
        shades of green.


function lines.h1 : split newline end-of-line character string

        Returns the content of the first element of the
        character vector argument, split by newline.  Each
        element of the return is a character vector containing
        the characters found between the newline characters of
        the first element of the argument. Example:
                > lines.h1(c("A B\n1 2\n3 4", "a\nb\nc\n"))
                [1] "A B" "1 2" "3 4"

function odds : sequences

        Returns the first n odd numbers, where n is arg 1.
'
