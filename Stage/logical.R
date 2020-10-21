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

#TODO WHY DOESNT { isnotna <- is.na %O% un } WORK?
isnotna <- function(x) x %|% un(is.na)

not  <- `!`
and  <- `&`
nand <- and %O% not
or   <- `|`
nor  <- or %O% not
xnor <- xor %O% not

none <- any %O% `!`

    Doc$none <- '
        none is the opposite of `any.` It returns TRUE if there
        are no `TRUE`-values.'

`%notin%` <- `%in%` %|% un

    Doc$`%notin%` <- '
        `%notin%` is the opposite of `%in%`. It tells whether
        each item is not in the `table` (see documentation for
        `%in%`).'

positive.edge <- function (x) {
    x %|% as.logical %|% xorr
     }

' WORKONG ON "NOT ALL" AND "NOT ANY"
nall <- function(x) {
    if (any(is.na(x))) return (NA)
    ! all(x)

}

nany <- `!` %O% all
'

allequal <- function (x)
        if (length(x) < 2) T else all(x[1] == x[-1])

    Doc$allequal <- '
        allequal returns a single logical value that tells
        whether the elements of the vector argument are all
        equal.'

fn.compress.choice.of.three <- function(choices) { #TAGS factor

        if (!is.list(choices))
                stop("Arument must be a list or data.frame.")

        LENGTH <- length(choices)

        if (!LENGTH %in% 2:3)
                stop("The list argument must have two or three elments.")

        if (!all(vapply(choices, is.vector, T)))
                stop("All elements of the list argument must be vector or list or data.frame.")

        if (sum(abs(diff( # sum abs diff : ARE 3 VALUES UNEQUAL?
            vapply(choices, length, 1L)))))
                stop("The list argument must have elements of equal length.")

        names(choices) <- c("TRUE.", "FALSE.", "NA.")[1:LENGTH]

        inner <- function(opts, x)
            if (missing(x)) return (opts) else c(F, NA, T)[
                opts[[2]] %in% x * -1L +
                opts[[1]] %in% x * +1L +
                2L] # +2L MAPS -1:1 TO 1:3
        inner %<=% choices }

    Doc$fn.compress.choice.of.three <- '
    fn.compress.choice.of.three is intended for situations where
    there are one or more groups of options, where the
    within-group options are mutually exclusive, and there are
    two or three options in each group.

    The original application of 
    fn.compress.choice.of.three is in developing a shorthand for the states of mouse buttons: each button may be 1) released, 2) pressed or 3) we may not care about the state of a particular button.

    fn.compress.choice.of.three builds a function and returns it.

    The concepts behind this function are similar to those of factor.

    The single argument, a list or data frame, contains parallel vectors.
    Each group of corresponding elements represents a three-way choice.
    The corresponding elements of these parallel vectors are mapped to TRUE, FALSE, and NA.

opt <- fn.compress.choice.of.three(list(
    list("GROUP.1.CHOICE.1", 1L, "have it, too"),
    list("GROUP.1.CHOICE.2", 2L, "eat your cake") ) )
opt()

opt <- fn.compress.choice.of.three(list(
    c("yes", "true", "have it, too"),
    c("no", "false", "eat your cake") ) )
opt()

opt <- fn.compress.choice.of.three(
	list(
		list(data.frame(letters)),
		list(data.frame(LETTERS)) ) )
opt()

opt(c("yes", "no", "eat your cake", "true"))
opt()

opt <- fn.compress.choice.of.three(
    c("no", "false"),
    c("yes", "true", "have it, too"))
opt(c("yes", "no", "eat your cake", "true"))
opt()

'




    Doc$demote <- '
        demote returns a logical vector corresponding to the
        logical, numeric, or complex input vector. Zero-elements
        of the argument correspond to FALSE elements of the
        return.  Non-zero elements of the arguemnt correspond to
        TRUE elements of the return.'

na2T.b <- function (b)
        b %|% is.na | b

    Doc$na2T.b <- '
        na2T.b returns a modified copy of the logical vector
        argument. NA values of the argument map to TRUE values
        in the return.'

na2F.b <- function (b)
        b %|% is.na %|% `!` & b

    Doc$na2F.b <- '
        na2F.b returns a modified copy of the logical vector
        argument. NA values of the argument map to FALSE values
        in the return.'


