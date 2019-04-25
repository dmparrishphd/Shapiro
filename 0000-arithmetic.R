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

is.odd <- `%%` %|% argswap %<=% 2L %O% as.logical
is.even <- is.odd %|% un

    Doc$is.odd <- '
        is.odd returns a logical vector indicating whether each
        of the elements of the integer argument is odd. **** NOT
        TESTED **** for other than integer vectors and
        integer-valued double vectors.'

    Doc$is.even <- '
        is.even returns a logical vector indicating whether each
        of the elements of the integer argument is even. **** NOT
        TESTED **** for other than integer vectors and
        integer-valued double vectors.'

`%\\%` <- function(i, j) #TAGS rounding division ceiling
        (i %/% j) + as.logical(i %% j)

    Doc$`%\\%` <- '
        Note: Although the sourcecode and this doc string
        apparently require two backslashes to stand for one, only
        one is required on the R console. Application in
        sourcecode appears to require only one backslash.

        %\\% is Similar to the %/% operator, except that it rounds up.
        Useful for problems similar to determining the number of
        discrete containers needed to contain an amount of
        something (e.g., "How many egg cartons are needed to
        store 14 eggs? Answer: 14 %\\% 12").
        '


'
is_even <- `%%` %|% argswap %<=% 2L %O% `!`
is_not.even <- is_even %O% `!!` #UNTESTED
'

frac <- function(x) x %|% sign * (x %|% abs - x %|% abs %|% floor)

    Doc$frac <- '
        frac returns the fractional part of of the double
        argument. Example: frac(-1.25) == -.25.'

    #WAS   Doc$`standard form (integer)`   #NOW Doc$integers 
    Doc$integers <- '
        standard form (integer) refers to a particular
        representation of an integer.
        
        The standard form of a positive integer is a sequence of
        (possibly decimal) digits selected from b possible
        digits, where b (the base) is a natural number. The
        digits represent the values zero, one, two, ..., (b -
        one).  The initial digit is not the digit representing
        zero.
        
        The standard form of zero is, e.g.,
        
        0
        
        or some other digit representing zero.
        
        The standard form of a negative integer is the standard
        form of the absolute value of that integer, preceded by
        a symbol representing the sign of the integer, e.g., "-"'

as_percentage.d <- `*` %<=% 100L

`/2` <- `/` %|% argswap %<=% 2L #TAGS division divide

    Doc$`/2` <- '
        `/2` returns the result of dividing the argument by 2L.'

`diff/sum` <- function(a, b) (a - b) / (a + b)

    Doc$`diff/sum` <- '
        `diff/sum` returns the result of the difference of two
        vectors divided by their sum, (a - b) / (a + b)'

iPOW2 <- as.integer(2 ^ seq(from=0, to=30))

    Doc$iPOW2 <- 'iPOW2 is an integer vector of the first 31 powers of 2.'

ipow2 <- seq %O% (`[` %<=% iPOW2)

    Doc$ipow2 <- 'ipow2 returns the first n (arg 1 powers of 2), 1 <= n <= 31.'

as_integer.bb <- function (bb)
        (bb * ipow2(bb %|% `#`)) %|% sum

    Doc$as_integer.b <- '
        as_integer.b returns the integer equivalent of the
        unsigned, little-endian bit field argument. Examples:
        as_integer.b(c(T,T,T)) == 7L;   as_integer.b(rep(T, 31))
        == 2147483647L'

is_numeric.integer <- function(x) {
    if (x %|% mode != "numeric") return (rep(F, x %|% `#`))
    x == x %|% floor }

    Doc$is_numeric.integer <- '
        is_numeric.integer returns a logical vector indicating
        whether the elements of the vector argument are integers
        (which might be stored as double).'

is_among.contiguous.integers <- function(x, min_=-2^53, max_=2^53) {
    if (!is_numeric.integer(x)) return (rep(F, x %|% `#`))
    between(min_, max_, x) }

lsd <- function(n, base=10L)
        n %% base

    Doc$lsd <- '
        lsd returns a vector of nonnegative integers
        representing the least significant digits of the
        standard form of the natural number argument. Expect
        strange results if either n (arg 1) or base (arg 2) is
        not a positive integer.'

n.round.down.n <- function (n, base=10L)
        n - lsd(n, base)

    Doc$n.round.down.n <- '
        n.round.down.n returns an integer vector of values whose
        least significant digit (assuming base base) has been
        set to zero.'

n.right.shift.n <- function (n, base=2L)
        n %/% base

    Doc$n.right.shift.n <- '
        n.right.shift.n is designed for integer input (arg 1),
        and returns an integer vector where the digits (assuming
        the base specified, arg 2) have been shifted to the
        right (i.e., toward the least significant ditig) one
        place.'

pseudo.log=function(n, base=10L) {
    	truncate <- n.round.down.n %|% argswap %<=% base
    	shift <- n.right.shift.n %|% argswap %<=% base
    	0L -> m
    	while (0 < n) {
    		1L + m -> m
    		n %|% truncate %|% shift -> n }
    	m }

    Doc$pseudo.log <- '
        pseudo.log returns the number of digits needed to
        represent the natural number primary argument in
        standard form using the specified base (arg 2).'

bijective.numeral <- function(n, base=26L) {
    if (!is_among.contiguous.integers(n)) return (NULL)
    if (n  < 0) return(iNA)
    if (n == 0) return(integer())
    intermediate <- pseudo.log(n, base) %|% integer
    m <- 0L
    while (n) {
        m <- 1L + m
        intermediate[[m]] <- 1:base %[mod% n
        n <- n %|% pred %/% base }
    intermediate[1:m] }

