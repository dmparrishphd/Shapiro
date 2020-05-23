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



modinv <- function(n, m) #TAGS `%%` modular additive inverse
	-n %% m

	Doc$modinv <- '
		modinv is somewhat the opposite of `%%`. For fixed m,
		(n %% m + modinv(n, m) %% m == 0 == m %% m

        modinv(n, m) returns the number which, when added to n
        %% m, yields either 0 or m.
        
        > modinv(1:12, 4)

        # [1] 3 2 1 0 3 2 1 0 3 2 1 0

        > 1:12 %% 4

        # [1] 1 2 3 0 1 2 3 0 1 2 3 0

        > 1:12 %% 4 + modinv(1:12, 4)

        [1] 4 4 4 0 4 4 4 0 4 4 4 0
        '

multiples <- function(m, n)
	    m * n

    Doc$multiples <- '
        multiples is a wrapper for `*`. The intent is to promote
        more expressive code:
        
        thefives <- multiples(5, 1:20)
        
        thefives
        
        # [ 1]  5 10 15 20 25 30 35 40 45  50

        # [11] 55 60 65 70 75 80 85 90 95 100
        '



eqz <- function(x) x == 0 #TAGS equal to zero == 0
ltz <- function(x) x  < 0 #TAGS less than zero < 0
gtz <- function(x) x  > 0 #TAGS greater than zero > 0

    Doc$eqz <- '
        eqz, ltz, and gtz return logical vectors or arrays
        indicating whether each element of the argument is equal
        to zero, less than zero, or greater than zero,
        respectively.'

    Doc$ltz <- Doc$eqz
    Doc$gtz <- Doc$eqz

sum_pow <- function (x, n)
        x ^ seq(n) %|% sum

    Doc$sum_pow <- '
        sum_pow returns the sum of the first n powers of x.

        **** DESIGNED **** for single value arguments. There is
        **** NO CHECKING **** that this is indeed the case.
    
        > sum_pow(2, 4)

        [1] 30
        
        '


negate <- function (x) -x

    Doc$negate <- '
        negate returns the negative of the numeric vector.'

round2 <- function (x, digits=0) {
    k <- 2 ^ digits
    round(x * k) / k }

    Doc$round2 <- '
        round2 is a base-2 version of round. The digits argument
        refers to the number of binary digits to the right of
        the ones place. **** NOT TESTED **** for n < 0. ****
        NOT TESTED **** for non-integral values of n.
       
        > round2(c(1.375, 1.5, 1.875, 2.5, 3.5))
        [1] 1 2 2 2 4
        > round2(c(1:2/3, 1:4/5), 2)
        [1] 0.25 0.75 0.25 0.50 0.50 0.75
        '

`%*drop%` <- `%*%` %O% as.vector

    Doc$`%*drop%` <- '
        `%*drop%` is the same as `%*%`, except that the return
        is converted to a vector.'

iprod <- prod %O% as_integer.storage.mode
isign <- sign %O% as_integer.storage.mode
ifloor <- floor %O% as_integer.storage.mode
iceiling <- ceiling %O% as_integer.storage.mode

    Doc$ifloor <- '
        ifloor, iceiling, and isign are the same as floor,
        ceiling, and sign except that the return is an integer
        vector.'

    Doc$iceiling <- Doc$ifloor

odd <- function (i)
        i %|% ifloor * 2L  - 1L

    Doc$odd <- '
        odd returns the i-th (arg 1) integer odd number.
    
        > odd(2)
        
        3
        
        > odd(0)
        
        -1
        
        > odd(-3:3)
        
        [1] -7 -5 -3 -1  1  3  5'

compare <- `-` %O% isign
icompare <- compare %O% (`+` %<=% 2L)
compare0 <- isign

    Doc$compare <- '
        compare returns a vector of signs (each -1L, 0L, or 1L)
        of the (signed) difference between the primary argument
        (minuend) and the secondary argument (subtrahend).
        
        compare0 is similar to compare, except that,
        effectively, the secondary argument (subtrahend) is
        fixed at zero.
        
        > compare0(-2:2)

        [1] -1 -1  0  1  1

        > compare(-2:2, -1)

        [1] -1  0  1  1  1 '

    Doc$compare0 <- Doc$compare

    Doc$icompare <- '
        icompare is similar to compare, except that the return is two greater than that of comare.
        Intended for use in indexing: 1, 2, and 3 are natrual indices, where as -1 and 0, used as indices,
        have special behaior.'

`%\\%` <- function(i, j) #TAGS rounding division ceiling
        i %% j %|% as.logical   +   i %/% j

    Doc$`%\\%` <- '
        Note: Although the sourcecode and this doc string
        apparently require two backslashes to stand for one, only
        one is required on the R console. Application in
        sourcecode appears to require only one backslash.

        %\\% is Similar to the %/% operator, except that it rounds up.
        Useful for problems similar to determining the number of
        discrete containers needed to contain an amount of
        something (e.g., "How many egg cartons are needed to
        store 14 eggs? Answer: 14 %\\% 12").'

.ceiling_floor_multiple <- function(FUN, of, near)
        FUN(near, of) * of

ceiling_multiple <- .ceiling_floor_multiple %<=% `%\\%`
floor_multiple   <- .ceiling_floor_multiple %<=% `%/%`

    Doc$ceiling_multiple <- '
        ceiling_multiple and floor_multiple are intended for
        rounding numeric vectors (arg 2) to multiples of
        integers, as indicated by arg 1. As the names imply,
        these functions round up or down.'

    Doc$floor_multiple <- Doc$ceiling_multiple

sqr <- function(x) x * x

    Doc$sqr <- '
        sqr returns the squares of the numeric or complex vector
        argument. HISTORY 2019-05-24: formerly always returned
        double for integer arguments.'

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

rPOW2 <- iPOW2[1:8] %|% as.raw

b.r <- function(x)
        vapply(x, `&` %<=% rPOW2 %O% as.logical, logical(8))

unpackBits <- b.r

    Doc$b.r <- '
        b.r (alias unpackBits) returns a logical **** matrix
        **** representation of the bits represented by the raw
        **** vector **** argument. The return has one column for
        each element of the argument. The bit-order is ****
        ASSUMED **** to be little-endian.

        > b.r(as.raw(c(1, 128)))

              [,1]  [,2]

        [1,]  TRUE FALSE

        [2,] FALSE FALSE

        [3,] FALSE FALSE

        [4,] FALSE FALSE

        [5,] FALSE FALSE

        [6,] FALSE FALSE

        [7,] FALSE FALSE

        [8,] FALSE  TRUE'

ipow2 <- seq %O% (`[` %<=% iPOW2)

    Doc$ipow2 <- 'ipow2 returns the first n (arg 1 powers of 2), 1 <= n <= 31.'

as_integer.b <- function (bb)
        (bb * ipow2(bb %|% `#`)) %|% sum

as_integer.bb <- as_integer.b #DEPRECATED USE as_integer.b

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

lsd <- function(n, base=10L) #TAGS least significant digit
        n %% base

    Doc$lsd <- '
        lsd returns a vector of nonnegative integers
        representing the least significant digits of the
        standard form of the natural number argument. Expect
        strange results if either n (arg 1) or base (arg 2) is
        not a positive integer.'

.place.values <- function(base, n, bits=64) {
        lsd. <- lsd %|% argswap %<=% base
    	m <- 0L;   y <- integer(bits)
    	while (0 < n) {
    		m[1] <- m[1] + 1L
            y[m] <- n %|% lsd.
            n[1] <- (n - y[m]) %/% base }
    	y[y %|% which1rev %|% seq] }

place.values <- function (natural, base=10L)
        lapply(natural, .place.values %<=% base)

#TODO MAKE pretty.l TO PROVIDE CHARACTERS FOR PRINTING E.G., LISTS OF ATOMIC INTEGER VECTORS
    Doc$palce.values <- '
        place.values returns a list with one element for each
        element of the natural number vector argument. The
        elements of the list are **** LITTLE-ENDIAN **** integer
        vectors whose elements are the place values of the
        corresponding natural number
    
        place.values(8:15, 10)

[[1]]
[1] 8

[[2]]
[1] 9

[[3]]
[1] 0 1

[[4]]
[1] 1 1

[[5]]
[1] 2 1

[[6]]
[1] 3 1

[[7]]
[1] 4 1

[[8]]
[1] 5 1
    
> place.values(8:15, 8)
[[1]]
[1] 0 1

[[2]]
[1] 1 1

[[3]]
[1] 2 1

[[4]]
[1] 3 1

[[5]]
[1] 4 1

[[6]]
[1] 5 1

[[7]]
[1] 6 1

[[8]]
[1] 7 1


    
    '

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

.pseudo.log <- function(base, n) {
    	truncate <- n.round.down.n %|% argswap %<=% base
    	shift <- n.right.shift.n %|% argswap %<=% base
    	0L -> m
    	while (0 < n) {
    		1L + m -> m
    		n %|% truncate %|% shift -> n }
    	m }

pseudo.log <- function(n, base=10L)
        vapply(n, .pseudo.log %<=% base, 1L)

    Doc$pseudo.log <- '
        pseudo.log returns the number of digits needed to
        represent the natural number primary argument in
        standard form using the specified base (arg 2).'

#TODO: vectorize
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

    Doc$bijective.numeral <- '
        bijective.numeral returns a little-endian
        bijective-numeral, where each symbol is mapped to a
        natural number.'

isum <- sum %O% as.integer

i.bijective.numeral <- function(bn, base=26L)
        isum(bn * base ^ (bn %|% seq_along %|% pred))

i.bijective.numerals <- function(bn, base=26L)
        vapply(bn, i.bijective.numeral, L, base=base)

bijective.numerals <- function(i, base=26L)
        lapply(i, bijective.numeral, base)

    Doc$bijective.numerals <- '
        bijective.numerals returns a list of bijective numerals,
        computed by bijective.numeral. Each element of the retur
        corresponds to one element of the natural-number--valued
        vector argument.'
