# Shapiro: A Handsome Helper for R
# Copyright (C) 2019 D. Michael Parrish
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


expand.interval <- function (interval, expansion)
        expansion %|% t %m*v% .ll %|% t + interval

    Doc$expand.interval <- '
        expand.interval returns a modified copy of the interval
        (arg 1). Relative to the interval argument, the return
        is expanded in each direction by the amounts indicated
        in the respective positions of the expansion (arg 2).
        
        > expand.interval(

        matrix(c(-pi, pi, -1, 1), nrow=2),
        
        matrix(c(pi/2, pi/2, .5, .5), nrow=2))

            [,1] [,2]

        [1,] -4.712389 -1.5

        [2,]  4.712389  1.5'

expand.interval.unif <- function (interval, expansion=0L)
        expand.interval(interval, interval * 0L + expansion)

    Doc$expand.interval.unif <- '
        expand.interval.unif returns a modified copy of the
        interval (arg 1). Relative to the interval argument, the
        return is expanded in each direction by the amount
        indicated by the single numeric expansion value (arg 2).

        > expand.interval.unif(matrix(c(-pi, pi, -1, 1), nrow=2), 1)

        [,1] [,2]

        [1,] -4.141593   -2

        [2,]  4.141593    2
        '

b.beyond.interval <- function (interval, x)
        vapply(
            x %|% seq_along,
            function(i) beyond2(interval[,i], x[i]),
            T)

    Doc$b.beyond.interval <- '
        b.beyond.interval returns a logical vector whose
        elements indicate whether arg 2 is beyond the interval
        (arg 1) **** IN THE CORRESPONDING UNSIGNED DIRECTION
        ****

        > m

        [,1] [,2]

        [1,]    0    0

        [2,]    1    1

        > b.beyond.interval(m, c(.5, .5))

        [1] FALSE FALSE

        > b.beyond.interval(m, c(.5, 1.5))

        [1] FALSE  TRUE'

within.interval <- b.beyond.interval %O% any %O% `!`

    Doc$within.interval <- '
        within.interval returns a logical vector whose
        elements indicate whether arg 2 is within the interval
        (arg 1).

        > m

        [,1] [,2]

        [1,]    0    0

        [2,]    1    1

        > within.interval(m, c(.5, .5))

        [1] TRUE

        > within.interval(m, c(.5, 1.5))

        [1] FALSE'

within.interval.m <- function (interval, m)
        rapply(m, within.interval %<=% interval)

    Doc$within.interval.m <- '
        within.interval.m is similar to within.interval, except
        that the second argument is a matrix with one dimension
        per column. The return in a logical vector with one
        element per row of the matrix.'

within.interval.df <- function (interval, df) {
    warning("IN PROGRESS")
    dfapply(df)

     }
