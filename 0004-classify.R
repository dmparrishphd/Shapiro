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
# R Script

classify.self <- function (x) {
    u <- x %|% uniques
    list(
        VALUES=u,
        INDICES=vapply_(x, match %|% argswap %<=% u)) }

    Doc$classify.self <- '
        classify.self (c.f. indexed color) returns a list Y
        whose first element contains the unique elements of the
        vector argument and whose second element is a vector of
        indices into the first element. A copy of the argument
        vector may be recovered via (Y[[1]])[Y[[2]]].'

classify.self.m <- function (m)
        vapply_(m %|% as.data.frame, classify.self %O% second)

m.classify.self.df <- classify.self.m

classify <- function (a, breaks=0, compar=`>=`)
{   FUN <- function (x) compar(x, breaks)  % %  sum
    aapply(a, FUN, 0L) }

threshold <- function (v, lim) v >= lim

classify.unif <- function (x, n=2)
        classify(x, breaks=divisions(n)[-n]) %|% succ

    Doc$classify.unif <- '
        classify.unif classifies the data from the uniform
        distribution found in arg 1 into the number of equally
        sized bins specified by arg 2.

        Zero values are classified with those values just
        greater than zero, and unit values are classified with
        those values just less than one.

        Returns a vector of natural numbers, UNLIKE the default
        classify function, whose return would include zeroes
        (since the uniform distribution is bounded, there are no
        "lower than" items).'

classify.unif.symmetric <- function (v, n=2)
        classify(v, breaks=divisions_symmetric(n)[-2 * n])  % %  succ

#classify.unif.symmetric.odd <- function (v, n=2)
        #classify(v, breaks=divisions_symmetric(1L + n)[-2 * n])  % %  succ

m.classify.v <- function (v, breaks=0, compar=`>=`)
        matrix(unlist(lapply(sort(breaks), compar, v)), nrow=length(v))

.histogram <- function (i)
      if (i  % %  `#`) vapply(
        i  % %  max  % %  seqN,
        function(j, k) sum(j == k),
        integer(1),
        i) else integer()

histogram <- function(i) #TAGS classification
    i  %|%  rmna  %|%  .histogram

histogram.auto <- function (X) { #TAGS classification distinct discrete
    classes <- X %|% uniques
    list(x=classes, y=match(X, classes) %|% histogram) }

    Doc$histogram.auto <- '
        histogram.auto returns a list of two aligned elements.
        The first element (named x) contains the unique elements
        of the argument. The second element (named y) contains
        the counts of those elements. Example:
        plot(histogram.auto(c(2, 3, 3, 4, 4, 5)), type="h")'

df.histogram.auto <- function(X)
        as.data.frame(
		    rename.all(
			    histogram.auto(X),
			    "_N" %|% characters),
        row.names=1)

    Doc$df.histogram.auto <- '
        df.histogram.auto is similar to histogram.auto, except
        that the return is in the form of a data frame, where
        the unique members of X are the row.names, and the
        single column (named "N") is the count of items found in
        that category.'

histogram.sparse <- function(i) { #TAGS classification
    u <- i  %|%  uniques
    h <- matrix(
        c(u, vapply(u, curry(sum, na.rm=T) %O% `==`, integer(1), i)),
        ncol=2,
        dimnames=list(NULL, words.h("VALUE FREQUENCY")))
    if (i  % %  b.nas  % %  any) rbind(h, c(NA, n.nas(i))) else h }

categorize <- function (dims, m) #TAGS classify classification
        rowply(m, dims %|% n.index.i.factors %=>% n.index.i, FUN.VALUE=integer(1))

    Doc$categorize <- '
        Cross Reference: categorize_self function

        Given upper bounds (arg 1) on the natural number
        (integer vector) parallel vectors in the matrix or data
        frame (arg 2), returns an integer vector of natural
        numbers that each uniquely specify the combination of
        values in the parallel vectors at the corresponding
        position.

        **** WARNING **** Strange values may be returned if one
        or more elements of arg 1 is not an integer vector of
        natural numbers or if one ore more elements of the upper
        bounds is less than any of the values found in the
        corresponding integer vector. 

        > g <- seq(3)

        > reading <- data.frame(
                        GRADE.LEVEL=rep(g, 3),
                        READING.LEVEL=c(g, g + 1L, g + 2L))

        > row.names(reading) <- sort("FRODO
        SAM MERIDOC PEREGRIN GANDALF GIMLI
        BOROMIR FERIMIR LEGOLAS" %|% words)

        > cbind(reading, CATEGORY=categorize(c(3,5), reading))
                         GRADE.LEVEL READING.LEVEL CATEGORY
                BOROMIR            1             1        1
                FERIMIR            2             2        5
                FRODO              3             3        9
                GANDALF            1             2        4
                GIMLI              2             3        8
                LEGOLAS            3             4       12
                MERIDOC            1             3        7
                PEREGRIN           2             4       11
                SAM                3             5       15'

categorize.self <- function (m) { #TAGS classify
    capply(m, max, integer(1)) -> dims
    categorize(dims, m) }

categorize_self <- categorize.self #DEPRECATED USE categorize.self


df.with.category.df <- function (df1, cols=1)
        cbind(
            df1,
            cols.m(df1, cols) %|% classify.self.m %|% categorize.self)





'
function categorize_self : classify classification

        Cross Reference: categorize function
 
        Same as categorize, but takes only one argument: the
        parallel vectors. The upper bounds on the values therein
        are determied by analyzing that data.

        Example 2:

                > df1 <- data.frame(
                        DBKEY=rep(words.h("00000 00001"), 6),
                        MO=sort(rep(seq(2), 6)),
                        DA=rep(5L * seq(6), 2))

                > df1 <- cbind(df1, IKEY=as.integer(df1$DBKEY))

                > cbind(df1, CATEGORY=categorize_self(df1[,words.h("IKEY MO")]))
                   DBKEY MO DA IKEY CATEGORY
                1  00000  1  5    1        1
                2  00001  1 10    2        2
                3  00000  1 15    1        1
                4  00001  1 20    2        2
                5  00000  1 25    1        1
                6  00001  1 30    2        2
                7  00000  2  5    1        3
                8  00001  2 10    2        4
                9  00000  2 15    1        3
                10 00001  2 20    2        4
                11 00000  2 25    1        3
                12 00001  2 30    2        4


function m.classify.v 

        Designed with integer and double vector data in mind.

        Given a vector (arg 1), a set of breaks (optional vector
        arg 2), and a compar function (optional arg 3), returns
        a classificaiton matrix (rows=length(breaks)) where each
        column contains

                compar(v, breaks[column number])

        And, for the default case,
        
                v < 0

        Note: breaks is sorted prior to application.


function classify

        Designed with integer and double array data in mind.

        FOR UNIFORMLY DISTRIBUTED values in [0, 1], where there
        is a good chance that 1 is in the data set, try the
        classify.unif function.

        In the default case, specifying the breaks as the set of
        all integers (if that were possible) would result in the
        equivalent of the floor function.

        Returns an integer array (same dimensions as the primary
        argument) that tells which class the inputs fall into.
        The number of classes is equal to one more than the
        number of breaks.

        Examples:

                > classify(-1:1)
                [1] 0 1 1
                > classify(1:3, 1:3)
                [1] 1 2 3

        Classes are numbered beginning with 0. If numbering
        beginning with 1 is preferred, this can be accomplished
        with a trivial workaround. For example:

                > 1 + classify(-1:1)
                [1] 1 2 2

        or

                > classify(-1:1, breaks=c(-Inf, 0))
                [1] 1 2 2

        Note: breaks is sorted prior to application.

        HISTORY

                2018-12-05: formerly returned vector

                

function histogram

        Counts the occurrences of the values in the integer vector argument,
        whose members are assumed to natural numbers or NA.
        Returns an integer vector where element 1 is the count
        of ones in the argument, element 2 is the count of twos,
        and so on. Examples:

                > histogram(c(2, 3, 3, 4, 4, 5))
                [1] 0 1 2 2 1

                > classify(d, breaks=seq(...))  % %  succ  % %  histogram

function histogram.sparse

        See also: histogram

        Returns a histogram of the data that is represented as
        an integer vector i.
        
        The full range of integers, including NA, is accepted.

        The return is a matrix where column 1 contains the
        distinct values in i and column 2 contains the
        corresponding frequencies of occurrence.  Any NA values
        are represented at the end of the return; the return is
        otherwise sorted in the order determined by sort.

        Example:

                > histogram.sparse(c(2,3,3,4,4,5,NA))
                     VALUE FREQUENCY
                [1,]     2         1
                [2,]     3         2
                [3,]     4         2
                [4,]     5         1
                [5,]    NA         1





'
