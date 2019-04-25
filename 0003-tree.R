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
.h.t <- function(t)
{   h <- h.l(iterate.simple(defoliate(t, unleaf="."), rake, is.list.simple))
    selection <- h!="."
    h[selection] <- prefix(h[selection], " ")
    h <- `%//%`(h) % % trimws % % words.h
    h <- gsub("^:[.]$", ".", h)
    h <- gsub(":[.]$", "", h)
    h <- prefix(h, " ")
    h <- `%//%`(h)
    h <- gsub(" : [(]", " (", h)
    h <- h % % words.h
    h }

h.t <- function (t, htab="\t")
{   if ("list" != t  % %  typeof) return ("*\n")
    ntb <- 0L # Number of TaBs
    prv <- 3 # PREVious item class
    # Item Classes: 1: open list, 2: close list, 3: other
    Hat <- function (tb, it=item, pit=prv) if (pit == 3) it else `%//%`(heol, tb, it)
    hhh <- character()
    itemclass <- Curry(n.classify.discrete, "( )"  % %  words.h, null=3)
    jumptable <- list( # Aligned with Item Classes
            function (ht, n) list(Hat(`%//%`(rep(ht, n))), n  % %  succ),
            function (ht, n) list(                    ")", n  % %  pred),
            function (ht, n) list(Hat(`%//%`(rep(ht, n))), n           ))
    for (item in .h.t(t)) {
        jumptable[[itemclass(item)]](htab, ntb) -> tmp
        hhh <- hhh %,% tmp[[1]]
        ntb <-         tmp[[2]]
        prv <- itemclass(item) }
    hhh %,% heol }

catt <- function (t) cat(h.t(t, htab="    "))

'
function .h.t

        Helper function for h.t

        Returns a character vector where each element is a token
        representing part of the tree argument. Tokens are  "."
        for unnamed items "(" for beginning of list, ")" for end of
        list, and names for named items. Then name of a named
        list is affixed with a colon (:).

function h.t

        Produces a character representation of a tree.

        A tree is a list, typically containing other lists.

        Given a tree (arg 1) and an optional character string
        (arg 2) to stand in for a tab character (suggestion:
        four spaces), returns a character vector which could be
        displayed using cat. The returned character vector is
        a representation of a tree. Examples:

        > ht <- Curry(h.t, htab="    ")
        > cat(ht(list()))
        ( ) 
        > cat(ht(list(I="Section I")))
        ( 
            I ) 
        > cat(ht(list(I="Section I", II=list(A="Part A",
        + B="Part B"))))
        ( 
            I II: ( 
                A B ) ) 
        > cat(ht(list(I="Section I", II=list(A="Part A",
        + B="Part B"), III=list())))
        ( 
            I II: ( 
                A B ) 
            III: ( ) )
        > cat(ht(list(I="Section I", II=list(A="Part A",
        + B="Part B"), III=list("Thing 1", "Thing 2"))))
        ( 
            I II: ( 
                A B ) 
            III: ( 
                . . ) ) 


'
