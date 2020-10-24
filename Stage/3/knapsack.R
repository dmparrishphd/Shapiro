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

# KNAPSACK

overburden <- function (knapsack, items,
        kitems="items", fnencumbrance="fnencumbrance", capacity="capacity", room=NULL)
{
        #caterr("knapsack[[fnencumbrance]](knapsack[[kitems]]): ")
        #cater(knapsack[[fnencumbrance]](knapsack[[kitems]]))
        #caterr("knapsack[[fnencumbrance]](items): ")
        #cater(knapsack[[fnencumbrance]](items))
        #caterr("knapsack[[capacity]]: ")
        #cater(knapsack[[capacity]])

        knapsack[[fnencumbrance]](knapsack[[kitems]]) +
        knapsack[[fnencumbrance]](items) -
        knapsack[[capacity]]
}

pack <- function (knapsack, items, kitems="items", ...)
{   while ((items  % %  length) && 
            (overburden(knapsack, items[1], kitems=kitems, ...) <= 0)) { #NEW
            #(overburden(knapsack, list(items[[1]]), kitems=kitems, ...) <= 0)) { #OLD
        knapsack[[kitems]] <- c(knapsack[[kitems]], list(items[[1]]))
        #if (!length(items)) break
        items <- items  % %  rest }
    list(knapsack, items) }

knapsack.items <- l.zip.l %O% first #NEW
#knapsack.items <- function (items) items  % %  l.zip.l  % %  first #OLD

#TODO: Return should consistently have remaining items at the end, which could be NULL
knapsack.partition <- function (knapsack, items, kitems="items", ...) {
    grouped.items <- list()
    LAST <- list()
    while (items  % %  length) {
        tmp <- pack(knapsack, items, ...)
        if (  tmp[[1]][[kitems]]  % %  length) grouped.items <- c(grouped.items, list(tmp[[1]][[kitems]]  % %  knapsack.items))
        if (!(tmp[[1]][[kitems]]  % %  length)) { # no items in knapsack
            if (items  % %  length) LAST <- tmp[[2]]
            break }
        items <- tmp[[2]] }
    c(grouped.items, list(LAST), LAST  % %  `#`) }

knapsack.fnenc.direct <- l.zip.l %O% second %O% unlist %O% sum

        Doc$knapsack.fnencumberence.direct <- crunch.h('
                knapsack.fnencumberence.direct is a function
                that may be used with the knapsack family of
                functions. It computes the encumberence of a
                list of items where the list of attributes
                contains a single element for each item: the
                encumberence of that item.')

'
knapsack.fnencumberence.direct(
                               l.zip.l(
                                       list(as.list(seq_along(CWIDTHS[[1]])),
                                       lapply(as.list(CWIDTHS[[1]]), list))
                                       )
                               )

'                

knapsack.form.simple.attributes <- function (x) lapply(x  % %  as.list, list)

     
'

TEST DATA

ITEMS <- list(
    list("Thing 1", list(NA)),
    list("Thing 2", list(NA)),
    list("Thing 3", list(NA)))

ITEMS <- list(
    list("Thing 1", list(1)),
    list("Thing 2", list(2)),
    list("Thing 3", list(3)))

overburden(
    list(fnencumbrance=length, capacity=1, items=list()),
    ITEMS)

pack(
    list(fnencumbrance=length, capacity=1, items=list()),
    ITEMS)

knapsack.partition(
    list(fnencumbrance=length, capacity=1, items=list()),
    list(list("Thing 1", NA), list("Thing 2", NA)))

knapsack.partition(
    list(fnencumbrance=constant(1), capacity=0, items=list()),
    list(list("Thing 1", NA), list("Thing 2", NA)))

knapsack.partition(
    list(fnencumbrance=constant(1), capacity=99, items=list()),
    list(list("Thing 1", NA), list("Thing 2", NA)))

'

             'One solves the most difficult problem
             first, and all other problems are
             solved by themselves.---C-H Ting.'

#TODO ALLOW LONG WORDS TO BE BROKEN, TO FORCE A FIT---MAYBE USE cpaginate
fmt <- function (
        h, width=min(65L, getOption('width')), empty=HNULL) {
    w <- h %|% words
    if (w %|% `#` %|% `!`) return (empty)
    items <- l.zip.l(list(prefix(w, HSPACE)  %|%  as.list,
            lapply(w  %|%  nchar  %|%  succ, as.list)))
    enc <- l.zip.l %O% rest %O% unlist %O% sum
    knapsack <- list(fnencumbrance=enc, capacity=width %|% succ, items=list())
    kp <- knapsack.partition(knapsack, items)
    if (kp  %|%  last) return (h  %|%  words  %|%  unwords  %|%  characters)
    lapply(lapply(lapply(kp  %|%  unrest  %|%  unrest, unlist), `%//%`),
            characters %O% rest %O% `%//%`)  % % unlist }

catpar <- function (h) cat(h %|% fmt, sep=HEOL)

    Doc$catpar <- '
        catpar sends a character string to stdout. The text is
        formatted to fit the console: all consecutive sequences
        of whitespace are converted to single spaces, then lines
        of text are formed by replacing strategically chosen
        whitespace characters with line breaks.'

catpars <- function (h, split="\n\n")
        for (hh in separate(strsplit(h, split) %|% unlist, HNULL))
                hh %|% catpar

    Doc$catpars <- '
        catpars behaves much the same as catpar, except that
        sequences of exactly two newline characters are
        represented as blank lines in the output. FUTURE: allow
        for soft line breaks.'

doc.msg.fail <- function (h)
        'No documentation for "' %//% h %//% '" in ShapirEnv.'

arg.text <- function (call_, formal.arg.name)
        (call_ %|% as.list)[[formal.arg.name]] %|% as.character

    Doc$arg.text <- '
        arg.text returns the actual argument (as a character
        string) of the unevaluated call and formal argument
        specified specified.
        
        Intended use (from within another function): 

        argument_text <- arg.text(match.call(), "formal argument
        name")

        Compare to the C program:

        #include <stdio.h>
        int main (int argc, char * * argv) {
            printf("%s", argv[0]) /* prints the first argument */
            return 0; }
        '

doc <- function(h) {
    h <- arg.text(match.call(), "h")
    restore(Doc[[h]], h %|% doc.msg.fail) %|% catpars }

    Doc$doc <- '
        doc cat-s text to stdout that describes the object
        argument.'

cfmt1 <- function (h, column.height=24, FUN.height=function(h) 1) {
    enc <- function(h) vapply(h, FUN.height, 1)  % %  sum
    knapsack <- list(fnencumbrance=enc, capacity=column.height, items=list())
    lapply(knapsack.partition(knapsack, h), unlist)  % %  unrest  % %  unrest }

cfmt <- function (h, widths=strwidths, heights=strheight, ncol=1, parms=list()) {

    fmt(h, )

     }

'

cfmt1(fmt(bede()))
cfmt1(bede(), par.usr.height(), strheight)
'


'
function overburden : capacity fullness overfull
function pack : fit
function knapsack.items : extract
function knapsack.partition : divide group
function fmt : UNIX fmt command

        A family of functions related to the "classic" knapsack
        problem and its variations.

        Any dot-dot-dot arguments should match the optional
        arguments of the overburden function:
                        
                kitems: The name of the list of items of the
                        knapsack.

                fnencumbrance:
                
                        The name of the encumbrance function of
                        the knapsack
                
                capacity:

                        The name of the capacity value of the
                        knapsack

        At present (2018-02-23), no attempt is made to solve the
        knapsack problem. Rather, these functions could possibly
        be used in exploring the problem.

        The present application is the development of the fmt
        function, a greatly simplified version of the UNIX fmt
        command.


        KNAPSACK-s

        For the purposes herein, a knapsack is a list containing
        three items:

                1. A capacity value---an integer or double
                containing a single element

                2. An encumbrance function

                3. A list of items

        For example, this would be a valid knapsack:

                list(fnencumbrance=function(x)length(x),
                        capacity=10L, items=list())

        i.e., the encumberence of a group of items is simply the
        number of items; a single knapsack can hold 10 items,
        and the knapsack currently has no items (empty list).

        The encumbrance function returns a value comparable to
        the capacity value. It takes one argument: a list of
        items, such as specified above.

        The list of items has the following properties: each
        item is a list. This list has two items:

                1. The item proper

                2. A list of attributes for #1. The only
                requirement for these attributes is that they be
                compatible with the encumbrance function. The items
                proper need not all have the same number or type
                of attributes, so long as the encumbrance
                function can handle the heterogeneity.

function overburden

        Returns the difference between the capacity required to
        add the items (arg 2) to the knapsack (arg 1), which may
        already be storing items. The sign of the result
        indicates whether the items will fit.

        FUTURE: allow for precomputation of room, so that the
        knapsack\'s fullness need not be recomputed every time
        will.fit is evaluated.

function pack

        Given a knapsack (arg 1) and some items (arg 2), returns
        a list containing two items: 1) a knapsack and 2) some
        items.  The knapsack in the return contains as many
        items (arg 2) as would fit (or all the items, in the
        case where they are exhausted before the knapsack is
        filled to capacity).  The second item in the
        return---some items---are those that did not fit into
        the knapsack.

function knapsack.items

        Returns the items proper (i.e., without attributes) of
        the items argument, which has the form consistent with a
        knapsack.

function knapsack.partition

        Given a knapsack (arg 1) and some items (arg 2), returns
        a compound list where each inner list contains as many
        items as would fit in one knapsack, until all of the
        items are exhausted.

        In the case where an item is found that does not even
        fit into a knapsack alone, the operation returns with a
        warning and all of the remaining items may be found in
        the last inner list.

function fmt : format text line width

        An application of the family of knapsack functions.

        Given a character string and an optional width argument,
        returns a character vector where the words have been
        "typeset" into lines not longer than the width. Words
        in the character string argument are identified as
        sequences of substrings separated by whitespace, where
        whitespace is consistent with the trimws function.

        The width argument may be Inf, in which case all of the
        words of the character string will be "typeset" to the
        single character string return value.

        Examples:

                > fmt(bede(), width=64-22)
                 [1] "Britain, an island in the Atlantic,"       
                 [2] "formerly called Albion, lies to the"
                 :
                 [78] "longest day or night extends but to"       
                 [79] "fifteen hours, and the shortest to nine."

function cfmt : column format text

        Given a 

'
