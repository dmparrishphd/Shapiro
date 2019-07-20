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


# Theme: Functional Programming

fnNULL <- function(...){}

    Doc$fnNULL <- '
        fnNULL accepts any arguments and returns NULL;
        intended for use during testing.'

`%;;%` <- function (X, Y)
        c(list(X), list(Y))

`%;%` <- function (X, Y)
        c(X, list(Y))

`%[[%` <- function (X, i) X[[i]]

enlist <- function (X)
	    if (X %|% is.list) X else list(X)

specialize <- function(name, n=2) #TAGS % %x% speical binary operator
        eval.parent(
            parse(text=paste0("`%", name, "%`", "<-", name)),
            n=n)

    Doc$specialize <- '
        specialize creates a special binary operator in the
        environment in which specialize is called. The single
        argument is the name (as a character string) of an
        existing function of two arguments. The object created
        is attached to the name `%x%`, where x is the content of
        the character string argument.
        
        There is **** NO CHECKING **** that the arguments is a
        string, that the corresponding function object exists or
        is of the proper form, or that the name to be created
        does not already exist.
        
        > specialize("rbind")

        > 1:4 %rbind% 5:8

             [,1] [,2] [,3] [,4]
        [1,]    1    2    3    4
        [2,]    5    6    7    8
        '

EMPTY.LIST <- list()

first <- function(x)
        if (0 < x %|% length) {
            x[[1]]
        } else if (x %|% is.vector) {
            x
        } else {
            NULL }

`%-|%` <- function(FUN, x) FUN(x)

nop <- function(...) {}
no.operation <- nop
do.nothing <- nop

argswap <- function (FUN) function(x, y, ...) do.call(FUN, c(list(y, x), ...))

un <- function (f) function(...) !f(...)

compose <- function (g, f) function(...) f(g(...))
composer <- compose %|% argswap

`%O%` <- compose
`%-O%` <- composer

fn.table <- function (x, y)
        function(z) y[[match(z, x)]]

    Doc$fn.table <- '
        fn.table is a function of parallel vectors x and y.
        fn.table returns a function f of one argument. f returns
        the y that corresponds to the first x that matches the
        argument of f.'


#name it impartial ?
alternates <- function (FUN, ...) {
    warning("PLACEHOLDER: alternates will be similar to Curry,
            but with default args, rather than fixed args.")
    #function (...) FUN()
     }

curry <- function(FUN, ...) {
    'The present function has been adapted from Curry (
    https://stackoverflow.com/questions/2228544/higher-level-functions-in-r-is-there-an-official-compose-operator-or-curry-fun/26129498
    ), attributed to Aaron McDaid (accessed 2017-10), Copyright
    2014(--?) Stack Overflow, and is licensed and distributable
    under the terms of the Creative Commons Attribution-
    ShareAlike 4.0 International license (CC-BY-SA 4.0). The
    license (https://creativecommons.org/licenses/by-sa/4.0/legalcode)
    includes a disclaimer of warranties. The version of the R
    logo, as presented herein, has been modified as follows #1)
    changed = to <- , #2) revised comments per result of
    inspection / tests, #3) changed the name of the function to
    lower case, and #4) changed style of grouping symbols to a
    LISP-like style (see SICP). The preceeding numbering scheme
    refers to the items so numbered in the remaining code of
    this function.'
    .orig <- match.call() #1
    .orig[[1]] <- NULL #2 Remove first item, which matches FUN
    .orig[[1]] <- NULL #2 Remove second item, which matches the Curried argument
    function(...) {
        .inner <- match.call()
        .inner[[1]] <- NULL # Remove first item, which matches curry #3
        do.call(FUN, c(.orig, .inner), envir=parent.frame()) } } #4

Curry <- curry # Curry (capital C) is deprecated. use curry

`%=>%` <- function (X, FUN) do.call(curry, c(FUN, list(X)))
`%<=%` <- function (FUN, X) do.call(curry, c(FUN, list(X)))


placeholder <- warning %<=% "PLACEHOLDER"

`%=:%` <- function(LIST, newname_for_last_item) {
    names(LIST)[length(LIST)] <- newname_for_last_item;   LIST }

`%v%` <- function (LIST, FUN) do.call(curry, c(FUN, LIST))

`%^%` <- function (FUN, LIST) do.call(curry, c(FUN, LIST))
#WAS: `%^%` <- function (f, ...) do.call(curry, c(f, ...))

variants <- function (FUN, ARGS)
        lapply(ARGS, function(arglist) FUN %^% arglist)

`%do%` <- function(FUN, dummy) FUN()
`%call%` <- `%do%`

lformals <- formals %O% as.list

`%1|n%` <- function(X, FUNs) #TAGS apply
        lapply %<=% FUNs %-|% (`%|%` %<=% X)

    Doc$`%1|n%` <- '
        `%1|n%` applies (using lapply) each function of ..2 (a
        list of functions) to ..1. ..1 is treated as a single
        entity; members ..2 may or may not treat ..1 as a single
        entity. The return is a list containing the
        corresponding returns of ..2.'

`%1(-|)n%` <- `%1|n%` %|% argswap

    Doc$`%1(-|)n%` <- '
        `%1(-|)n%` is an argswap-ed version of `%1|n%`'

'
function un

        Returns a logical function that returns the logical
        inverse of the function argument. Example:

                > un(function(...) T)()
                [1] FALSE

        That is, in this case the argument of un is a function
        that always returns T, so un returns a function that
        always returns F.

function `%O%`

        Function Composition

        Designed for functions of one variable.

        Note: `%o%` is already taken by base (outer product of
        arrays)

        Example:

                > cos(sin(tan(1)))
                [1] 0.5403777

                > (cos %O% sin %O% tan)(1)
                [1] 0.5403777

                > (as.logical  %O% `#`)(list())
                [1] FALSE
                > # c.f. is_empty function

function `%O%`
function compose

        HISTORY 2018-07-10 switched order of arguments to
        facilitate multiple compose using %O%

        Returns a composed function. The call compose(g, f)
        returns the function f of g.

function `%v%`
function `%^%`

        Returns a partial function. The arguments (with or
        without names) supplied by the LIST are `baked into` a
        copy of the function FUN.

                > foo <- function(aa, bb, cc=64, dd=4) c(aa, bb, cc, dd)
                > foo(1, 2, 3, 4)
                [1] 1 2 3 4
                > (   foo %^% list(1, 2, cc=3, dd=4)   )   ()
                [1] 1 2 3 4
                > ( list(1, 2, cc=3, dd=4) %v% foo) ()


                

REFERENCES

Shane (2010) Compose and Curry. StackOverflow. https://stackover
        flow.com/questions/2228544/higher-level-functions-in-r-
        is-there-an-official-compose-operator-or-curry-fun?
        noredirect=1&lq=1
'
