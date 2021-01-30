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

'""
argswap #MOVED TO neatoFunProgA
un #MOVED TO neatoFunProgA
`%O%` #MOVED TO neatoCompose
`%:O%` #MOVED TO neatoCompose
""'

compose <- `%O%` #DEPRECATED USE `%O%`
composer <- `%:O%` #DEPRECATED USE `%:O%`
`%-O%` <- `%:O%` #DEPRECATED USE `%:O%`



`%-|%` <- `%:|%` #DEPRECATED USE `%:|%`
`% %` <- `%|%` #DEPRECATED USE `%|%`

fn.explode <- function(FUN) #TAGS multi-variable single variable
		function(...) FUN(unlist(list(...)))

    Doc$fn.explode <- '
        fn.explode converts a function of a vector into a function of many variables.
    
        fn1 <- function(x) sum(x)
        
        fn2 <- fn.explode(fn1)
        
        fn2(1,2,3,4,5,6)
        
        # 21'

gather <- function(FUN, END.FUN) {
	Y <- list()
	repeat {
		Y <- FUN() %|% list %,% Y
		if (Y %|% END.FUN) break }
	Y }

    Doc$gather <- '
        gather calls FUN (arg 1), a function of no arguments,
        until END.FUN (arg 2) returns FALSE for an argument that
        is the return of FUN.
        
        The returns are gathered into a list, with the most
        recent item being item 1.'

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

epithet <- function() #TAGS function name called
	    match.call(
		    definition=sys.function(-1),
    		call=sys.call(-1),
	    	expand.dots=F) %|% first %|% as.character %|% first

    Doc$epithet <- '
        epithet returns the name of the function from which
        epithet is called.

        STATUS: IN DEVELOPMENT

        Expect strange results if the function does not have a name.
        
        > foo <- function() epithet()
        
        > foo()
        
        [1] "foo"
        '


nop <- function(...) {}
no.operation <- nop
do.nothing <- nop




parameters <- formals %O% names #TAGS arguments help

fn.table <- function (x, y)
        function(z) y[[match(z, x)]]

    Doc$fn.table <- '
        fn.table is a function of parallel vectors x and y.
        fn.table returns a function f of one argument. f returns
        the y that corresponds to the first x that matches the
        argument of f.'


#name it impartial ?
alternates <- function (FUN, ...) {
    warning("PLACEHOLDER: alternates will be similar to curry,
            but with default args, rather than fixed args.")
    #function (...) FUN()
     }




placeholder <- warning %<=% "PLACEHOLDER"

`%=:%` <- function(LIST, newname_for_last_item) {
    names(LIST)[length(LIST)] <- newname_for_last_item;   LIST }


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

'

'""
`%=>%` #MOVED TO neatoCurrySBOP
`%<=%` #MOVED TO neatoCurrySBOP
`%v%` #MOVED TO neatoCurrySBOP
`%^%` #MOVED TO neatoCurrySBOP
`%-|%` #MOVED TO neatoPipeSBOP as `%:|%`
""'

