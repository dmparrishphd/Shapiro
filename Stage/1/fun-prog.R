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
# Shapiro: A Handsome Helper for R
# Copyright (C) 2018 D. Michael Parrish

# Theme: Functional Programming
#
# TO DO:
#
#       VERIFY THAT THERE ARE NO DEPENDENCIES ON functional PACKAGE
#
#               install.packages('functional')
#               library(functional)
#
# HISTORY:
#
#       Old Name: fun-prog.R



pairwise <- function ( #TAGS apply
    X, FUN, FUN.VALUE=NULL, FUN.size=ncol,
    FUN.extract=cols %<=% X)
        capply(
            X %|% FUN.size %|% ipairs.n,
            FUN.extract %O% FUN, FUN.VALUE)

    Doc$pairwise <- '
        pairwise applies the function FUN to pairs of objects
        extracted from the object obj. The FUN.size argument is
        a function of obj and returns the size of an extracted
        object.  The FUN.extract function returns two extracted
        objects.

        **** NOT TESTED **** for other than cases where obj is a
        matrix, and FUN.size and FUN.extract are default.

        See m.lines.pg for an example.'


mimd <- function (
    info=list(),
    FUNs=list(constant(NULL)),
    positional=rep(list(list()), length(FUNs)),
    tagged=rep(list(character()), length(FUNs)),
    retagged=rep(list(list(list(), list())), length(FUNs)))
    lapply(
        seq_along(FUNs),
        function(i) do.call(
            FUNs[[i]], (
                l.extract (info, positional[[i]]) %,%
                nl.extract(info,     tagged[[i]]) %,%
                nl.extract(info,   retagged[[i]][[1]]) %|%
                        (rename.some %|% argswap %<=% retagged[[i]]))))

apply.permutations.to.many <- mimd


# BELOW: TO REORGANIZE


.do.call.nice.dot.only = function (what, args, quote, envir)
        do.call(what, args$..., quote, envir)

.do.call.nice.with.dot = function (what, args, quote, envir)
{   len=length(args) - 1 # assumption: the item named ... is at the end of args
    do.call(what, c(args[1:len], args$...), quote, envir)
}
.do.call.nice.selector = function (args)
{   if (!('...' %in% names(args))) return (do.call)
    if (length(args) == 1) return (.do.call.nice.dot.only)
    .do.call.nice.with.dot
}
do.call.nice = function (what, args, quote=F, envir=parent.frame())
{   'Usage---Preprocessing. Execute as.list(formals(what)) to
    get arguments of what as a list (formals returns a pairlist).
    Poplate the list items representing the non-optional
    arguments.  Overwrite any or all items representing the
    optional arguments (may overwrite with NULL). Either
    populate any dot-dot-dot argument or remove ... from the
    list by assigning it NULL. HINT: use the latent.call and
    lcopy functions
            Usage---Processing. Call this function with
    the modified as.list version of the formals return.'

    .do.call.nice.selector(args)(what, args, quote, envir)
}

do.call.nice.example = function()
{   plotargs = formals(plot)
    plotargs[[1]] = seq(3)
    plotargs[[2]] = plotargs[[1]] ** 2 # finished baking in all the required parameters of plot
    plotargs$... = list(type='l')
    function () do.call.nice(plot, plotargs)
}

call.parts = function (FUN)
{   l = list(FUN, as.list(formals(FUN)))
    names(l)=c('FUN','args')
    c(l, quote=F, envir)
}

as.univariate.fn = function (FUN, quote=F) function(x) do.call.nice(FUN, x, quote=quote) # TO DO: handle envir

function (FUN) function (args) do.call.nice(FUN, args, quote=F, envir=parent.frame())




latent.call = function (FUN, mods=list())
{   'Returns a list containing $FUN (the funciton FUN) and $args
    (a list of defaults for the formal parameters of FUN).
    Intended to be used with pseudoCurry and do.lc. These three
    functions do not yet fully support control over order of
    execution (e.g., using a latent.call version of scan,
    without assigning a quote argument results in an error).
    Static arguments seem to work well.'

    list(FUN=FUN, args=lcopy(lformals(FUN), mods))
}

pseudoCurry = function (lc, mods=list())
{ list(FUN=lc$FUN, args=lcopy(lc$args, mods))  }

`%~>%` = function (x, y) pseudoCurry(y, x)

do.lc = function (lc, quote=F, envir=parent.frame())
{   do.call.nice(lc$FUN, lc$args, quote=quote, envir=envir)
}



`%<%` <- function(x, y) curry(x, y)


Compose <- function(...)
{   'Acknowledgement:
    provided by Shane (2010)
https://stackoverflow.com/questions/2228544/higher-level-functions-in-r-is-there-an-official-compose-operator-or-curry-fun/26129498#26129498'
    fs <- list(...)
    function(...) Reduce(function(x, f) f(x), fs, ...)
}


Curry2 <- function(FUN, ...) # modified from Shane (2010)
{   outer... = list(...) # allows dots to be distinguished
    function(...)
            do.call(FUN, c(outer..., list(...)[[1]]))}

`%2<%` <- function(FUN, x) Curry2(FUN, x)

uncall = function (FUN, arg, val)
{   args=list(val)
    names(args)=arg
    function (...) do.call(FUN, args, ...)
}

uncall2 = function (FUN, lst) # works if the resultant calls w/ no args
{   l=lst;   function (...) do.call(FUN, lst, ...)
}

uncall3 = function (FUN, lst=NULL)
{   if (is.null(lst)) return (function () do.call(FUN))
    function (...) do.call(FUN, lst, ...)
}

uncall4 = function (FUN, lst=NULL) # works only if all positional parms are satisfied and 
{   if (is.null(lst)) return (function () do.call(FUN))
    function (...) do.call(FUN, lst)
}

`%$%` <- function (x, y) uncall4(x, y)


FUNloc <- function (FUN, x, na.rm=F)
        which(x == list(identity, rmna)[[na.rm %|% index.b]](x) %|% FUN)

v.FUNloc.v <- FUNloc #DEPRECATED use FUNloc
maxloc <- FUNloc %<=% max
minloc <- FUNloc %<=% min
v.maxloc.v <- maxloc #DEPRECATED use maxloc
v.minloc.v <- minloc #DEPRECATED use minloc

'
function mimd : multiple instruction multiple data apply

        Given

        info    an object containing some values to from which
                to select paramters for function calls,
                
        FUNs    a list of functions,

        positional

                a list of vectors or lists. One sublist per
                function. Each sublist contains indices into
                info, which may be either numeric or character.

        tagged  same structure as positional.

        retagged

                A compound list, one sublist per function. Each
                sublist contains two parallel vectors (or
                lists).  The first innermost vector contains
                tags found in info. The second innermost vector
                contains new tags that should be applied to the
                respective function call.

        Each function is called with parameters specified by
        positional (which become positional paramters), tagged,
        (which become tagged parameters with the SAME tag), and
        retageg (which become tagged parameters with NEW tags).

        The results of the function calls are gathered in a list
        and returned.
'

NULL
