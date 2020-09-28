# Shapiro: A Handsome Helper for R
# Copyright (C) 2018 D. Michael Parrish
'
SHAPIRO

        One theory suggests that it derives  from
        Shpira,  the Hebrew/ Yiddish name for Spira...
        pronounced Shpira, which is an Aramaic borrowed
        word meaning "handsome."*


Author

D. Michael Parrish, PhD+

 
History
        2018-12-11  Put all code in single function
                    Changed default "file" parameter
        2018-03-05  Revised and posted to RUG 
        2018-02-02  Initiated


Abstract

I am developing the shapir.R et seq. R scripts to solve some++
of my computable** problems.


Introduction

THE NAME. In many regex systems, the period (.) can stand for
any character; if we choose "o," we get "Shapiro." Additionally,
"shapirr" is an anagram for my surname.

THE AUDIENCE I hope to communicate with herein is, foremost, my
future self (see Abstract). However, I hope also to provide for
the beginner or intermediate programmer, and for beginner R
programmer some examples of the kinds of things that can be done
in R. Additionally, I invite the vicious, constructive criticism
of experienced programmers: I want a useful, robust system that
works.

THE PROBLEMS that I work on most often center on pre- and
post-processing of numerical and simple text data associated
with numerical modelling.

THE SOURCE CODE is what you are reading now. This part of the
source code is a character string with which R will do almost
nothing: it is effectively a comment.
______
* The "handsome" opening quotation is taken from Wikipedia
(2018-02-02) "Shapiro." That article, in turn, cites Hebrew-
English, English-Hebrew Dictionary in three volumes, by Israel
Efros, Ph.D., Judah Ibn-Shmuel Kaufman Ph.D, Benjamin Silk,
B.C.L., Edited by Judah Ibn-Shmuel Kaufman, Ph.D., The Dvir
Publishing Co.  Tel-Aviv, 1950
+ South Florida Water Management District
++ This series does not attempt to present even all of my
computable problems, let alone solutions to them (for one
thing, this series is restricted to problems for which I
think R is a good tool for attempted solutions).
** Of course, not all problems are computable. For those
problems, there are other approaches. See, e.g., Donald
Knuth (2009-03-16) Talks at Google. YouTube JPpk-1btGZk .

CONVENTIONS

        Phonocentrism: functions names may have meaning
        according to the sound of their names.
                Cosmopolitanism: function names may be borrowed
        from other languages, particularly French (Fr.), Latin
        (Lt.), Greek (Gr.), and German (De.)
                Brevity: short words good; long words bad.
        Better a hundred one-line functions than one,
        one-hundred--line function. Better one function
        definition and two function calls than duplicate
        statements.
                Vector-centric: functions both receiving and
        returning vectors have no type-prefixes or type
        suffixes.
                Tradition: names for e.g., function names,
        formal parameter names, and the like, are borrowed from
        other programming languages: C, Fortran, and Python, in
        particular.
                Natural Numbers (see Integers).
                Integers: The integers strictly greater than
        zero will be considered the natural numbers. R begins
        indexing with 1 by default.
                Ordering: The term "sorted", unless otherwise
        specified, means "sorted in ascending order."
                Endianness: Where there is not an obvious choice
        to the contrary, we generally prefer little endian,
        since, "Little endian is the correct endianness, anyway"
        (Steil M [2008] The Ultimate Commodore 64 Talk. 25c3;
        Berlin).


        "FIXES"

                Fix     Meaning
                .       stands in for a character that would
                        require the name to be quoted, or
                        an aesthetic separator

                kk      having to do with circles, including
                        circular indexing or circular lists.
                        The Greek workd for circle is kyklos.

                Prefixes

                Prefix  Meaning: The function returns a(n)...

                (none)  vector, typically of the same type as
                                the primary argument; or as
                                implied by the name of the
                                function.
                b       logical (Boolean)
                bb      logical (Boolean)--a bit field--or
                        a bounding box
                cn      connection (file handle)
                d       a double vector
                fn      function
                i       integer vector, possibly an index
                ij      ij matrix, typically containing
                        integer-valued xy "points" or indices
                        into a matrix.
                im      indexed color image matrix.
                img     image matrix (possibly an im)
                l       list (possibly a named list)
                m       matrix
                mi      matrix-index (see documentation for `[`)
                n       a singleton natural number
                nl      named list
                q       quadrance, quadrea, quadrume, etc.
                xy      xy matrix, typically suitable as a plot
                        argument.

                Prefix  Meaning

                .       possibly, hidden
                c       The function concerns columns (e.g., of
                        a matrix or of text.)
                
                Prefix  Note
                
                im.     defined in image-ext.R
                
                .       helper function; not intended to be
                        called directly


                Infixes

                .       aesthetic separator
                _       aesthetic separator---used instead of .
                        in order to avoid interaction with
                        generic functions

                Suffixes

                r       reverse
                .       sometimes used when the characters
                        preceeding the . form a name that is
                        apready built into R. Example: "range."

                "Dot" Suffixes (a dot, followed by *)

                *       The function-s first parameter is a/an...
                -       -----------------------------------------
                a       array
                bb      logical (Boolean)--a bit field--or
                        a bounding box
                l       list
                m       matrix
                pg      polygon
                v       function receives a vector argument

                "Dot Dot" Suffixes
                
                        a dot, followed by a character sequence
                        consistent with a "Dot" Suffix


        HUNGARIAN NOTATION - FUNCTIONS

                c       "column"
        
                        The function operates on the columns of
                        the primary argument.  May return an
                        array having a number of elements that
                        is a multiple of the number of columns
                        of the primary argument.

        HUNGARIAN NOTATION - VALUES

                a       an array
                b       a logical (Boolean) vector
                c       a complex vector
                d       a double vector
                df      a dataframe
                f       a double vector (floating point)--DEPRECATED
                f       a file or filename
                fn      a function (typically applied only when
                                it is not otherwise obvious that
                                the symbol refers to a function)
                h       a character vector, often a character
                                string. Note: "h" is the second
                                letter in "character" and the
                                first letter in "Holerith"
                i       an integer vector, perhaps
                        a vector of indices
                l       a list or
                        (deprecated) a logical vector
                lc      latent call (see the latent.call function)
                ll      a compound list (list of lists) or tree
                m       a matrix
                n       a singleton integer vector containing a
                                natural number
                nnn     integer vector of positive integers
                r       a raw vector
                t       a tree (list, possibly---and typically---
                                containing (an)other list(s))
                tf      a logical vector (TRUE/FALSE)
                v       a vector, often, but not necessarily, an
                                integer vector
                vn      a vector of natural numbers
                ygreg   a year (numeric or character, depending
                        on context), e.g. 1970. The Gregorian
                        Calendar is assumed.



REFERENCES

[DP] Wildberger NJ (2005) Divine Proportions: Rational
        Trigonometry to Universal Geometry. Wild Egg Pty Ltd;
        Sydney, Australia.  http://wildegg.com



ACKNOWLEDGEMENTS

https://www.vim.org/
online dictionaries, especially those accessed via
https://onelook.com/ , including especially
https://www.wordsmyth.net/ and
https://en.oxforddictionaries.com/


FUNCTION COMMENTS (for functions in this file.)

function `% %` : function application

        Usage: arg  % %  fn
        Example: 1  % %  as.logical

function prefix : character string concatenate

        A modified version of character vector h is returned,
        with each element prefixed with the longest substrings
        of elements of p, recycled as in paste.

        Examples:

            > prefix(c("database", "metry"), "geo")
            [1] "geodatabase" "geometry"

            > prefix(c("1", "x^2/2!", "x^4/4!", "x^6/6!"), c("+", "-"))
            [1] "+1"      "-x^2/2!" "+x^4/4!" "-x^6/6!"

function shapir.bootstrap

        Example Bootstrap Procedure

                > source("R:/shapir.R")
                > shapir.bootstrap("R:/", file="shapir-ext.txt")

        Description

                System Bootstrap

                hpath (arg 1): full path to the root of the R
                system to be bootstrapped. Must include the
                final separator, e.g. "/"

                file (arg 2): The name of a text file. Each line
                of this file contains the name of a source file
                at the locaiton indicated by hpath. These files
                will be sourced.

                local (arg 3): The environment to apply to
                source. The designe is for this environment to
                be ShapirEnv.
                
                Effect

                        ... sourcing ...
                        ... ShapirEnv ...
                        ... bootstrap ...

        Internal Functions

                l$prefix:

                        Returns the argument, prefixed with
                        hpath.

                l$source:

                        This function source-s the file
                        specified by the argument, where the
                        full path is the result of applying
                        l$prefix to the argument.
                
                l$source.f:

CODE

        commences on the next line...' 

function(
        hpath, file="shapir-ext.txt", local=ShapirEnv,
        selfname="Shapir", quiet=T) {

    while ("ShapirEnv" %in% search()) detach(ShapirEnv)

    ShapirEnv <<- new.env(parent=globalenv())
    #cat("shapir.R (post precleaning):\n")

.reattach <- function() {
    while ("ShapirEnv" %in% search()) detach("ShapirEnv")
    attach(ShapirEnv)
    NULL }

    ShapirEnv$Doc$.reattach <- '
        .reattach is intended for **** INTERNAL USE. **** It
        **** ASSUMES **** ShapirEnv is at position 2 in the
        search path.  Originally intended to be used for
        bootstrapping Shapiro, to enable using functions as soon
        as they are defined.'

    #ShapirEnv$

    ShapirEnv$Doc <<- list("")
            # FIRST ELEMENT IS SPECIAL. USED BY doc FUNCTION.

    ShapirEnv$
`%|%` <<- function(x, FUN) FUN(x)

    .reattach()

    ShapirEnv$
unformat <<- function (h, whitespace="( |\t|\r|\n)+") gsub(whitespace, " ", h) %|%  trimws

    ShapirEnv$
`% %` <<- function(x, FUN) FUN(x)

    ShapirEnv$
prefix <<- function (h, p="-") paste(p, h, sep="")

    ShapirEnv$Doc$prefix <- '
        prefix returns a modified version of the character
        vector argument (arg1) where each element has been
        prefixed by the character string specified by arg2.'

    ShapirEnv$Shapir <<- list()

    ShapirEnv$Shapir$Stack <<- list()

    ShapirEnv$Doc[["`% %`"]] <- "DEPRECATED. Use %|% ."

    ShapirEnv$Doc[["`%|%`"]] <-
            "Function application: x %|% FUN ==> FUN(x) ."

    attach(ShapirEnv) # MAKE COMPONENTS AVAILABLE NOW

    hpath ->> ShapirEnv[[selfname]]$Path
    local ->> ShapirEnv[[selfname]]$Local
    (function()ShapirEnv[[selfname]]$Path) ->> ShapirEnv[[selfname]]$path
    (function(h) prefix(h, ShapirEnv[[selfname]]$path())) ->>
            ShapirEnv[[selfname]]$prefix
    (function(h) lapply(
        h  % %  ShapirEnv[[selfname]]$prefix,
        source,
        local=ShapirEnv[[selfname]]$Local)  % %  invisible) ->>
                ShapirEnv[[selfname]]$source
    ShapirEnv[[selfname]]$source.f <<- function(infile) {
        list() -> loadlog
        namesLoaded <- as.list(rep(NA_character_, 2))
        for (h in scan(infile, what=character(), quiet=T)) {
            namesLoaded[[1]] <- namesLoaded[[2]]
            if (!quiet) {
                cat(
                "\n\nShapirEnv[[selfname]]$source.f:",
                "loading extension: ", h, "\n")
            }
            ShapirEnv[[selfname]]$source(h)
            c(h, loadlog$Filenames) -> loadlog$Filenames
            c(length(ShapirEnv[[selfname]]$Local), loadlog$Item.Subtotal) ->
                    loadlog$Item.Subtotal
            namesLoaded[[2]] <- names(ShapirEnv)
            if (!quiet) {
                cat("\nNewly-added names:\n")
                cat(
                    sort(
                        namesLoaded[[2]] [
                            !( namesLoaded[[2]] %in% namesLoaded[[1]] ) ]),
                    sep="\n")
            }
        }
        loadlog }
    loadlog <- ShapirEnv[[selfname]]$source.f(
            file %|% ShapirEnv[[selfname]]$prefix)
    data.frame(
        Items=unlist(loadlog$Item.Subtotal) -
                c(unlist(loadlog$Item.Subtotal)[-1], 0),
        Item.Subtotal=loadlog$Item.Subtotal,
        row.names=unlist(loadlog$Filenames))[
                length(loadlog[[1]]):1,] -> dfloadlog
    print(dfloadlog)
    while ("ShapirEnv" %in% search()) detach("ShapirEnv")
    attach(ShapirEnv)
    NULL %|% invisible }

