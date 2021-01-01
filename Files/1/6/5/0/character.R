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



strsplitbynewline <- strsplit %|% argswap %<=% HNL

.longword <- strsplitbynewline %O% unlist %O% trimws

longword     <- .longword %O% `%//%`
longsentence <- .longword %O% `%/ /%`

    Doc$longword <- '
        longword returns a modified copy of the character vector argument.
        The return is a character string having the individual characters.
        of the argument, but with all whitespace removed.

        Use case: constructing filenames from formatted text

        longword("
                
                 http://

                 this.

                 url.

                 is.

                 too/

                 long/

                 for/

                 one/

                 line/

                 of.

                 html")
                 '

crush.h <- strsplitbynewline %O% first %O%
        (vapply %|% argswap %<=% trimws %|% argswap %<=% "") %O%
        anon %O% `%//%`

    Doc$crush.h <- '
        crush.h formats a word that is split among several
        lines. After splitting by newline, leading and trailing
        whitespace are removed from each line. Finally, the
        lines are concatenated. Internal whitespace is
        preserved.

        >   crush.h("
                ~/Files/
                My Documents/
                Library/
                SICP/")
        [1] "~/Files/My Documents/Library/SICP/"
    ' 


split_alpha <-function(x, n=1)
		split %<=% x %-|% left(x, n)

    Doc$split_alpha <- '
        split_alpha splits the character vector alphabetically
        by the first n characters. The result is a structure
        analogous to the indices of many books.

        > split_alpha("rodents unusual size" %|% words)

        $`r`

        [1] "rodents"

        $s

        [1] "size"

        $u

        [1] "unusual"
        '

lsa <- function(n=1) {
	WORDS <- split_alpha(
		eval.parent(parse(text="ls()")),
		n=n)
	for (k in WORDS %|% seq_along) {
		cat(
            "",
            "",
            names(WORDS)[[k]],
            "",
            WORDS[[k]],
            hrule(),
            sep=HEOL) } }

    Doc$lsa <- '
        lsa cat-s the result of ls to stdout in a formatted
        manner: names are grouped according to split_alpha.'
        
.strbar.length <- function (h, length.)
        if (length. < 1) {
            character()
        } else {
            strrep(h, length %\% nchar(h))[1:length.] }

strbar <- function (h, length.out=NULL, times=1) {

     }


