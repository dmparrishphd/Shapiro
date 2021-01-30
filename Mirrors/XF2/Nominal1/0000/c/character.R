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

nchar_max <- nchar %O% max

strspaces <- strrep %<=% " "

pad.h <- function(h, n=h %|% nchar_max, hpad=HSPACE) {
	n <- max(h %|% nchar_max, n)
    dimension(
        prefix(h, strrep(hpad, n)) %=>% right %:|% n,
        h %|% dim) }

    Doc$pad.h <- '
        pad.h returns a modified copy of the primary character vector
        argument. Each element of the return is the same width;
        hpad character strings are used to pad out to this
        width. The width is the maximum of the optional argument
        n and the maximum number of characters of the primary
        argument.
        
        HISTORY
        
        2019-10-21: now preserves dimensions of array arguments.

        pad.h(words("AS YOU WISH"))

        # [1] "  AS" " YOU" "WISH"

        pad.h(42 %,% "SIX TIMES SEVEN", hpad="0")

        # [1] "000000000000042" "SIX TIMES SEVEN"
        '

print_x <- function(FUN, x) {
   lapply(x, function (X) X  % %  FUN  % %  pad.h  % %  cat)  % %  invisible
    cat.eol() }
'

'

'""
rmnullstrings #MOVED TO THE rmALLTHE PACKAGE
rmnl #MOVED TO THE rmALLTHE PACKAGE
rmtabs #MOVED TO THE rmALLTHE PACKAGE
""'

strfixedwidth.regular <- function(string, nfields, width) #TAGS strsplit text columns delimited
        vapply(
            nfields %|% seq,
            FUN=function(i) substr(string, i %|% pred * width + L, i * width),
            FUN.VALUE="",
            USE.NAMES=F)

    Doc$strfixedwidth.regular <- '
        strfixedwidth.regular returns a character vector formed
        from the character string (arg 1).  Provided that
        nfields * width <= nchar(string) is of sufficient nchar,
        the return will contain nfields (arg 2) elements, each
        of width (arg 3) nchar. The form of the return will be
        different if nchar(string) < nfields * width.'





strrep2 <- strrep %|% argswap %<=% 2

    Doc$strrep2 <- '
        strrep2 returns a character string that is the
        concatenation of the argument with a copy of itself.'
