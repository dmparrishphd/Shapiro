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

strsplitbynewline <- strsplit %|% argswap %<=% HNL

crush.h <- strsplitbynewline %O% first %O%
        (vapply %|% argswap %<=% trimws %|% argswap %<=% "") %O%
        anonymize %O% `%//%`

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


