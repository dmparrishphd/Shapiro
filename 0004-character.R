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
strsplitn <- function(h, n) {
    #warning("strsplitn returns a value consistent with strsplit as of 2018-10-17.")
    dimension(
        substr(h, 1, n) %,% substr(h, succ(n), nchar(h)),
        c(h  %|%  `#`, 2))  %|%  rows.m }

    Doc$strsplitn <- '
        strsplitn returns a list of character vectors; the list
    elements correspond to the elements of the 
    character vector argument.
    Each of the elements of the return contains a character
    vector of length 2, where the first element contains a
    character string with the first n individual characters from
    the corresponding character vector argument, and the second
    element contains the remaining individual characters.'

.unright <- function (h, n) strrev(strsplitn(strrev(h), n)[[1]][2])

unright <- function (h, stop=1) vapply_(h, .unright, stop)

    Doc$unright='
        Similar to right, but EXCLUDES the individual characters
        from the stop position onward. unright("RIGHT", 2) ==
        "RIG"'

substrs <- function(h, stops=nchar(h))
	colply(
		rbind(1L %,% succ(unrest(stops)), stops),
		substr2  % %  argswap,
		"",
		h)

Doc$substrs=crunch.h("
        Similar to substr, but allows a vector of stops rather
        than a single stop. The corresponding starts are
        computed such that the start for one substring is one
        more than the stop of the previous substring. The start
        of the first substring is 1.")

