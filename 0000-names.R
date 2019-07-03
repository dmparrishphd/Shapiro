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



with_dimnames <- function (x, dimnames.) {
    dimnames(x) <- dimnames.;   x }

    Doc$with_dimnames <- '
        with_dimnames is a functional version of dimnames.'

regular.dimnames <- function(.dim, prefixes=LETTERS[.dim %|% seq_along], sep=" ")
		lapply(
			.dim %|% seq_along,
			function(k) paste(prefixes[k], .dim[k] %|% seq, sep=sep))

    Doc$regular.dimnames <- '
        regular.dimnames returns a list that can be used as arg
        2 of dimnames<-. The names are produced by postfixing
        the first few natural numbers (as character strings) to
        the specified prefixes.

        > regular.dimnames(c(2, 3), sep=".")

        [[1]]

        [1] "A.1" "A.2"

        [[2]

        [1] "B.1" "B.2" "B.3"'


rename.all <- function (x, newnames=NULL) {
    names(x) <- newnames;   x }

rename.all.dims <- function (x, newnames=NULL) {
    dimnames(x) <- newnames;   x }

sans.row.names <- function (x) {
        row.names(x) <- NULL;   x }

