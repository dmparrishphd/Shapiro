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


# An Object for Storing the Coefficients of an N-Dimensional Polynomial
# Usage: foo <- source.object("current filename")
list(
    .DOC=list(
        doc='An object for storing and retrieving the coefficients of a polynomial

        a00 + a11 x + a 21 y + ... = 0
        
        ',
        eval='
            eval evaluates the left-hand-side of the polynomial and returns the result.

            One may elect to plot different color points or rasters depending on the sign of the return.',
        init='
            dim

            an integer-valued vector of two elements. Element 1 is the order of the polynomial (1 for a line in the plane). Element 2 is the dimension of the polynomial (2 for a line in the plane)
        ',
        .NULL=NULL ),
	.NA=NA_real_,
 	init=function(., dim, data=0, quiet=F, dimnames=character()) {
		.$.ORDER <- dim %|% second
		.$.DIM <- dim %|% first
		if (dimnames %|% is.empty)
				.$.DIMNAMES <- "X Y Z" %|% words
		if (.$.DIM <= dimnames %|% `#`) {
			.$.DIMNAMES <- dimnames[.$.DIM %|% seq]
		} else {
			.$.DIMNAMES <- (
				.$.DIMNAMES %,%
				vapply(max(0, .$.DIM - .$.DIMNAMES %|% `#`) %|% seqN, `%//%` %<=% "D", ""))[.$.DIM %|% seq]
			}
		if (!quiet) {
			cat(
				"Returning polynomial of dimension ", .$.DIM, ", order ", .$.ORDER, ".\n",
				sep="",
				file=stderr())
			if (2 < dim[[1]]) "Object is not reliable for dimension > 2." %|% warning }
		CrossTemp <- matrix.square(nrow=.$.ORDER %|% pred)
		CrossTemp[lower.tri(CrossTemp, diag=T)] <- 0
		.$.TERMS <- list(
			ZERO=0,
			PRINCIPAL=array(0, dim=dim),
			CROSS=array(
				CrossTemp,
				dim=
						dim %|% second %|% pred %|% rep2 %,%
						choose(dim %|% first, 2)))
		if (2 < .$.DIM) "Cannot populate data for higher order coefs" %|% warning
		if (1 < .$.DIM) .$.TERMS$PRINCIPAL[,1] <- data %[mod% (1L + seq(.$.DIM))
		if (0 < .$.DIM) .$.TERMS$ZERO <- data %|% first
		. },
	dim=function(.) .$.DIM,
	porder=function(.) .$.ORDER,
	.coef.n=function(., n) {
		if (n < 0) return (rep(.$.NA, .$.ORDER))
		if (!n) return (.$.TERMS$ZERO)
		if (n <= .$.ORDER) return(cols(.$.TERMS$PRINCIPAL, n))
		return (rep(.$.NA, .$.ORDER)) },
	.coef.lower=function(., n) {
		if (n < 0) return (double())
		if (!n) return (.$.TERMS$ZERO)
		if (n <= .$.ORDER) {
			if (1 < n) {
				"Not implemented for higher order polynomials."
				return () }
			return(.$.coef.n(., n - 1) %,% .$.coef.n(., n)) } },
	coef=function(., x=1, group="", ...) {
		if (group == "") {
			if (1 < .$.ORDER) "Not reliable for order > 1." %|% warning
			return (.$coef(., 1, "lower"))
		}
		if (group %|% uppeR == "LOWER")
				return (.$.coef.lower(., x))
		if (group %|% uppeR == "LINEAR")
				return (.$.coef.n(., 1))
		"Arguments do not match capabilities of coef function" %|% warning
		return (double()) },
    eval=function(., point) {
        COEF <- .$coef(.)
        if (.$.DIM != 2 || .$.ORDER != 1) warning("Not implemented for other than lines in the plane.")
        sum(COEF[1], COEF[-1] * point) },
	.NULL=NULL )
