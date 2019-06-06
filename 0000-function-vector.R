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

#USAGE: foo <- source.object("current filename")
list(
	DOC=list(
		DOC='
			A function vector object.
			Intended use: call the functions corresponding to the elements of the argument.

			> qux <- foo$init(foo, list(cos, sin, sin %O% `-`, cos))

			> ROTATE45 <- qux$`(`(qux, matrix2(rep4(pi/4)))

			> ROTATE45

			          [,1]       [,2]

			[1,] 0.7071068 -0.7071068

			[2,] 0.7071068  0.7071068


			> ROTATE45 %*% c(1, 0)

		          [,1]

			[1,] 0.7071068

			[2,] 0.7071068',
		init='',
		`(`='
			`(` may be used to call the function vector with a vector or array argument.
			The return has the same dimensions as the argument.',
		.NULL=NULL ),
	init=function(., FUNs, FUN.VALUE=NA_real_) {
		.$.FUNS <- FUNs
		.$.LENGTH.ACTUAL <- FUNs %|% `#`
		.$.FUN.VALUE <- FUN.VALUE
		. },
	`(`=function(., x)
		dimension(
			vapply1to1(
				x,
				.$.FUNS[x %|% seq_along %mod1% .$.LENGTH.ACTUAL],
				.$.FUN.VALUE),
			x %|% dim.),

	.NULL <- NULL )
