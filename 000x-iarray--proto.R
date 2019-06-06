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


# An object to facilitate indexing arrays by arbitrary values
list(
	DOC=list(
		init='
			a is an array

			FUNS is a list of functions, one per dimension.
			Each element of FUNS takes any value and returns
			a vector index into that dimension of the array.
			Any element of each vector might be NA_integer_.
		'
	),
	init=function(., a, FUNs) {
		.$.LARRAY <- a %|% list
		.$.FUNS <- FUNs
		. },
	`[`=function(., x, drop=T) {
		do.call(`[`, c(.$.LARRAY, lapply1to1(x, .$.FUNS), drop=drop))
		},
	.NULL=NULL )
