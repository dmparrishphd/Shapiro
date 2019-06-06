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


list(
	DOC=list(
		doc='
			An obect for handling array data, where only the non-default
			values are stored in array blocks.

			An array block is a contiguous array section.

			Originally intended to facilitate merging array blocks
			into a "parent array"',
		init='
			x is an array, considered to be an array block of a parent array.
			The parent array is assumed to be indexed from 1.
			The extent of the parent array is not stored.

			origin is the n-dimensional index, relative to the parent array,
			of the first element of the array block.',
		dim='dim returns the dimension of the array block.',
		length='length returns the number of elements of the array block.',
		as.array='as.array returns the array block as an array.',
		origin='
			origin returns the origin of the array block.
			See also documentation for init.',
		ibb='
			ibb returns the bounding box of indices for the array block.
			This information might be used for plotting.',
		merge='
			merge ',
		.NULL=NULL ),
	init=function(., x, origin=NULL) {
		.$.ARRAY <- x
		.$.ORIGIN <- if (origin %|% is.null) x %|% ndim %|% ones else origin
		.$.IBOUNDING.BOX <- rbind(.$.ORIGIN, .$.ORIGIN + dim(x) - 1L) %|% with.dimnames.bb
		. },
	dim=function(.) .$.ARRAY %|% dim,
	length=function(.) .$.ARRAY %|% length,
	as.array=function(.) .$.ARRAY,
	origin=function(.) .$.ORIGIN,
	ibb=function(.) .$.IBOUNDING.BOX,
	merge=function(X) {
		ibb <- lapply(X, X[[1]]$ibb) %|% bb.bb
		TEMPLATE <- dimension(
			na(
				mode=do.call(
					max_typeof,
					lapply(X, X[[1]]$as.array %O% first)),
				length=ibb %|% diff_inclusive %|% prod),
			ibb %|% diff_inclusive)
		X[[1]]$init(
			X[[1]],
			dimension(
				do.call(
					overlay,
					lapply(
						X,
						function(x)
								replace.a.at(
									TEMPLATE,
									x$as.array(x),
									x$origin(x) %[-]% ibb[1,]) %|%
								as.vector)),
				TEMPLATE %|% dim),
		 	ibb[1,])

		},
#	image_x=function(.) .$.LATTICE.DOMAIN[,1] %|% i.seq.range %|% pred,
#	image_y=function(.) .$.LATTICE.DOMAIN[,2] %|% i.seq.range %|% pred,
	.NULL=NULL )
