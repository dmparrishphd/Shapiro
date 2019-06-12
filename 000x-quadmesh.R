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
	#TODO: include provision for different orientations?
	DOC=list(
		self='
			works with an attribute array that stores the
			cartesian cooridinates of points indexed by
			their topological coordinates in a quad mesh.

			See also 0001-attrib-array--proto.R',

		iquad='
			returns a matric of topological indices of the
			points that form the quadrilateral of the cell
			whose n-dimensional index is given.',

		itris='
			returns the topological indices (a list of two
			matrices) of the points that form the TWO
			triabgles of the cell whose n-dimensional index
			is given.',

		tris='
			returns the points (a list of two matrices)
			that form the TWO triangles of the cell whose
			n-dimensional index is given.',

		quad='
			returns the indices of the points
			that form the quadrilateral of the cell whose
			n-dimensional index is given.',

		.area.coords='
			return area coordinates for the two consitutent
			triangles of a quadrilateral element.',

		bb='
			returns a bounding box for the cell specified
			by cell index.',

		.NULL='' ),


	init=function(., points.) {
		.$.POINTS <- points.
		.$.DIM <- .$.POINTS$.DIM - 1L
		.range.check=range.check %<=% .$.DIM
		.$.TRIANGLE1 <- .$tris(., 1 %,% 1)[[1]]
		.$.CENTROID1 <-  .$.TRIANGLE1 %|% centroid
				# CENTROID OF THE "FIRST" TRIANGLE
		.$.AREA.SIGN <- area.coords(.$.TRIANGLE1, .$.CENTROID1)[1] %|% sign
				# ALL THE AREA COORDS SHOULD HAVE THE SAME SIGN, AS THE POINT IS AT THE CENTROID
		. },
	dim=function(.) .$.DIM,
	iquad=function(., i)
		xy.square.() %m+v% i,
	itris=function(., i) {
		SEASHELL <- 4 %|% i.triangles.seashell
		IQUAD <- .$iquad(., i)
		list(
			IQUAD[SEASHELL[,1],],
			IQUAD[SEASHELL[,2],]) },
	tris=function(., i)
			lapply(
				.$itris(., i),
				function(m) .$.POINTS$`[`(.$.POINTS, m)),
	quad=function(., i)
			.$.POINTS$`[`(.$.POINTS, .$iquad(., i)),
	.area.coords=function(., i, point)
		lapply(
			.$tris(., i),
			function(m) area.coords(m, point)),
	.in=function(., i, points) {
		#ASSUMPTION: QUAD IS CONVEX
		i <- sign_coords(.$quad(., i), points)
		!capply(i, 0L %=>% `>` %O% any, T) & # NOT OUTSIDE
		i[2,] & # NOT ON LINE 2. REMINDER: 0L ==> ON THE LINE
		i[3,]   # NOT ON LINE 3
		},
	`in`=function(., i, point) {
		if (point %|% is.matrix) return (.$.in(., i, point))
		AREAS <- .$.area.coords(., i, point)
		if (AREAS[[1]][2] == 0) return (F) # ON THE LINE AT "RIGHT" - BELONGS TO OTHER CELL
				# INCLUDES CASE WHERE AT "LOWER RIGHT" OR "UPPER RIGHT" CORNER
		if (AREAS[[2]][2] == 0) return (F) # ON THE LINE "ABOVE" - BELONGS TO OTHER CELL
				# INCLUDES CASE WHERE AT "UPPER LEFT" OR "UPPER RIGHT" CORNER
		if (all(AREAS[[1]] %|% sign != -.$.AREA.SIGN)) return (T) # IN THE FIRST TRIANGLE
		if (all(AREAS[[2]] %|% sign != -.$.AREA.SIGN)) return (T) # IN THE SECOND TRIANGLE
				# USING "!= -" INSTEAD OF "==" BECAUSE WANT TO sign OF ZERO INDICATES ON THE LINE, COUNTED AS IN THE TRIANGLE
		F },
	bb=function(., i)
			rapply(.$quad(., i), range) %|% t,
	.NULL=NULL )
