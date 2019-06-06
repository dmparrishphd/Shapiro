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

    Doc$pg <- '
        pg is a prefix, infix, or suffix used in some names to
        indicate a polygon. Herein, a polygon is a matrix of
        points. If the polygon is in 2D space, it-s transpose is
        compatible with the R graphics::polygon function.'

i.closed.pg <- ncol %O% seq %O% i.closed

lines_pg <- function (pg, ...)
        lines(pg[, pg %|% i.closed.pg] %|% t, ...)

line.points <- function(points)
	c(
		det(points),
		-points %|% secondr %|% diff,
		+points %|% firstr  %|% diff)

    Doc$line.points <- '
        line.points returns the vector of coefficients

        c(c, a, b)

        of the 2-D line

        c + ax + by = 0

        passing through the 2-D points given in the matrix
        argument.  **** UNLIKE **** many of the R functions,
        each point of the argument occupies a COLUMN, rather
        than a ROW, of the matrix.'

pairwise <- function (FUN, obj, FUN.size=ncol, FUN.extract=cols)
        rapply(
           obj %|% FUN.size %|% pairs.n,
           FUN.extract %<=% obj %O% FUN)

lines_pg <- pairwise %<=% line.points

    Doc$lines_pg <- '
        lines_pg returns a matrix of lines, one for each pair of
        points in the polygon argument, one line per column of
        the return. The order of the lines is consistent with
        the order of the points of the polygon argument (i.e.,
        the first line is from point 1 to point 2)'

.rat.spread <- function (m) c(
    m %|% det2 %|% sqr,
    m[,1] %|% ssq * m[,2] %|% ssq )

rat.spread <- function (lines)
        lines[2:3,] %|% .rat.spread

    Doc$rat.spread <- '
        rat.spread returns the spread between two lines (arg 1)
        as a ratio.'

rat.spreads.lines <- pairwise %<=% spread.frac

    Doc$rat.spreads.lines <- '
        rat.spreads.lines returns a matrix of spreads, one for
        each pair of lines in the lines argument, one spread per
        column of the return. The order of the spreads is
        consistent with the order of the points of the lines
        argument (i.e., the first spread is that between line 1
        and line 2).'

rat.spreads.pg <- lines_pg %O% spread.fracs.lines

    Doc$rat.spreads.pg <- '
        rat.spreads.pg returns a matrix of spreads, one for each
        pair of lines in the polygon argument, one spread per
        column of the return. The order of the spreads is
        consistent with the order of the points of the polygon
        argument (i.e., the first spread is that between the
        first pair of sides or the spread of the lines formed by
        points 1 and 2 and points 2 and 3).'

is.rectangle <- rat.spreads.pg %O% `r==` %O% all
        # TEST IF ALL OF THE SPREAD RATIOS HAVE EQUAL NUMERATOR
        # AND DENOMINATOR, I.E., IF ALL THE SPREADS ARE EQUAL TO 1.

solveline.x <- function(line, x)
	-(line[[1]] + line[[2]] * x) / line[[3]]

solveline.y <- function(line, y)
		solveline.x(line[1 %,% 3 %,% 2], y)

.solveline <- function(line, x=NULL, y=NULL)
		if (is.null(y)) {
			solveline.x(line, x)
		} else {
			solveline.y(line, y) }

solveline <- function(line, x=NULL, y=NULL) {
		solution <- .solveline(line, x=x, y=y)
		replace(solution, solution %|% is.finite %|% `!`, NA) }
        
line.parallel <- function (line., point.)
        -dot.prod(line.[2:3], point.) %,% line[2:3]

    Doc$line.points <- '
        line.parallel returns the vector of coefficients

        c(c, a, b)

        of the 2-D line

        c + ax + by = 0

        parallel to the 2-D line (arg 1) and passing through the
        2-D point (arg 2) given in the matrix argument.'

walk <- vapply_ %|% argswap %<=% seq0 #TAGS geometric vector

    Doc$walk <- '
        walk returns a (possibly higher dimensional) sequence
        from the zero vector to the "position" specified by the
        primary argument.
        
        Additional arguments are passed to seq.
        
        In most cases, the length.out named argument will also
        need to be specified, in order to ensure that
        intermediate results are of the same length.'


walk.pg <- function(pg, length.out=1L) #REF Wildberger (sp?) N
        do.call(rbind, lapply(
            pg %|% nrow %|% seq,
            function(i) rename.all.rows.m(
                m.add.m..v(
                    (rows %|% argswap %<=% -1)( #TODO why doesn't rest.m work?
                        walk(
                            rows %|% argswap %<=% i %-|% (
                                rbind(pg, rows %<=% pg %-|% 1) %|% diff),
                            length.out=length.out %|% succ %[mod% i)),
                    rows %<=% pg %-|% i %|% as.vector),
            (rep %<=% i)(length.out %|% succ %[mod% i - 1))))

    Doc$walk.pg <- '
        walk.polygon returns a walk around the given polygon
        (arg 1). The steps of the walk are uniform for a given
        polygon side.
    
        The length of the "legs" of the walk along each side of
        the polygon are given by the optional length.out
        argument. The default value of 1L results in a return
        that is equal to the primary argument.
    
        Elements of length.out are recycled if necessary to
        obtain one value per polygon side.'                                               
geom.Square.PROTO <- list(
    init=function (., side.length=1L) {
        .$.SIDE.LENGTH <- side.length
        . },
    as.pg=function(.) {
        as_xy(c(
                        0L,             0L,
            .$.SIDE.LENGTH,             0L,
            .$.SIDE.LENGTH, .$.SIDE.LENGTH,
                        0L, .$.SIDE.LENGTH))},
    .NULL=NULL)                                        

geom.Rectangle.PROTO <- list(
    init=function (.=geom.Rectangle.PROTO, dims=1L %|% rep2) {
        .$.WIDTH  <- dims[1]
        .$.HEIGHT <- dims[2]
        . },
    width =function(.) .$.WIDTH  %|% as.double,
    height=function(.) .$.HEIGHT %|% as.double,
    dims=function(.) .$.WIDTH %,% .$.HEIGHT %|% as.double,
    as.pg=function(.) {
        as_xy(c(
                  0L,        0L,
            .$.WIDTH,        0L,
            .$.WIDTH, .$.HEIGHT,
                  0L, .$.HEIGHT))},
    walk.uniform=function(., scale=1L)
            walk.pg(
	            . %|% .$as.pg,
                length.out=round(. %|% .$dims %|% rep2 * scale)),
    .NULL=NULL)                                        

        

