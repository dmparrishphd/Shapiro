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

xy.pp <- first2r %O% t

    Doc$xy.pp <- '
        xy.pp transforms a pp (polypoint) object into an xy
        object. Only the first two dimensions are preserved.'

points_pp <- function(x, ...) points(x %|% xy.pp, ...)

    Doc$points_pp <- '
        points_pp is the same as points, EXCEPT that the first
        argument should be a pp (polypoint) object.'


'""
pg # POLYGON #SEE ALSO: doc(pg)
""'

    Doc$pg <- '
        pg is a prefix, infix, or suffix used in some names to
        indicate a polygon. Herein, a polygon is a matrix of
        points, one point per column, one row per dimension. If
        the polygon is in 2D space, it-s transpose is compatible
        with the R graphics::polygon function.
        
        The representation of a polygon and a polypoint is
        identical. The difference is interpretation.
        
        "It is better to have 100 functions opperate on one data
        structure than to have 10 functions operate on 10 data
        structres"---Alan Perlis, foreward, [SICP]'

i.closed.pg <- ncol %O% seq %O% i.closed

lines_pg <- function (pg, ...)
        lines(pg[, pg %|% i.closed.pg] %|% t, ...)

line.pp <- function(pp)
	c(
		det2(pp),
		-pp %|% secondr %|% diff,
		+pp %|% firstr  %|% diff)

line.points <- line.pp #DEPRECATED USE line.pp

    Doc$line.pp <- '
        line.pp returns the vector of coefficients

        c(c, a, b)

        of the 2-D line

        c + ax + by = 0

        passing through the 2-D points given in the matrix
        argument.

        When the LHS is evaluated, the result will be positive
        for points to the "left" of the line and negative for
        points to the "right" of the line, consistent with the
        right-hand rule.
        
        **** UNLIKE **** many of the R functions,
        each point of the argument occupies a COLUMN, rather
        than a ROW, of the matrix.'

    Doc$line.points <- line.pp


.rat.spread <- function (m) c(
    m %|% det2 %|% sqr,
    m[,1] %|% ssq * m[,2] %|% ssq )

rat.spread <- function (lines)
        lines[2:3,] %|% .rat.spread

    Doc$rat.spread <- '
        rat.spread returns the spread between two lines (arg 1)
        as a ratio.'


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

eval.lines <- function (lines, points)
        lines[-1,] %|% t %*% points + lines[1,]

sign_eval.lines <- function (lines, points)
        compare(lines[-1,] %|% t %*% points, -lines[1,])

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

        

