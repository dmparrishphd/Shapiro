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



image_at <- function(
    z, origin=rep(0, z %|% ndim), xlab="", ylab="", useRaster=T, ...)
		do.call(image, c(
			rename.all(
				lapply(
					l.enumerate(z %|% grids.a),
					function(ki) origin[ki[[1]]] + ki[[2]]),
				"x y" %|% words),
			list(z=z, xlab=xlab, ylab=ylab, useRaster=useRaster),
			list(...) ) )

image_natural <- image_at

	Doc$image_natural <- '
        image_natural plots the matrix argument in its natural
        position---in Quadrant I of the Cartesian Plane,
        adjacent to the axes---and at its natural scale--- one
        usr unit per raster cell. The `...` is passed to image.
        As the x and y arguments of image are computed and
        applied internally, they should not be specified.

		image_natural is just an alias for image_at.
		May behave strangely if arguments other than those
		accepted by image are specified.'

	Doc$image_at <-  '
		image_at is similar to image_natural, except that
		an additional argument, origin, specifies the
		bottom-left corner of the plotted image.

		REMINDER: R-s indexing from one is consistent with
		an origin that is one less than the index of the
		first grid cell. consider:

		image_at(im.chess.board(), c(1, 1) - 1, col=YEL %,% BRN)
        '

.rasters <- function( #TAGS plot image grid
        x, y, z, zlim=z %|% finites %|% range,
        col=12 %|% heat.colors,
        add=T,
        xaxs="i", yaxs="i",
        xlab="", ylab="",
        breaks=NULL, oldstyle=F, useRaster=T, ...) {
    image(x, y, z,
        col=col, add=add,
        xaxs=xaxs, yaxs=yaxs,
        xlab=xlab, ylab=ylab,
        breaks= if (breaks %|% is.null) {
                    image_breaks(col %|% `#`, zlim)
                } else {
                    breaks},
        oldstyle=oldstyle,
        useRaster=useRaster,
        ...) }

rasters <- function (x, y, z, ...)
    if (missing(z)) {
            # NO z. MIGHT HAVE X AND Y
            if(!missing(y)) {
                stop(
                    'rasters: Provide
                    1) x, y, and z all or
                    2) z, but neither x nor y.' %|% longsentence)
            } else {
                .rasters( # NO z AND NO y. ASSUME x IS THE MATRIX
                x=seq_fromto(par.usr.x(), length.out=x %|% nrow %|% succ),
                y=seq_fromto(par.usr.y(), length.out=x %|% ncol %|% succ),
                z=x, ...)
    } } else {
        .rasters(x, y, z, ...) }

        
    Doc$rasters <- '
        rasters is to image as points is to plot. The rasters
        function is designed to plot a matrix onto an existing
        plot---in typical usage, xlim and ylim are left
        unspecified.'

