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



grids.i <- function(i) #TAGS image raster fencepost error
	c(i[1] %|% pred, i)

	Doc$grids.i <- '
		grids.i returns a numeric vector suitable for the
		x or y arguments of image, provided that the
		arguments is a contiguous sequence of integers and
		the corresponding raster cells are regular.
        
        See also grids.a'

grids.a <- function(a)
	lapply(a %|% dim, seq %O% grids.i)

    Doc$grids.a <- '
        grids.a is similar to grids.i, except that the return is
        a list containing one vector for each dimension of the
        array argument.'

diptych <- function( #TAGS image matrix side-by-side
        z1,
        z2,
        x=0:(z1  % %  nrow + z2  % %  nrow),
        y=-ncol(z1):0, ...)
    image(x, y, rbind(z1, z2), ...)

    Doc$diptych <- '

        diptych is a wrapper for image: it displays image
        matrices side-by-side on the current device.

        Note the order of the first four parameters is different
        from image.'

image_setup <- function( #TAGS page setup
        mar=NULL,
        mai=NULL,
        x=par.usr.x(),
        y=par.usr.y(),
        bg=par("bg"),
        xaxs="i",
        yaxs="i",
        xaxt="n",
        yaxt="n",
        bty="n",
        main="",
        xlab="",
        ylab="",
        asp=NULL) {
    if (!is.null(mar)) par(mar=mar)
    if (!is.null(mai)) par(mai=mai)
    if (!is.null(bg )) par( bg= bg)
    image(
        x=x,
        y=y,
        z=M1,
        col=TRANSPARENT,
        xaxs=xaxs,
        yaxs=yaxs,
        xaxt=xaxt,
        yaxt=yaxt,
        bty=bty,
        main=main,
        xlab=xlab,
        ylab=ylab,
        useRaster=T,
        asp=asp,
        ...) }

    Doc$image_setup <- '
        image_setup is a "page setup" for the current device.
        Intended to create a new "page" on the current device,
        setting the margins and background color, if the
        corresponding arguments are specified.  It is intended
        that other aspects of the plot, even user coords, be by
        other processes.'
