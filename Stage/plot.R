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

'""
xy #SEE ALSO: Doc$xy #TAGS plot points
""'

    Doc$xy <- '
        xy is a prefix, infix, or suffix used in some names to
        indicate an n x 2 numeric matrix. An xy object is
        compatible with the R graphics::points function.'

spiral <- function(n=100, s=1/2, w=sqrt(.5 + .5 * sqrt(5))) {
	t <- n %|% seq %|% pred %|% sqrt * s
	a <- 2 * pi * w * t
	t * cbind(a %|% cos, a %|% sin) }

    Doc$spiral <- '
        spiral returns a two-column matrix of coords suitable
        for plotting an approximate spiral.  The default
        parameters were selected emperically by visual
        inspections so that the points are nearly equidistant in
        every direction as one moves further out on the spiral.'

PAR.MAR.TIGHT <- c(5, 4, 2, 0) # par mar
PAR.MAR.0 <- rep(0, 4) # par mar
PAR.MAR.C <-c(5, 4, 2, 6/10)

plot_blank <- function(xlim, ylim, xaxt="n", yaxt="n", bty="n", ...)
        image(M1, zlim=ol, xlim=xlim, ylim=ylim, xaxt=xaxt,
              yaxt=yaxt, bty=bty, col=TRANSPARENT, useRaster=T, ...)

    Doc$plot_blank <- '
        plot_blank creates a new plot on the current device
        using the image function. Originally intended to start a
        new plot with specified x- and y-axis limits.'

par.col.dot <- function (col) par(
    col.axis=col,
    col.lab =col,
    col.main=col,
    col.sub =col)


pch <- function(npch=25, bg=grey(.5)) {
    qpch <- ceiling(sqrt(npch)) # next nearest perfect square
    shift <- ceiling(qpch / 2)
    inner.limits <- c(-1, 1) * shift # nominal range of plotted data
    outer.limits <- 1.5 * inner.limits # range of plotting area
    middle.limits <- vapply( # mean of inner- and outer.limits
            1:2, function(i, m) mean(m[,i]), double(1), matrix(
                    c(inner.limits, outer.limits), ncol=2, byrow=T))
    dev.new(width=4, height=4)
    par(mar=rep(1, 4))
    image(matrix(), xlim=outer.limits, ylim=outer.limits,
            xaxt="n", yaxt="n", asp=1, bty="n") # a blank plot
    x <- rep(1:qpch, qpch) - shift - ((1 + qpch) %% 2)/2
            # x-coords for plotting symbols
    y <- rev(sort(x)) # y-coords for plotting symbolic
    offset <- 1/6 # relatively less space with increasing qpch
    CEX <- 2*3/qpch
    points(x - offset, y, pch=1:npch, cex=CEX, bg=bg)
            # draw every valid pch
    text  (x + offset, y, as.character(1:npch), cex=CEX)
            # label each pch with its number
    rect(   middle.limits[1], middle.limits[1],
            middle.limits[2], middle.limits[2]) # an aesthetic box
    text(0, mean(c(middle.limits[1], outer.limits[1])),
            labels="R Plotting Characters (pch)") }

seq_par.usr.x <- function (by=1) {
     }

.lines_grid.x <- function (spacing=NULL, start="left", ...)
        linesv(
            (if (start == "left") {
                par.usr.x1()
            } else if (start == "integer") {
                par.usr.x1() %|% ceiling
            } else start) +
           par.usr.width() %\% spacing %|% seq %|% pred * spacing, ...)

.lines_grid.y <- function (spacing=NULL, start="bottom", ...)
        linesh(
            (if (start == "bottom") {
                par.usr.y1()
            } else if (start == "integer") {
                par.usr.y1() %|% ceiling
            } else start) +
           par.usr.height() %\% spacing %|% seq %|% pred * spacing, ...)

lines_grid <- function (spacex=1, spacey=spacex, startx="left", starty="bottom", ...) {
    .lines_grid.x(spacex, startx, ...)
    .lines_grid.y(spacey, starty, ...) }

'
function pch : plotting characters points symbol marker

        Creates a plot that displays the first npch (arg 1)
        plotting characters. If npch is not a perfect square,
        additional points will be added to the plot until the
        next perfect square number of points is reached. The bg
        parameter (arg 2) operates as in the graphics::points
        function.

value PAR.MAR.TIGHT : plot par margins

        A value that may be assigned to the mar parameter:

                par(mar=PAR.MAR.TIGHT)

        Provides just enough space for title, axis titles, and
        subtitle.

function par.col.dot : plot color fg foreground

        sets the col.axis, col.lab,  col.main, and col.sub
        colors of the current device to the value
        specified by the argument.

value PAR.MAR.C : par mar margin

        This value of mar (an argument of par) allows enough
        space for a subtitle, y-axis title, and a main title.
        The default value for "right" (6/10) (6 lines / 10) is
        nominally one [c]haracter width, hence the "C" suffix.

'
