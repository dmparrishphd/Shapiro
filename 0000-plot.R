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
    par(mai=PAR.MAR.0)
    dev.new(width=4, height=4)
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

pch.h <- function(description)
        (default %|% argswap %<=% 1L)(
            (match %<=% description)(
                "CIRCLE"%,% #1
                "TRIANGLE UP"%,% #2
                "CROSS"%,% #3
                "X"%,% #4
                "DIAMOND"%,% #5
                "TRIANGLE DOWN"%,% #6
                "X SQUARE"%,% #7
                "CROSS X"%,% #8
                "CROSS DIAMOND"%,% #9
                "CIRCLE CROSS"%,% #10
                "TRIANGLE TRIANGLE"%,% #11
                "CROSS SQUARE"%,% #12
                "CIRCLE X"%,% #13
                "TRIANGLE SQUARE"%,% #14
                "SQUARE SOLID"%,% #15
                "CIRCLE SOLID"%,% #16
                "TRIANGLE SOLID"%,% #17
                "DIAMOND SOLID"%,% #18
                "CIRCLE BORDER"%,% #19
                "CIRCLE SOLID SMALL"%,% #20
                "CIRCLE FILLED"%,% #21
                "SQUARE FILLED"%,% #22
                "DIAMOND FILLED"%,% #23
                "TRIANGLE UP FILLED"%,% #24
                "TRIANGLE DOWN FILLED"))
        
.lines_grid.x <- function (spacing=NULL, offset=0, ...) {   
    # TO DO: refactor using linesv
    nlin <- ceiling(par.usr.width() / spacing)
    yy <- rep(c(par.usr.y1(), par.usr.y2(), NA), nlin)
    xx <- (as.vector(matrix(
            c(rep(seq(nlin), 2), rep(NA, nlin)), nrow=3, byrow=T)) - 1) *
            spacing + par.usr.x1() + offset
    lines(xx, yy, ...) }

.lines_grid.y <- function (spacing=NULL, offset=0, ...) {   
    # TO DO: refactor using linesh
    nlin <- ceiling(par.usr.height() / spacing)
    xx <- rep(c(par.usr.x1(), par.usr.x2(), NA), nlin)
    yy <- (as.vector(matrix(
            c(rep(seq(nlin), 2), rep(NA, nlin)), nrow=3, byrow=T)) - 1) *
            spacing + par.usr.y1() + offset
    lines(xx, yy, ...) }

lines_grid <- function (spacex, spacey=spacex, offsetx=0, offsety=0, ...) {
    .lines_grid.x(spacex, offsetx, ...)
    .lines_grid.y(spacey, offsety, ...) }

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
