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
# SHOULD BE *text.R

text.axis.x <- function ( #TAGS plot
        x,
        y=y.center.line.number.below.x.axis(2),
        labels=rep_along("", x), ...)
    text(x=x, y=y, labels=labels, xpd=T, ...)

text.pos.h<- function(h) which1(h == words.h("below left above right"))

.ticks <- function(
        x=NULL,
        y=NULL,
        tick.length=par.usr.line.height(),
        lend="butt",
        col=par()$fg) {
    if (y %|% is.null) z <- xy.ticks.x(x, tick.length=tick.length)
    if (x %|% is.null) z <- xy.ticks.y(y, tick.length=tick.length)
	segments.xy(
		z,
		lend=lend,
        col=col,
		xpd=T)
}

ticks.x <- 1 %|% NULLs %=:% "y" %v% .ticks
ticks.y <- 1 %|% NULLs %=:% "x" %v% .ticks

pos <- function(pch=3) {
    warning("pos: status: In Progress (problem with plot positions [lils])")
    dev.new(width=4, height=4)
    par(mar=PAR.MAR.0)
    image(x=ol, y=ol, M1, col=TRANSPARENT)
    TEXT <- words.h("ONE TWO THREE FOUR")
    y <- .8
    x <- .5
    lapply(1:4, function(n) text(x, y, TEXT[n], pos=n))  % %  invisible
    points(x, y, pch=pch, cex=.5)
    lapply(1:4, function(n)   text(.2 * n, y - .4, TEXT[n], pos=n, srt=90))  % %  invisible
    lapply(1:4, function(n) points(.2 * n, y - .4, pch=pch))  % %  invisible
    text(mean(x), .2, "pos 1:4  and  srt 0, 90", pos=3)
    text(mean(x), 0, "see also text.pos.h", pos=3) }

ctext <- function (x, y, labels, line.height=par.usr.line.height(basis=""), ...)
        text(x, y - (labels %|% Nos. - 1) * line.height, labels, ...)

marginal.text.below <- function (x=0, y=0, ...)
        text(x=par.usr.x1() + x, y=par.usr.y1() - y, xpd=T, ...)

    Doc$marginal.text.below <- #TAGS plot
            "text.margin.below is designed to place text below
            the plotting area of a plot. Essentially the same as
            text, except that y is the distance below the x-axis
            (in usr coords) rather than the vertical distance,
            and x is the distance to the right of the
            left-hand-side of the plotting region."

marginal.text.above <- function (x=0, y=0, ...)
        text(x=par.usr.x1() + x, y=par.usr.y2() + y, xpd=T, ...)

marginal.text.left <- function (x=0, y=0, srt=90, ...)
        text(x=par.usr.x1() - x, y=par.usr.y1() + y, srt=srt, xpd=T, ...)

    Doc$marginal.text.left <- '
        marginal.text.left is similar to marginal.text.below.
        Text is rotated 90 degrees counter-clockwise by
        default.'

marginal.text.below.on.line.No <- function (x=0, y=1, ...)
        marginal.text.below(x=x, y=par.usr.line.height() * (y * 2 - 1)/2, ...)

    Doc$marginal.text.below.on.line.No <- #TAGS plot
            "marginal.text.below.on.line.No is designed to place
            text below the plotting area of a plot. Essentially
            the same as text, except that y is the number of
            lines below the x-axis (a y-value of 1 corresponds
            to the first line below the x-axis) rather than the
            vertical distance."

marginal.text.above.on.line.No <- function (x=0, y=1, ...)
        marginal.text.above(x=x, y=par.usr.line.height() * (y * 2 - 1)/2, ...)
            

marginal.text.left.on.line.No <- function (x=1, y=0, srt=90, ...)
        marginal.text.left(x=par.usr.line.height.y() * (x * 2 - 1)/2, y=y, srt=srt, ...)

    Doc$marginal.text.left.on.line.No <- '
        marginal.text.left.on.line.No is similar to
        marginal.text.below.on.line.No'

marginal.text.on.subtitle.line <- marginal.text.below.on.line.No %<=% 5
marginal.text.below.xlab <- marginal.text.on.subtitle.line
marginal.text.above.xlab <- marginal.text.below.on.line.No %<=% 3

plot_xlab <- function (xlab="", x=par.usr.x() %|% diff / 2)
        marginal.text.below.on.line.No(
            x=x, y=4, labels=xlab, col="col.lab" %|% par)
    
    Doc$plot_xlab <- '
        plot_xlab plots on the current device the text specified
        by xlab (arg1) in much the same manner as plot. No other
        changes are made to the existing plot.'

plot_ylab <- function (ylab="", y=par.usr.y() %|% diff / 2)
        marginal.text.left.on.line.No(
            x=4, y=y, labels=ylab, col="col.lab" %|% par)

'
function pos : text pos

        Creates a new graphics device and displays a diagram
        illistrating the effect of the text pos parameter.

function ctext : plot text columns

        Places text in a column. Arg 2 (y) is the y-positon of
        the first label (a line in a column). Additional labels
        are placed on their own lines, below. line.height may
        be specified.
'
