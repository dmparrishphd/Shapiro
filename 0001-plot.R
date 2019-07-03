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

rect. <- rect %^% list(lend="SQUARE" %|% lend.h, ljoin="MITRE" %|% ljoin.h)

    Doc$rect. <- '
        rect. is a version of rect with lend set to SQURE and
        ljoin set to MITRE. Produces a rectangular frame that is
        noticibly different with larger lwd values.'

rect.exo <- function(xleft, ybottom, xright, ytop, lwd.in, ...) {
	LWD2 <- lwd.in / 2 * par.usr.per.in()
	rect.(
		xleft   - LWD2[1],
		ybottom - LWD2[2],
		xright  + LWD2[1],
		ytop    + LWD2[2], ...) }

    Doc$rect.exo <- '
        rect.exo draws a rectangular frame OUTSIDE of the
        rectangle speficied by args 1 to 4 (same order as rect),
        **** PROVIDED THAT **** the lwd.in parameter (line width
        in inches) is specified in a manner consistent with
        par("lwd") and the current device; the inner edge of the
        frame will be coincident with the aforementioned
        rectangle.'

box.exo <- function (lwd.in, which="plot", lty="solid", col=NA, fg=NA, ...)
	rect.exo(
		par.usr.x1(), par.usr.y1(), par.usr.x2(), par.usr.y2(),
		lwd.in, xpd=T, border=if (col %|% is.na %|% `!`) col else if (fg %|% is.na %|% `!`) fg else par("col"), ...)

    Doc$box.exo <- '
        box.exo is much the same as box, except that the drawn
        box does not obscure the user space, **** PROVIDED THAT
        **** the lwd.in parameter (line width in inches) is
        specified in a manner consistent with par("lwd") and the
        current device. Color specification follows that of box.'

        

        


clip_sift.par.usr.x <- function (x, include.min=T, include.max=T)
        clip_sift(
            x,
            min_=par.usr.x1(), max_=par.usr.x2(),
            include.min=include.min, include.max=include.max)

y.center.line.number <- function (n=1) {
        n <- floor(n)
        if (!n) return (NULL)
        (par.usr.meta.y2 %,% nop %,% par.usr.meta.y1)[[n.index.sign(n)]]() +
                (n * 2 - sign(n)) * par.usr.line.height()/2 }

        Doc$y.line.center <-
                "Returns the nominal center of the line number
                given. The current par-s are assumed."

y.center.line.number.below.x.axis <- function (n=1)
        par.usr.y1() - (n * 2 - 1) * par.usr.line.height()/2

'
y.center.line.number.below.x.axis(1)
text(
    x=0.6,
    y=y.center.line.number.below.x.axis(2),
    labels="0.6",
    xpd=T)
'

legend.item <- function ( #TAGS plot margin
        x=par.usr.meta.x1(),
        y=y.center.line.number(),
        line.number=1,
        pch=1,
        symbol_width="M" %|% strheight * par.usr.per.in() %|% quotient,
        label="",
        lines_args =list(),
        points_args=list(),
        text_args  =list()
        ) {
    lines.  <- T %=:% "xpd" %v% lines  %=>% do.call
    points. <- T %=:% "xpd" %v% points %=>% do.call
    text.   <- T %=:% "xpd" %v% text   %=>% do.call
    if (line.number %|% is.null %|% `!`) y <-
            y.center.line.number(line.number)
    lines.(lines_args %,% list(x=symbol_width * ol + x, y=y %|% rep2))
    points.(points_args %,% list(pch=pch, x=symbol_width/2 + x, y=y))
    text.(text_args %,% list(
        x=symbol_width + x, y=y, pos="right" %|% text.pos.h, labels=label)) }

    Doc$legend.item <- 
            "~plot-s~ a legend item on the current device at the
            x, y coords specified. Any non-NULL line.number
            argument will override any y-value specified. The
            pch argument is passed to points.  The horizontal
            space allocated to the combindation line and point
            symbol is determined by the symbol width argument.
            The label argument is passed to text as labels.
            Additional arguments specific to lines, points, and
            text may be specified in the corresponding list
            arguments."


xy.ticks.x <- function (x, y0=par.usr.y1(), tick.length=par.usr.line.height()) {
    xx <- m.rep(x, 2) %|% t %|% as.vector
    yy <- c(y0, y0 - tick.length)
    cbind(xx, yy) }

    Doc$xy.ticks.x <-
        "Given x-coords of hypothetical x-axis tick marks,
        returns an xy matrix that might be used to plot those
        ticks."

xy.ticks.y <- function (y, x0=par.usr.x1(), tick.length=par.usr.line.height.y()) {
    warning("
        xy.ticks.y is not fully developed. probably need to
        reconsider calculation of default tick length (line
        height is probably different in the horizontal
        direction).")
    xy.ticks.x(y, y0=x0, tick.length=tick.length) %|% cswap }

lineh <- function (y, col=BLK, ...)
	    lines(par.usr.x(), rep(y, 2), col=col, ...)

lineh.under <- function (y, col=BLK, lwd=par.lwd()) {
        warning("Need to fix computation of line width in terms of user coords.")
        lineh(y - lwd/2, col=col, lwd=lwd)
}

lineh.over <- placeholder

linev.left <- placeholder

linev.right <- placeholder

line45 <- function (...) lines(
    par.usr() %|% abs %|% max %|% rep4 %|% as_xy %m*v% .ll %|% t, ...)

    Doc$line45 <- '
        line45 draws a 1:1 line on the current device. The ...
        argument may be used to pass any argument **** EXCEPT
        **** x or y to lines.'

xy.linesv <- function (x)
        cbind(x.linesv(x), par.usr.y() %,% NA)

linev <- function (x, col=BLK, ...)
	    lines(rep(x, 2), par.usr.y(), col=col, ...)

linesh <- function(y, col=BLK, ...) for (wye in y) lineh(wye, col=col, ...)
linesv <- function(x, col=BLK, ...) for ( ex in x) linev( ex, col=col, ...)

#linesh.in <- function (y, ...) {
    #linesh(y * par.usr.per.in.y() + )
     #}

cross <- function(x, y, col=BLK, ...) {
    lineh(y, col=col, ...);   linev(x, col=col, ...) }

cross.xy <- function(xy, ...) cross(xy[1], xy[2], ...)

doublecross <- function (x, y, col=BLK, ...) {
    cross(x, y, col=col, ...);   cross(x - 1L, y - 1L, col=col, ...) }

        Shapir$Doc$lineh <- "
                Draw a horizontal line across the entire plotting
                region (par()$usr), through y (arg 1). The
                dot-dot-dot parameters are passed to lines."

        Shapir$Doc$linev <- "
                Draw a vertical line across the entire plotting
                region (par()$usr), through x (arg 1). The
                dot-dot-dot parameters are passed to lines."

        Shapir$Doc$cross <- "
                Draw a crosshair across the entire plotting
                region (par()$usr), vertically through x (arg 1)
                and horizintally through y (arg 2). The
                dot-dot-dot parameters are passed to lines."

        Shapir$Doc$doublecross <- "
                Intended for drawing along the edges of raster
                cells. Draw two crosshairs across the entire
                plotting region (par()$usr), vertically through
                x (arg 1) and x - 1 and horizintally through y
                (arg 2) and y - 1. The dot-dot-dot parameters
                are passed to lines."

'
function linev : vertical line
function lineh : horizontal line
function linesv : vertical lines
function linesh : horizontal lines

function legend.item

        EXAMPLE

        par(bg=BLK, fg=DGR, col.axis=DGR, col.lab=DGR, col.main=DGR, col.sub=DGR)
        par(mar=PAR.MAR.C)
        plot(1)
        y0 <- y.center.line.number(1)
        dy <- par.usr.line.height()
        x <- par.usr.x1()
        legend.item(x=x, line.number=1, label="1: Labelqypg",
                    lines_args=list(col=CYN), points_args=list(col=YEL))
        legend.item(x=x, line.number=2, label="2: Label")
        legend.item(x=x, line.number=3, label="3: Label")
        legend.item(x=x, line.number=4, label="4: Label")
        legend.item(x=x, line.number=5, label="5: Label")
        legend.item(x=x, line.number=6, label="6: Labelqypg")

'
