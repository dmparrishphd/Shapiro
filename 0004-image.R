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
### R script

#TODO: MAKE alternates FUNCTION that returns a version of a function with different defaults
image_bare <- function(..., bty="n", xaxt="n", yaxt="n", xlab="", ylab="")
        image(..., bty=bty, xaxt=xaxt, yaxt=yaxt, xlab=xlab, ylab=ylab)

    Doc$image_bare <- '
        image_bare applies image with no box (bty="n"), no axes
        (xaxt="n", yaxt="n"), and no axis titles (xlab="",
        ylab=""); i.e., displays the only the image itself.'

'
image_baRe <- function(..., useRaster=T)
'

im.chess.board <- function ()
        matrix(rep(rep(T %,% F, 4) %,% rep(F %,% T, 4), 4), ncol=8)

centroids.ij <- function (ij) ij - .5

width.im <- function(m) nrow(m)

height.im <- function(m) ncol(m)

aspect.im <- function (m) n.row.m(m) / n.col.m(m)

hw.im <- function (m, w=NULL, h=NULL)
    if (w  % %  is.null)
        if (h  % %  is.null) dim(m) else
        c(aspect.im(m)*h, h) else
    c(w, w/aspect.im(m))

img.m <- function (m) m  % %  t  % %  turn.m
im.m <- img.m # function DEPRECATED. use img.m

img.v = function(z, ...) matrix(z, byrow=T, ...)  % %  img.m
im.v <- img.v # function DEPRECATED. use img.v

sprite.h <- function (h, FUN=NULL) img.v(
    h  % %  dec.sift.h  % %  characters  % %  as.integer,
    h  % %  nlines)

im. <- function() "
        011111000
        110001100
        110000011
        110000000
        110000022
        110001100
        011111000"  % %  sprite.h

icolors.im <- function (m) m  % %  as.vector  % %  unique

n.colors.im <- function (m) m  % %  icolors.im  % %  `#`

monochrome.colors <- function (col=BLK) c(TRANSPARENT, col[1])

monochrome.im <- function (m)
{   0L -> m[m  % %  is.na];   1L -> m[!!m];   m }

image_par.x.reg <- function (m) c(0, rowNos.m(m))

image_par.y.reg <- function (m) c(0, colNos.m(m))

####    PLOTTING IMAGES

image_im <- function (z, x=NULL, y=NULL, xlim=NULL, ylim=NULL,
        col=colors64, add=T, xaxs="i", yaxs="i", xlab="",
        ylab="", useRaster=F, bty="n", xaxt="n", yaxt="n", ...) {
    if (x     % %  is.null) image_par.x.reg(z) -> x
    if (y     % %  is.null) image_par.y.reg(z) -> y
    if (xlim  % %  is.null) range(x)           -> xlim
    if (ylim  % %  is.null) range(y)           -> ylim
    0  %,%      length(col) - .5               -> zlim
    image(x, y, z, zlim, xlim, ylim, col=col, add=add, xaxs=xaxs, yaxs=yaxs,
            xlab=xlab, ylab=ylab, useRaster=useRaster, bty=bty,
            xaxt=xaxt, yaxt=yaxt,...) }

swatch <- function (xlim, ylim, col=grey(.5), ...)
    image(xlim, ylim, MAT1, xlim, ylim, col=col, add=T, breaks=c(0, 2), ...)

swatch1 <- function (x, y, col=grey(.5), ...) {
    xlim <- interval(x)
    ylim <- interval(y)
    swatch(xlim, ylim, col=col, ...) }

stamp <- function (z, w=NULL, h=NULL, xmax=NULL, ymax=NULL, col=colors64, ...) {
    n.col.m(z) -> nc
    n.row.m(z) -> nr
    if (h   % %  is.null) nc -> h
    if (w   % %  is.null) nr -> w
    if (xmax  % %  is.null) par.usr.x2() -> xmax
    if (ymax  % %  is.null) par.usr.y2() -> ymax
    0:nc * h / nc + ymax - h -> y
    0:nr * w / nr + xmax - w -> x
    image_im(z, x, y, col=col, ...) }

wipe <- function () stamp(
        M1, w=par.usr.width(), h=par.usr.height(),
        col=monochrome.colors(par()$bg))

canvas.im <- function (z, ...)
        image_im(matrix(NA), x=c(0, n.row.m(z)), y=c(0, n.col.m(z)),
                add=F, ...)

canvas.xy <- function (xmax=1, ymax=1, ...)
        image_im(matrix(NA), x=c(0, xmax), y=c(0, ymax), add=F, ...)
'
function canvas.im
function canvas.xy

        Prepare an image plot.

        The canvas.xy function takes the width and height of the
        desired plotting area. A blank plot is produced. The
        dot-dot-dot parameters are passed to image.

        The canvas.im function has the same effect as canvas.xy.
        Arg 1 is a matrix; the corresponding plot extents are
        taken from the dimensions of the The primary arguments is takes an "im" matrix argument.
'

.im.axes <- function (to1, to2, by)
{   par(xaxt="s", yaxt="s")
    axis(2, at=seq(from=0,to=to2,by=by) ,labels=seq(from=0,to=to2,by=by))
    axis(1, at=seq(from=0,to=to1,by=by) ,labels=seq(from=0,to=to1,by=by)) }

by.d <- function (d, bys=c(1L, 2L, 5L), base=10L, toomany=17L)
{   1L -> p
    0L -> j 
    F -> flag
    repeat {
        for (i in Nos.(bys)) {
            if ((d %/% bys[i]) < toomany) {
                T -> flag
                break }}
        if (flag) break
        base ** p * bys -> bys 
        succ(j) -> j }
    bys[i] }

by.m <- function (im, bys=c(1L, 2L, 5L), base=10L, toomany=17L)
{   im  % %  dim  % %  max -> siz # larger dimension
    by.d(siz, bys=bys, base=base, toomany=toomany) }

.im.axes.img <- function (toomany=17L, bys=c(1L, 2L, 5L), base=10L)
{   #by = by.m(, toomany=toomany, bys=bys, base=base)
    # want reasonable no. of ticks along short side
    .im.axes(par.usr.width(), par.usr.width(), by=by.d(par.usr.length(),
                                               bys=bys,
                                               base=base,
                                               toomany=toomany)) }

im.swatches = function (nc, tw=256, nr=64) {
    # tw = total width of swatches
    # nr = height of swatches in number of rows
    rr=553;   cc=512   # nc=number of colors
    m=matrix(rep(0, rr*cc), ncol=512)
    m[(1+cc*(rr-nr)):(rr*cc)] = rep(   c( (seq(tw)-1)%/%(tw/nc), rep(0 ,cc-tw) ), nr   )
    im.v(m, ncol=cc) }

dot <- function(x, y, dx=1, dy=1, col=BLK, add=T, ...) image_bare(
    x=c(x - dx, x),
    y=c(y - dy, y),
    z=M1,
    xlim=par.usr.x(),
    ylim=par.usr.y(),
    col=col,
    add=add,
    xlab="",
    ylab="",
    useRaster=T,
    ...)

blank.slate <- function (col=par.bg()) #TAGS plot
        dot(
            x=par.usr.meta.x2(),
            y=par.usr.meta.y2(),
            dx=par.usr.meta.width(),
            dy=par.usr.meta.height(),
            col=col, # using TRANSPARENT leaves a white border at top and left. Why?
            add=T,
            xpd=T)

    #+doc$blank.slate <- '
        #+Covers the current device with the background color
        #+(default) or with the color specified.'

dev_reset.no.margin <- function ()
    par(mar=PAR.MAR.0)

dev_reset.as.pixmap <- function (xdpi=96, ydpi=xdpi) {
    dev_reset.no.margin()
    par(usr=xdpi %,% ydpi * rbind(0, par()$din))
    blank.slate() }

blank.page <- function (bg=WHT) {
    par(mar=PAR.MAR.0, bg=bg)
    blank.slate() }



# SAVING IMAGES

.write.im = function (v, file) { write(v, file=file, ncolumns=10, append=T) }

write.im = function (m, file) {
    for (i in rev(colNos.m(m))) .write.im(m[,i], file=file) }

'
function im.

        Returns a sample indexed color image matrix. Takes no
        arguments.

function dot : image plot

        Plots a rectangular dot on an existing device at
        location (x, y) (arg 1 and arg 2).

        dx, dy. The dot is one unit wide and one unit tall by
        default; these dimensions are controlled by the optional
        parameters dx and dy. The dot extends from x - dx to x
        along the x-axis, and from y - dy to y along the y-axis.

        col. Dot color is specified by the optional col argument.

        add. This parameter is not intended to be specified in
        the typical case. It is used by blank.slate.

        HISTORY 2018-09-06 completely revised.

function blank.slate : image


        HISTORY
                2018-10-23 formerly started a new plot/page.
                2018-10-22 added optional col argument.
                2018-09-06 completely revised.

function dev_reset.as.pixmap : image plot bitmap screen terminal

        Alters the margins and user coordinates (par usr) of the
        current device so that each pixel on the device has
        dimensions of 1 x 1 and the bottom-left corner
        corresponds with (0, 0).

        Originally intended for on-screen graphics devices.

        Can be used to simulate a terminal screen, etc.


function by.d

        Returns a vlaue suitable for use as a "by" parameter in
        graphing tick marks along axes.

function im.m : format matrix image plot

        Given a matrix argument, returns another matrix suitable
        for plotting with image. 

function im.v : format matrix image plot

        Receives a vector z in row-major order ("English"
        order); returns a matrix suitable for plotting with
        image
        
        usage:
        
                im.v(z, nrow=number.of.rows.in.image) 
                
                or
                
                im.v(z, ncol=number.of.cols.in.image)

        Warning:

                Expect strange results if nrow or ncol is unspecified

        To "image" a returned matrix, m:

                image(m % % rowNos.m, m % % colNos.m, m)

        Example:

                mm <- m.im.sample()
                m <- im.v(as.vector(mm), nrow=n.col.m(mm))
                # The nrow=...col has to do with the pecuiliarty
                # of storage order.
                image(m % % rowNos.m, m % % colNos.m, m,
                        col="white blue red"  % %  h.words.h)

        Example:

                > m.()
                     [,1] [,2] [,3]
                [1,]    1    3    5
                [2,]    2    4    6
                > im.v(m.(), nrow=2)
                     [,1] [,2]
                [1,]    4    1
                [2,]    5    2
                [3,]    6    3

function n.colors.im : indexed color image palette

        Returns the number of colors represented in the indexed
        color image matrix ("im").

function icolors.im : indexed color image palette

        Returns the indices of the colors represented in the
        indexed color image matrix ("im").

function monochrome.colors

        Given a color (arg 1), returns an object suitable for
        the col parameter of the image function. Intended for
        use with image_im, which maps 0 to the first color, 1 to
        the second, and so forth. Color 0 is transparent, color
        1 is an arbitrary color.

function monochrome.im

        Given a matrix, returns a modified copy where all NA
        values are set to zero and all nonzero values are set to
        1L.  Intended for use with indexed color image matrices.

function wipe

        Clears the current graphics plot box ("box" in the sense
        of the bty parameter of the par function).

function aspect : ratio dimensions rectangle geometry

        Returns the aspect ratio of an indexed color image
        matrix, width / height, assuming pixels are square.

function hw.im : dimensions geometry

        Returns the width and height of an indexed color image
        matrix (arg 1), preserving aspect ratio. If neither the
        height and width are specified, the elements of the
        returned value are the number of rows and columns of the
        argument. If both the width and height are specified,
        the height argument is ignored.

function stamp : image overlay

        Adds content to an existing graphics device: draws the
        indexed color image matrix at the specified width and
        height, and at the position specified by xmax and ymax
        (upper-right corner).

function centroids.ij

        Given a 2-column matrix of (i, j) coordinates (compatible
        with plot) that indicate the column and row numbers of
        cells of an image matrix (i.e., c(1, 1) indicates bottom
        left corner of the image and c(2, 1) indicates the next
        image cell to the right), returns a matrix of centroids
        of the corresponding cells.

        The following example plots a chess board with white
dots on the dark squares.

    > image_im(im.chess.board(), col=c("white", "brown"), add=F)
    > points(im.chess.board() % % ij.im % % centroids.ij, pch=20, cex=5, col="white")
'
