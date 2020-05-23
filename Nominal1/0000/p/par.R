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



par.usr <- function() par()$usr
par.bg <- function () par()$bg

# CHARACTER SIZE
par.cex <- function () par()$cex

#
axis.h <- fn.table("BELOW LEFT ABOVE RIGHT" %|% words, 1:4) #TAGS
lend.h <- fn.table("ROUND BUTT SQUARE" %|% words, 0:2) #TAGS
ljoin.h <- fn.table("ROUND MITRE BEVEL" %|% words, 0:2) #TAGS
par.lwd <- function() par()$lwd

# GEOMETRY
par.din <- function() par()$din
par.usr.x  <- function () par.usr()[1:2]
par.usr.x1 <- function () par.usr.x()[1]
par.usr.x2 <- function () par.usr.x()[2]
par.usr.y  <- function () par.usr()[3:4]
par.usr.y1 <- function () par.usr.y()[1]
par.usr.y2 <- function () par.usr.y()[2]
xy.par.usr.center <- function () as_xy(mean(par.usr.x()) %,% mean(par.usr.y()))
par.usr.center <- xy.par.usr.center #DEPRECATED use xy.par.usr.center
par.usr.center.x <- xy.par.usr.center %O% first
par.usr.center.y <- xy.par.usr.center %O% second
par.usr.width  <- function () par()$usr[1:2] %|% diff
par.usr.height <- function () par()$usr[3:4] %|% diff

pin <- function () par("pin")
pin1 <- pin %O% first
pin2 <- pin %O% second

par.scale.x <- function(...) par.usr.width()  / pin1()
par.scale.y <- function(...) par.usr.height() / pin2()

asp <- function(...) par.scale.x() / par.scale.y()

par.usr.length <- function () c(par.usr.height(), par.usr.width()) %|% max
par.fin   <- function () par()$fin
par.fin.x <- par.fin %O% first
par.fin.y <- par.fin %O% second
par.fin.width  <- par.fin.x
par.fin.height <- par.fin.y
par.plt.height <- function () par()$plt[4] - par()$plt[3]
par.plt.width  <- function () par()$plt[2] - par()$plt[1]
par.usr.meta.height <- function () par.usr.height() / par.plt.height()
par.usr.meta.width  <- function () par.usr.width()  / par.plt.width()
par.usr.per.in.y <- function () par.usr.meta.height() / par("fin")[2]
par.usr.per.in.x <- function () par.usr.meta.width()  / par("fin")[1]
par.usr.per.in   <- function () par.usr.per.in.x() %,% par.usr.per.in.y()
par.usr.x.in <- function (xin) xin / par.usr.per.in.x()
par.usr.y.in <- function (yin) yin / par.usr.per.in.y()
par.mai <- function (i=NULL) if (i %|% is.null) par()$mai else par()$mai[i]
par.mar <- par %<=% "mar"
PAR.MAR.NAMES <- c("BOTTOM", "LEFT", "TOP", "RIGHT")
par.mar.NAMES <- PAR.MAR.NAMES #DEPRECATED USE PAR.MAR.NAMES
par.mar.named <- function() rename.all(par.mar(), PAR.MAR.NAMES)
xy.par.mar <- function () warning("PLACEHOLDER: function to return xy matrix of mar")
xy.par.mai <- function () warning("PLACEHOLDER: function to return xy matrix of mai")
par.mai.bottom <- par.mai %<=% 1
par.mai.left   <- par.mai %<=% 2
par.mai.top    <- par.mai %<=% 3
par.mai.right  <- par.mai %<=% 4
par.mai.longitudinal <- function () par.mai.bottom() %,% par.mai.top()
par.mai.lateral      <- function () par.mai.left()   %,% par.mai.right()
par.mai.sides <- par.mai.lateral
par.mai.longitudinal.sum <- par.mai.longitudinal %O% sum 
par.mai.lateral.sum      <- par.mai.lateral      %O% sum
par.mai.sum <- function () par.mai.lateral.sum() %,% par.mai.longitudinal.sum()
par.mai.sides <- par.mai.lateral
par.uin <- function () par.fin() - par.mai.sum()
par.uin.width  <- par.uin %O% first
par.uin.height <- par.uin %O% second
par.mar.bottom <- function () par.mar(1)
par.mar.left   <- function () par.mar(2)
par.mar.top    <- function () par.mar(3)
par.mar.right  <- function () par.mar(4)
par.mar.reset <- function () par(mar=c(5.1, 4.1, 4.1, 2.1))
par.mar.2 <- function () par(mar=c(2, 2, 2, 2))
par.mar.mutate <- function (
        bottom=NULL, left=NULL, top=NULL, right=NULL) {
    if (bottom %|% is.null) bottom <- par.mar.bottom()
    if (left %|% is.null) left <- par.mar.left()
    if (top %|% is.null) top <- par.mar.top()
    if (right %|% is.null) right <- par.mar.right()
    par(mar=c(bottom, left, top, right)) }

    Doc$par.mar.mutate <- '
        par.mar.mutate *MUTATES* the "mar" par-ameter by setting
        the elements specified by name. If a given argument is
        unspecified or NULL, the corresponding element is left
        unchanged.'
par.mai.mutate <- function (
        bottom=NULL, left=NULL, top=NULL, right=NULL) {
    if (bottom %|% is.null) bottom <- par.mai.bottom()
    if (left %|% is.null) left <- par.mai.left()
    if (top %|% is.null) top <- par.mai.top()
    if (right %|% is.null) right <- par.mai.right()
    par(mai=c(bottom, left, top, right)) }

    Doc$par.mai.mutate <- '
        par.mai.mutate *MUTATES* the "mai" par-ameter by setting
        the elements specified by name. If a given argument is
        unspecified or NULL, the corresponding element is left
        unchanged.'
H.PAR.MAI <- words.h("bottom left top right")
H.PAR.MAR <- H.PAR.MAI
par.mai.h <- function (h) par()$mai[which1(h == H.PAR.MAI)]
par.usr.meta.x1 <- function () par.usr.x1() - par.mai.left()   * par.usr.per.in.x()
par.usr.meta.y1 <- function () par.usr.y1() - par.mai.bottom() * par.usr.per.in.y()
par.usr.meta.x2 <- function () par.usr.x2() + par.mai.right()  * par.usr.per.in.x()
par.usr.meta.y2 <- function () par.usr.y2() + par.mai.top()    * par.usr.per.in.y()
par.usr.meta.x  <- function () par.usr.meta.x1() %,% par.usr.meta.x2()
par.usr.meta.y  <- function () par.usr.meta.y1() %,% par.usr.meta.y2()
xy.par.usr.meta <- function () matrix(par.usr.meta.x() %,% par.usr.meta.y(), ncol=2)
x.par.usr.meta <- xy.par.usr.meta %O% firstc
y.par.usr.meta <- xy.par.usr.meta %O% secondc

par.cex <- function () par()$cex

.par.usr.line.heights <- function (basis="mar")
        par.mai() / par.mar()

par.usr.line.heights <- function (basis="mar")
        .par.usr.line.heights() * par.usr.per.in.y()

par.usr.line.heights.y <- function (basis="mar")
        .par.usr.line.heights() * par.usr.per.in.x()

par.usr.line.height <- function (basis="mar")
        if (basis == "mar") { par.usr.line.heights() %|% finites %|% first
        } else { 1.81268882175226542 * strheight("A") }
        # constant 1.81... determined by test: PAR.USR.LINE.HEIGHT.FACTOR == lineheight() / strheight("A")

par.usr.line.height.y <- function (basis="mar")
        if (basis == "mar") { par.usr.line.heights.y() %|% finites %|% first
        } else {
            warning("Returning an untested value for line height.")
            1.81268882175226542 * strheight("A") #TODO TEST
            # constant 1.81... determined by test: PAR.USR.LINE.HEIGHT.FACTOR == lineheight() / strheight("A")
        }

par.mar.lpi <- function(precision=1024)
        (par.mar.named() / par.mai()) %|%
            (d.trunc %|% argswap %<=% precision)

    Doc$par.mar.named <- "par.mar.named returns the margins (in
        line units) with names that indicate which is which."

x.linesv <- function (x)
        rbind(x, x, rep(NA, x %|% `#`)) %|% as.vector

y.linesh <- x.linesv

y.linesv <- function (x)
        rep(par.usr.y() %,% NA, x %|% `#`)



    Doc$pin <- '
        pin returns the dimensions of the current plot. Alias for par("pin").
    
        pin1 returns the x-dimension of the current plot.
        
        pin2 returns the y-dimension sof the current plot.'

    Doc$pin1 <- Doc$pin

    Doc$pin2 <- Doc$pin 


    Doc$asp <- '
        asp returns the aspect ratio of the current plot.'

    Doc$par.mar.2 <- '
        par.mar.2 sets the mar of the current device to 2.  This
        setting is sufficient to include the rows of axis labels
        in an otherwise default plot.'

'
function pairs.xy : lines segments

        Transforms an xy matrix into a form more suitable for
        plotting segments as alternating sets of points, e.g.:

                1,0,  2,0,  3,0  4,0               would plot as

                (1)==(2)  (3)==(4)                rather than as 
                
                (1)==(2)==(3)==(4)

                that is, without a line drawn between points 2
                and 3.
'


'
function par.*

        Function family related to par. See also help(par).
 
function par.usr.length

        The greater of the height or width of the user
        coordinates of the plotting region.

function par.usr.height

        The height of the user coordinates of the plotting
        region.

function par.usr.width

        The width of the user coordinates of the plotting
        region.

function par.usr.meta.height
function par.usr.meta.width

        Returns the full width (height) of the plot, including
        margins.  Reminder: Gr. "meta" == Eng. "beyond"

par.usr.meta.x
par.usr.meta.x1
par.usr.meta.x2
par.usr.meta.y
par.usr.meta.y1
par.usr.meta.y2
xy.par.usr.meta

        Returns the meta user coords (i.e., "beyond" the plot) of
        the extremes of the plot (i.e., at the edge of any margin).

function par.usr.per.in.x
function par.usr.per.in.y

       Returns the ratio of usr coords vs. inches in the
       respective direction. 

function par.usr.line.height

        Returns the line height on the basis indicated, in user
        coordinates.  Currently (2018-10-18), only "mar"
        (margins) is supported.
'
