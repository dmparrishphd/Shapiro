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
linegraph <- function (X=NULL, Y=X,
        G=lapply(seq_along(Y), function (x) c(x,x)),
        col=Colors64,
        xlim=c(min(unlist(X), na.rm=T), max(unlist(X), na.rm=T)),
        ylim=c(min(unlist(Y), na.rm=T), max(unlist(Y), na.rm=T)),
        type=rep("l", times=min(1, length(Y))), xlab="", ylab="",
        new=F, ...)
{   if (is.null(X)) { linegraph(
            list((seq(17)-1)/16*2),
            c(lapply(seq(16), function (n) cos((seq(17)-n)/16*2*pi))),
            c(lapply(seq(16), function (x) c(1, x))),
            type="l", bg="black", col.axis="#C0C0C0")}
    for (i in seq_along(G)) {
        par(new=((1 < i) || new))
        plot(X[[G[[i]][1]]], Y[[G[[i]][2]]], xlim=xlim, ylim=ylim,
                xlab=xlab, ylab=ylab, type=type %[mod% i,
                col=col %[mod% i)}
    NULL }

linegraph <- function (X=NULL, Y=X,
        G=matrix(Y %|% Nos. %|% rep2, ncol=2), # lapply(seq_along(Y), function (x) c(x,x)),
        par.mar=PAR.MAR.C, par.new=F,
        uncol=BLU,
        col=Colors64,
        type=rep("l", times=min(1, length(Y))), xlab="", ylab="",
        xaxs="i", yaxs="i",
        col.axis=NULL, col.lab=NULL,
        col.main=NULL, col.sub=NULL, fg=NULL,
        xlim=c(X  % %  unlist  % %  rmna  % %  min,
               X  % %  unlist  % %  rmna  % %  max),
        ylim=c(Y  % %  unlist  % %  rmna  % %  min,
               Y  % %  unlist  % %  rmna  % %  max),
        FUN.plot=plot,
         ...) {
    if (is.null(X)) { linegraph(
            list((seq(17)-1)/16*2),
            c(lapply(seq(16), function (n) cos((seq(17)-n)/16*2*pi))),
            c(lapply(seq(16), function (x) c(1, x))),
            type="l", bg="black", col.axis="grey") }
    if(col.axis  % %  is.null) col.axis <- uncol
    if(col.lab   % %  is.null) col.lab  <- uncol
    if(col.main  % %  is.null) col.main <- uncol
    if(col.sub   % %  is.null) col.sub  <- uncol
    if(fg        % %  is.null) fg       <- uncol
    args.consistent <-
            list(...) %,% list(
                xlim=xlim, ylim=ylim, xaxs=xaxs, yaxs=yaxs,
                col.axis=col.axis, col.lab=col.lab,
                col.main=col.main, col.sub=col.sub, fg=fg)
    par(mar=par.mar)
    for (i in rowNos(G)) { # seq_along(G)) {
        args.i.pos <- list(X[[G[i, 1]]], Y[[G[i, 2]]])
        args.i.nom <- list(
            type=type %[mod% i, col=col %[mod% i,
            xlab=if (i < 2) xlab else HNULL,
            ylab=if (i < 2) ylab else HNULL)
        if (i < 2) par(new=par.new) else par(new=T)
        do.call(FUN.plot, c(args.i.pos, args.i.nom, args.consistent)) }
    invisible(NULL) }

'
function linegraph

                             linegraph

                     PLOT MULTIPLE LINE GRAPHS

    PARAMETERS

    X is a list of vectors. Each vector contains the x-values
    which may be plotted.
    
    Y is similar to X, but for y-values.
    
    G is a nx2 array-index of lines to plot. Column 1 indexes X
    and column 2 indexes Y.

    par.mar is the value for setting the mar parameter using the
    par function. See also documentation for PAR.MAR.C

    par.new is used to set the "new" parameter using par for the
    initial plot only. All successive plots have new=T---which
    is the essential feature of linegraph.

    uncol is the *default* color for every graph element
    *except* the data to be plotted.

    col is a vector of colors for each of the lines to be
    plotted. These colors are recycled if their number is fewer
    than the length of G.

    All remaining parameters, including ..., are passed to plot.
    The col and type parameters vary with each line plotted,
    while the other parameters are fixed for the entire graph.

    xaxs and yaxs.   The default values for xaxs and yaxs are
    different from those of the plot function.


    DETAILS

    DOT-DOT-DOT (...) is passed to the plot function, which will
    be executed with the same set of ... parameters for each
    line graphed. Before 2018-08-09, ... was passed to par.

    AXIS LABELS ARE OVERPRINTED for each line graph beyond the
    first. This is understood as a feature as opposed to a bug:
    if there are conflicting axis labels in the final plot, it
    is a sign that something has gone wrong.


    EXAMPLES
    
    DEFAULT PLOT. Call this function as linegraph() to see an
    example plot.

    TRIANGLE WAVES. This call plots a pair of triangle waves,
    where the y-values are reused:

        linegraph(
                X=list(      seq(8),   # first set of x-values
                        .5 + seq(8)),  # second set of x-values
                Y=list(rep(c(1,2),4)), # y-values for waves
                G=list( c(1,1),        # use X[[1]] and Y[[1]]
                        c(2,1)),       # use X[[2]] and Y[[1]]
                col=c("red", "blue"))  # optional color specs.


    POSSIBLE EXTENSIONS
    
    COMPUTED FUNCTIONS. One could write a wrapper function in
    which elements of Y are computed as functions of elements of X.
    

    HISTORY

    2018-09-14:
            Changed interpretation of G. Was list of index
            vectors before.
    2018-08-09:
            Enhanced flexibility for color specification.
            Minimized par parameters.
            Reworked logic.
            Changed behavior relative to the ... parameter.

    2018-02-07: clarified meaning documentation for X, Y, G.

    2018-02-19: dot-dot-dot parameters no longer used in par
                call: used for plot call instead.
                col specified in plot rather than in par call.

    2018-02-08: Spelling corrections.
                Embedded col default.
                Nested extract_modulo function and %[mod%
                        operator.
                Uploaded to SFWMD RUG SharePoint site as
                        linegraph.R.

    2018-02-07: Changed type of default graph to "l".
                Clarified documentation.
                Added Triangle Waves example.
                Changed default xlim and ylim calculations to
                        ignore NA values.

    2017-10-26: Uploaded to SFWMD RUG SharePoint site as
                linegraph.R.


    LICENCE

    Public Domain.

    Kindly acknowledge the author and SFWMD if you use this for
    published work.


    AUTHOR

    Developed by D. Michael Parrish at the South Florida Water
    Management District.

    ___________________________________________________________'
