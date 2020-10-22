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



dots <- function(x, ..., pch=0)
        points(x, ..., pch=i.h("16 16 16 17 15")[pch %|% succ %[mod% 5])

    Doc$dots <- '
        dots has the same behaviour as points, EXCEPT that pch
        values are interpreted as mod 5 integers which are then
        mapped to actual pch values as 0 ==> 16, 1 ==> 16, 2 ==>
        16, 3 ==> 17, 4 ==> 15; so that dot-s pch argument
        corresponds to the number of sides on the plotting
        character (circles are considered to have zero sides).'

multipoints <- function(X, ...) {
    default <- fndefault(0)
    lfn <- lapply(X, function (x) points %^% c(x$par, ...))
    for (i in seq_along(X)) lfn[[i]](
            X[[i]]$x - default(X[[i]]$shiftx),
            X[[i]]$y - default(X[[i]]$shifty)) }

'
function multipoints

        Plot multiple sets of points---adding points to an
        existing plot. Argument 1 is a compound list, each
        element of which is a list containing members x and y.
        These are interpreted in much the same way as a list of
        x and y in the points function. Optional members include
        par, which are effectively passed to the call of points
        that is specific to that set of x-y parallel vectors;
        and shiftx and shifty, which are applied to shift the
        corresponding point sets in the -x- and -y-directions
        (i.e., to the left and down). Finally, the dot-dot-dot
        argument is passed to every call of points.  Thus the
        dot-dot-dot argument represents parameters that all of
        the points share. Example:

                > image(matrix(NA), xlim=c(0,10), ylim=c(0,10))
                multipoints(list(
                +       list( x=1, y=1, par=list(col="red") ),
                +       list( x=9, y=1, par=list(col="blue")),
                +       list( x=5, y=8, par=list(col="green"),
                +               shifty=0.071796769724490825)),
                +       pch=19)
                # Should draw the vertices of an equilateral
                # triangle, each in its own color.

        On the name multipoints, see
        https://en.wikipedia.org/wiki/Simple_Features and
        http://www.opengeospatial.org/docs/is
'
