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

    Doc$pg <- '
        pg is a prefix, infix, or suffix used in some names to
        indicate a polygon. Herein, a polygon is a matrix of
        vertices. If the polygon is in 2D space, it is
        compatible with the R graphics::polygon function.'

walk <- vapply_ %|% argswap %<=% seq0 #TAGS geometric vector

    Doc$walk <- '
        walk returns a (possibly higher dimensional) sequence
        from the zero vector to the "position" specified by the
        primary argument.
        
        Additional arguments are passed to seq.
        
        In most cases, the length.out named argument will also
        need to be specified, in order to ensure that
        intermediate results are of the same length.'


walk.pg <- function(pg, length.out=1L) #REF Wildberger (sp?) N
        do.call(rbind, lapply(
            pg %|% nrow %|% seq,
            function(i) rename.all.rows.m(
                m.add.m..v(
                    (rows %|% argswap %<=% -1)( #TODO why doesn't rest.m work?
                        walk(
                            rows %|% argswap %<=% i %-|% (
                                rbind(pg, rows %<=% pg %-|% 1) %|% diff),
                            length.out=length.out %|% succ %[mod% i)),
                    rows %<=% pg %-|% i %|% as.vector),
            (rep %<=% i)(length.out %|% succ %[mod% i - 1))))

    Doc$walk.pg <- '
        walk.polygon returns a walk around the given polygon
        (arg 1). The steps of the walk are uniform for a given
        polygon side.
    
        The length of the "legs" of the walk along each side of
        the polygon are given by the optional length.out
        argument. The default value of 1L results in a return
        that is equal to the primary argument.
    
        Elements of length.out are recycled if necessary to
        obtain one value per polygon side.'                                               
geom.Square.PROTO <- list(
    init=function (., side.length=1L) {
        .$.SIDE.LENGTH <- side.length
        . },
    as.pg=function(.) {
        as_xy(c(
                        0L,             0L,
            .$.SIDE.LENGTH,             0L,
            .$.SIDE.LENGTH, .$.SIDE.LENGTH,
                        0L, .$.SIDE.LENGTH))},
    .NULL=NULL)                                        

geom.Rectangle.PROTO <- list(
    init=function (.=geom.Rectangle.PROTO, dims=1L %|% rep2) {
        .$.WIDTH  <- dims[1]
        .$.HEIGHT <- dims[2]
        . },
    width =function(.) .$.WIDTH  %|% as.double,
    height=function(.) .$.HEIGHT %|% as.double,
    dims=function(.) .$.WIDTH %,% .$.HEIGHT %|% as.double,
    as.pg=function(.) {
        as_xy(c(
                  0L,        0L,
            .$.WIDTH,        0L,
            .$.WIDTH, .$.HEIGHT,
                  0L, .$.HEIGHT))},
    walk.uniform=function(., scale=1L)
            walk.pg(
	            . %|% .$as.pg,
                length.out=round(. %|% .$dims %|% rep2 * scale)),
    .NULL=NULL)                                        

        

