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

Ninepin <- list(
    .Width=8L,
    .Height=8L,
    .BG=TRANSPARENT, # default symbol negative space (background) color
    .FG=WHT, # default symbol positive space (foreground) color
    NULL
)

Ninepin$dots2symbol <- words.h %O% `%//%` %O% characters %O% unlist %O% (`!=` %<=% ".") %O% (im.v %^% list(ncol=Ninepin$.Width))

Ninepin$.SymbolData$Space <- '
........
........
........
........
........
........
........
........'

Ninepin$.SymbolData$`0` <- '
...888..
..8...8.
.8.....8
.8..8..8
.8.....8
..8...8.
...888..
........'

Ninepin$.SymbolData$`8` <- '
..8888..
.8....8.
.8....8.
..8888..
.8....8.
.8....8.
..8888..
........'

Ninepin$.SymbolData$A <- '
.......8
......88
.....8.8
....8..8
...88888
..8....8
.8.....8
........'

Ninepin$.SymbolData$B <- '
.88888..
.8....8.
.8....8.
.88888..
.8....8.
.8....8.
.88888..
........'

Ninepin$.SymbolData$C <- '
..8888..
.8....8.
.8......
.8......
.8......
.8....8.
..8888..
........'

Ninepin$.SymbolData$D <- '
888888..
.8....8.
.8....8.
.8....8.
.8....8.
.8....8.
888888..
........'

Ninepin$.SymbolData$E <- '
.888888.
.8......
.8......
.888888.
.8......
.8......
.888888.
........'

Ninepin$.SymbolData$F <- '
.888888.
.8......
.8......
.888888.
.8......
.8......
.8......
........'

Ninepin$.SymbolData$G <- '
..8888..
.8....8.
.8......
.8..888.
.8....8.
.8....8.
..8888..
........'

Ninepin$.SymbolData$H <- '
.8....8.
.8....8.
.8....8.
.888888.
.8....8.
.8....8.
.8....8.
........'

Ninepin$.SymbolData$I <- '
.88888..
...8....
...8....
...8....
...8....
...8....
.88888..
........'

Ninepin$.SymbolData$J <- '
.88888..
...8....
...8....
...8....
.8.8....
.8.8....
.888....
........'

Ninepin$.SymbolData$K <- '
.8....8.
.8...8..
.88.8...
.8.8....
.8..8...
.8...8..
.8....8.
........'

Ninepin$.SymbolData$L <- '
.8......
.8......
.8......
.8......
.8......
.8......
.888888.
........'

Ninepin$.SymbolData$M <- '
8.....8.
88...88.
8.8.8.8.
8..8..8.
8.....8.
8.....8.
8.....8.
........'

Ninepin$.SymbolData$N <- '
.8....8.
.88...8.
.8.8..8.
.8..8.8.
.8...88.
.8....8.
.8....8.
........'

Ninepin$.SymbolData$O <- '
...888..
..8...8.
.8.....8
.8.....8
.8.....8
..8...8.
...888..
........'

Ninepin$.SymbolData$P <- '
.88888..
.8....8.
.8....8.
.88888..
.8......
.8......
.8......
........'

Ninepin$.SymbolData$Q <- '
...888..
..8...8.
.8.....8
.8.....8
.8...8..
..8...8.
...88..8
........'

Ninepin$.SymbolData$R <- '
.88888..
.8....8.
.8....8.
.88888..
.8....8.
.8....8.
.8....8.
........'

Ninepin$.SymbolData$S <- '
..8888..
.8....8.
.8......
..8888..
......8.
.8....8.
..8888..
........'

Ninepin$.SymbolData$T <- '
.88888..
...8....
...8....
...8....
...8....
...8....
...8....
........'

Ninepin$.SymbolData$U <- '
.8....8.
.8....8.
.8....8.
.8....8.
.8....8.
.8....8.
..8888..
........'

Ninepin$.SymbolData$V <- '
.8.....8
.8....8.
.8...8..
.8..8...
.8.8....
.88.....
.8......
........'

Ninepin$.SymbolData$W <- '
8.....8.
8.....8.
8.....8.
8..8..8.
8.8.8.8.
88...88.
8.....8.
........'

Ninepin$.SymbolData$X <- '
.8.....8
..8...8.
...8.8..
....8...
...8.8..
..8...8.
.8.....8
........'

Ninepin$.SymbolData$Y <- '
.8.....8
..8...8.
...8.8..
....8...
....8...
....8...
....8...
........'

Ninepin$.SymbolData$Z <- '
.8888888
......8.
.....8..
....8...
...8....
..8.....
.8888888
........'

Ninepin$.SymbolData$`-` <- '
........
........
........
..8888..
........
........
........
........'

Ninepin$.SymbolData$`'` <- '
....8...
....8...
........
........
........
........
........
........'


Ninepin$.SymbolData$. <- '
........
........
........
........
........
........
....8...
........'

Ninepin$.SymbolData$`,` <- '
........
........
........
........
........
........
....8...
....8...'

Ninepin$.SymbolData$`/` <- '
.......8
......8.
.....8..
....8...
...8....
..8.....
.8......
........'


Ninepin$.Symbols <- lapply(Ninepin$.SymbolData, Ninepin$dots2symbol)
Ninepin$.Symbols$DEL <- !Ninepin$.Symbols$Space

Ninepin$symbol <- function(x) {
	if (x == " ") x <- "Space"
	if (x %in% names(Ninepin$.Symbols)) {
		Ninepin$.Symbols[[x]]
	} else { Ninepin$.Symbols$DEL }
}

Ninepin$symbols <- function(x)
		do.call(rbind, lapply(characters(x), Ninepin$symbol))

Ninepin$stamp <- function(
		spec, xmax=NULL, ymax=NULL,
		w=nchar(spec) * Ninepin$.Width,
		h=Ninepin$.Height,
		col=Ninepin$.BG %,% Ninepin$.FG, ...) {
	#cater(w)
	#cater(h)
	#print(spec)
	#print(Ninepin$symbols(spec))
	stamp(Ninepin$symbols(spec), w=w, h=h, xmax=xmax, ymax=ymax, col=col, useRaster=T, ...)
}

Ninepin$symbols("EARLY")

"         1         2         3         4         5         6         7"
"1234567890123456789012345678901234567890123456789012345678901234567890123456789"
lines <-
"EARLY RESEARCH IN COMPUTER GRAPHICS WAS" %,%
"DELAYED DECADES BECAUSE OF MANAGEMENT'S" %,%
"OVERREACTION,   NON-EXISTANT  SENSE  OF" %,%
"HUMOR,  LACK OF ART APPRECIATION,   AND" %,%
"FAILURE TO SEE NOTHING  FOR WHAT IT IS."
par(bg=WHT)
blank.slate()
s <- 8 # spacing
h <- 13 # line height
Ninepin$stamp(lines[1], xmax=60*s, ymax=11*h, col=c(TRANSPARENT, BLK))
Ninepin$stamp(lines[2], xmax=60*s, ymax=10*h, col=c(TRANSPARENT, BLK))
Ninepin$stamp(lines[3], xmax=60*s, ymax=9*h, col=c(TRANSPARENT, BLK))
Ninepin$stamp(lines[4], xmax=60*s, ymax=8*h, col=c(TRANSPARENT, BLK))
Ninepin$stamp(lines[5], xmax=60*s, ymax=7*h, col=c(TRANSPARENT, BLK))
Ninepin$stamp("00", xmax=41, ymax=57, col=c(TRANSPARENT, BLK))
