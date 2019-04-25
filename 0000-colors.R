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

# THESE MAGIC NUMBERS WERE DETERMINED EMPERICALLY. SEE ./Doc/Colors/colo.R FOR DETAILS
.hc1.K <- 1+38/128
.hc1.CMAX <- 59

hc1 <- function(h=0, c=1, alpha=1)
	hcl(h=360*h, c=.hc1.CMAX * c, l=.hc1.K * .hc1.CMAX * c, alpha=alpha, fixup=F)

	Doc$hc1 <- '
        hc1 produces colors from a subset of the hcl colors. The
        h, c, and alpha parameters should all fall in the range
        [0, 1]; hence the "1" in "hc1." This range maps to the
        full range of hues, and to chroma values [0, 60]. No
        out-of-gamut colors should be produced, provided that
        arguments are within range.'

hc1.greys <- function(c=1, alpha=1)
	hcl(c=0, l=.hc1.K * .hc1.CMAX * c, alpha=alpha, fixup=F)

	Doc$hc1.greys <- '
		hc1.greys is intended to produce grey shades that match the luminances
		of the colors returned from hc1 for the same c and alpha arguments.'

monochrome1 <- function(h=0, n=2, alpha=1)
	hc1(h=h, c=1:n/n, alpha=alpha)

	Doc$monochrome1 <- 'h is scaled to [0, 1]'

.dichromatic1.even <- function(h, h2, n, alpha=NULL)
        rev(monochrome1(h=h2, n=n %/% 2, alpha=alpha)) %,%
		monochrome1(h=h, n=n %/% 2, alpha=alpha)

.dichromatic1.odd <- function(h, h2, n, alpha=1) {
    nn <- 1L + n %/% 2
    rev(monochrome1(h=h2, n=nn, alpha=alpha)[-1]) %,%
    hc1.greys(c=1:nn/nn)[1] %,%
    monochrome1(h=h, n=nn, alpha=alpha)[-1] }

dichromatic1 <- function(h=0, h2=NULL, n=16, alpha=NULL) {
	if (!is.null(alpha)) warning("behavior of alpha parameter subject to change. not implemented at this time.")
	h2 <- if(h2 %|% is.null) frac(h + .5) else h2
	(if (n %% 2) .dichromatic1.odd else .dichromatic1.even)(h, h2, n, alpha) }

pal <- function(dark=1, n=16, alpha=1, reverse=T)
	hc1(h=rotate..n((0:n)[-1]/n, dark), c=proc(1:n/n), alpha=alpha) %|%
	(proc <- if (reverse) rev else identity)


LLG <- grey(7/8) # light, light grey
LGR <- grey(3/4)
GRY <- grey(2/4)
DGR <- grey(1/4)

RBRN <- "brown"
RORG <- "orange"
RRED <- "red"
RPUR <- "purple"
RBLU <- "blue"
RCYN <- "cyan"
RGRN <- "green"
RYEL <- "yellow"

find_colors.h <- function (h) colors()[grep(h, colors(), ignore.case=T)]

'
function find_colors.h

        Returns a character vector of R colors having a
        substring matching the argument.
'
