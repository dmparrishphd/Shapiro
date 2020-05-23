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

HDIGITS <- 0:9 %|% as.character
HHEX <- HDIGITS %,% LETTERS[1:6]

RRED <- "red"
RCYN <- "cyan"
RPUR <- "purple"
RGRN <- "green"
RBLU <- "blue"
RYEL <- "yellow"
RORG <- "orange"
RBRN <- "brown"

is.from.letter.bank <- function (bank, x) { #TAGS anagram digits hex
    Return <- rep_along(along.with=x)
    Return[grep("^["  %//% bank %//% "]+$", x)] <- T
    Return }

is.bin <- is.from.letter.bank %<=% "01"
is.oct <- is.from.letter.bank %<=% "0-7"
is.dec <- is.from.letter.bank %<=% "0-9" #TAGS digit
is.hex <- is.from.letter.bank %<=% "0-9A-F"

colorsplit <- function (x)
        lapply(
            lapply(x, substrs %|% argswap %<=% c(1, 3, 5, 7, 9)),
            function(y) if (y[5] %|% nchar != 2) y[1:4] %,% "FF" else y)

is_color.vector <- function (x) {
    if (! x %|% is.character) return (F)
    LG <- vapply(x, nchar, L)
    if (any(LG  < 7)) return (F)
    if (any(LG == 8)) return (F)
    if (any(LG  > 9)) return (F)
    if (any(left(x) != "#")) return (F)
    substrs

    vapply(vapply(x, nchar, L), `%in%` %|% argswap %<=% ic(7, 9), T)
     }

    Doc$color.vector <- '
        A color.vector is a character string'

alpha <- function (colors)
        vapply(FUN.VALUE="", X=substr(colors, 8, 9),
            FUN=function(h) if (h %|% nchar != 2) "FF" else h)

noalpha <- function (colors)
        substr(colors, 1, 7)

    Doc$alpha <- '
        alpha returns the alpha channel (as a character vector of 2-strings)
        of a "#" color character vector.'

    Doc$noalpha <- '
        noalpha returns a "#" color character vector without the
        alpha field.'

wash <- function (opaque.colors, alpha=.5) #TAGS translucent transparent
        paste0(noalpha(opaque.colors), substr(hcl(a=alpha), 8, 9))

#TODO FACTOR OUT COLOR EXCLUSION
colors.random <- function (n=1,
        patterns.exclude="black gray grey white" %|% words) {
    acolors <- colors()[
        lapply(patterns.exclude, function(h) grep(h, colors())) %|%
                unlist %|% unique %|% `-`]
    N <-  acolors %|% `#`
    if (n < 1) return (N)
    acolors[N %|% shuffle %[mod% 1:n] }

col2color.vector <- function (col)
        (col %|% col2rgb %|% t / 255) %|% rgb

    Doc$colors.random <- '
        colors.random returns a character vector of colors chosen randomly
        from among colors(). The number of colors is determined by n'

    Doc$col2color.vector <- '
        col2color.vector returns a color vector (e.g., "#000000")
        given a vector of any of the kinds of colors supported by
        col2rgb.
        
        9 %|% colors.random %|% col2color.vector'

TRANSLUCENT <- wash("black" %|% col2color.vector, 1:7 * 32 / 255)

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

hc1.scale <- function(breaks, h=0, a=1, ...) {
	CENTERS <- capply(breaks %|% fdpairs, mean)
    CENTERSR <- CENTERS %|% range
	CENTERS01 <- rescale(CENTERS, CENTERSR %|% range)
    hc1(
        h=rescale(x=CENTERS01, to=h %[mod% 1:2),
        c=CENTERS01,
        a=a) }

    Doc$hc1.scale <- '
        hcl1-scale--proto.R
    '

seq_kk <- function (n=16, from0=T) #TAGS circle angles radians
        seq(from=0, to=2*pi, length.out=1+n) %|% (
            (rest %,% except.last) [[from0 %|% index.b]])

m.seq.kk <- function (n=16)
        do.call(cbind, applyf(n %|% seq_kk, list(cos, sin)))

'"
hsl.wheel #OLDNAME hcl.wheel
""'
hcl.wheel <- function (n=16, cex=4, plot.args=NULL, par.args=NULL) { #TAGS color palette
    'STATUS: **** IN DEVELOPMENT **** Do not rely on any
    specific behavior of this function. Future; xkcd color
    names.' %|% warning
    par(mar=rep4(0))
    image_bare(.ll*9/8, .ll*9/8, M1, col=BLK, asp=1, add=F)
    xy <- n %|% m.seq.kk
    segments(xy %|% firstc, xy %|% secondc, 0, 0, col=grey(1/8))
    points(xy, col=hc1(h=0:n %|% except.last / n), cex=cex,
           pch="CIRCLE SOLID" %|% pch.h)
    if (n == 16) {
        text(xy * 2 / 3, col=DGR, c(
            "RED",
            "ORANGE",
            "BROWN",
            "YELLOW",
            "YELLOW-GREEN",
            "GREEN",
            "",
            "",
            "CYAN",
            "",
            "",
            "BLUE",
            "INDIGO",
            "PURPLE",
            "MAGENTA",
            "PINK") ) }
    }

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

pal <- function(dark=1, n=16, alpha=1, reverse=F)
	    hc1(
            h=rotate..n(1:n/n, dark),
            c=(if (reverse) rev else identity)(1:n/n),
            alpha=alpha)

    Doc$pal <- '
        pal returns a color palette (character vector) that
        spans a wide range of linearly-varying luminance. Hues are
        regularly spaced around the color hcl color wheel. chroma is
        is proportional to luminance.

        Arguments
        
        dark tells the index of which of the hues is darkest and
        simultaneously defines one end of the return.

        n specifies the number of colors in the return.

        alpha has the same meaning as in the hcl function.

        reverse tells whether the lighter colors are at the
        beginning of the palette and **** WITHOUT AFFECTING
        **** affecting the variation in hue.'

LLG <- grey(7/8) # light, light grey
LGR <- grey(3/4)
GRY <- grey(2/4)
DGR <- grey(1/4)


find_colors.h <- function (h) colors()[grep(h, colors(), ignore.case=T)]

'
function find_colors.h

        Returns a character vector of R colors having a
        substring matching the argument.
'
