glyph.encode.h <- function (h, ncol=8)
        sum.m(2 ** (1:ncol - 1L) * matrix(
            h %|% dec.sift.h %|% as.integer, ncol=ncol))
