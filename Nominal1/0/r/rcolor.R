rcolor <- function(n=1, exclude=integer()) {
    COLORS      <- colors()
    NCOLORS     <- COLORS %|% `#`
    EXCLUDE     <- NCOLORS %|% succ %,% exclude
    COLORS.USE  <- COLORS[-EXCLUDE]
    NCOLORS.USE <- NCOLORS.USE %|% `#`
    i <- (NCOLORS.USE %|% shuffle)[1:n]
    COLORS.USE[i] }
