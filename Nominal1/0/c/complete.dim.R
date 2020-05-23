complete.dim <- function (x, .dim) {
    .dim <- .dim %|% ifloor
    DD <- .dim %|% iprod
    LG <- x %|% `#`
    if (DD < LG) {
        .dim <- c(.dim, LG %\% DD)
        DD <- .dim %|% iprod }
    if (DD > LG) stop(
        "dims [" %//% unwords(.dim) %//% "] product [" %//% DD %//% "] exceeds length [" %//%
        LG %//% "] of object.")
    .dim }
