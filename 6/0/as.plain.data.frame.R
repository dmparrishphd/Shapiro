as.plain.data.frame <- function(x) (
    if (x %|% is.matrix) {
        as.plain.data.frame.m
    } else if (x %|% is.data.frame) {
        anon
    } else if (x %|% is.list) {
        as.plain.data.frame.l
    } else if (x %|% is.character) {
        as.plain.data.frame.h
    } else if (x %|% is.vector) {
        matrix %O% as.plain.data.frame
    } else nop )(x)
