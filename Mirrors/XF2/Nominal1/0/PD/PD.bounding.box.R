.bounding.box <- function(X) {
    if (X %|% is.vector) return (range.finite %O% enlist %-|% X)
    if (X %|% is.matrix) return (X %|% ranges)
    if (X %|% is.data.frame) return (X %|% ranges)
}
