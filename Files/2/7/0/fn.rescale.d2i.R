fn.rescale.d2i <- function (from, to) {
    DX <- diff(from) / diff(to)
    function(x) to[1] + (DX/2 + x - from[1]) %/% DX }
