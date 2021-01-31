pp.lpp <- function(x)
        list(nop, .pp.lpp)[[x %|% is.lpp %|% index.b]](x)
