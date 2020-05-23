first <- function(x)
        if (0 < x %|% length) { x[[1]]
        } else if (x %|% is.vector) { x
        } else { NULL }
