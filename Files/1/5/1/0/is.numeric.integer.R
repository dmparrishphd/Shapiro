is.numeric.integer <- function(x)
        if (x %|% mode != "numeric") {
            rep(F, x %|% `#`)
        } else {
            x == x %|% floor }
