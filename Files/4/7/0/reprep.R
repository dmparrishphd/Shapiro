reprep <- function(x, times.each=1L)
        lapply(
            x %|% seq_along,
            function(i) rep(x[[i]], times.each %[mod% i)) %|% unlist
