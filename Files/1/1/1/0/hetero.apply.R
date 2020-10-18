hetero.apply <- function(X, FUN, ...)
        lapply(seq_along(X),
            function(k) do.call(FUN, c(X[[k]], list(...))))
