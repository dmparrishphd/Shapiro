b.beyond.interval <- function (interval, x)
        vapply(
            x %|% seq_along,
            function(i) beyond2(interval[,i], x[i]),
            T)
