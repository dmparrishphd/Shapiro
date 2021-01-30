binaryToVariadic <- function(FUN)
        function(...) Reduce(FUN, c(..1, ..2, list(...)[-2:-1]))
