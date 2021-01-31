named <- function (x) {
    if (is.null(names(x))) names(x) <-
            rep("", times=length(x))
    x }
