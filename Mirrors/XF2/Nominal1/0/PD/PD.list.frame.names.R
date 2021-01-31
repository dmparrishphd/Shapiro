.list.frame.names <- function(.names, nrow)
    rename.all(
        .list.frame.dim(c(nrow, .names %|% `#`)),
        .names)
