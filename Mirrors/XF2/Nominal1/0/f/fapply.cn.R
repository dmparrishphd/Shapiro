fapply.cn <- function(
        connection, FUN,
        next.item=next.item.f.default) {
    Return <- list()
    repeat {
        y <- .fapply.step(connection, FUN, next.item)
        if (y %|% is.empty) break else Return <- append(Return, y) }
    Return }
