fapply.f <- function(
        filename, FUN,
        next.item=next.item.f.default,
        open="rt", ...) {
    cn <- file(filename, open=open, ...)
    Return <- fapply.cn(cn, FUN, next.item)
    close(cn)
    Return }
