.fapply.step <- function(
        connection, FUN,
        next.item=next.item.f.default) {
    item <- connection %|% next.item
    if (item %|% `#`) item %|% FUN %|% list else list() }

fapply.cn <- function(
        connection, FUN,
        next.item=next.item.f.default) {
    Return <- list()
    repeat {
        y <- .fapply.step(connection, FUN, next.item)
        if (y %|% is.empty) break else Return <- append(Return, y) }
    Return }

    Doc$fapply.cn <- '
        fapply.cn is similar to lapply, except that arg 1 is a
        connection.  The present position of the connection is
        taken as the beginning of item 1.  The optional
        next.item function-argument defines how the connection
        is traversed.'

fapply.f <- function(
        filename, FUN,
        next.item=next.item.f.default,
        open="rt", ...) {
    cn <- file(filename, open=open, ...)
    Return <- fapply.cn(cn, FUN, next.item)
    close(cn)
    Return }

    Doc$fapply.f <- '
        fapply.f is similar to fapply.cn, except that arg 1 is
        the name of a file rather than a connection. fapply.f
        opens the file and closes it again before terminating.
        Intended for application with a "read" mode for the open
        argument.'
