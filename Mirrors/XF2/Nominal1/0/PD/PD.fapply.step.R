.fapply.step <- function(
        connection, FUN,
        next.item=next.item.f.default) {
    item <- connection %|% next.item
    if (item %|% `#`) item %|% FUN %|% list else list() }
