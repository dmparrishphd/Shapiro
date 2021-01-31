transpose.by.name <- function(X) {
    Return <- .transpose.by.name.template(X)
    for (k in X %|% seq_along) {
    for (n in X[[k]] %|% seq_along) {
        Return[[names(X[[k]][n])]][[k]] <- X[[k]][[n]]
    } }
    Return }
