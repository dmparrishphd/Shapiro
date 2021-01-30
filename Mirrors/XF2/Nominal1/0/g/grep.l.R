grep.l <- function(pattern, X, ...) {
    LIST <- lapply(seq_along(X), function(k)
        grep(pattern, X[[k]], value=T, ...) )
    FOUND <- as.logical(vapply(LIST, length, 1L))
    `names<-`(LIST[FOUND], names(X)[FOUND]) }
