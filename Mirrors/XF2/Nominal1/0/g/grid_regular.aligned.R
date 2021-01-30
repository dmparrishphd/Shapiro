grid_regular.aligned <- function (X) {
    i <- igrid.regular(vapply(X, `#`, 1L))
    vapply(
        X %|% seq_along,
        function(k) X[[k]][i[,k]],
        vector(mode=X[[1]] %|% typeof, length=i %|% nrow)) }
