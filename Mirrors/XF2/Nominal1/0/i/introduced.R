introduced <- function(X) { #TAGS names alignment populate extend
    Return <- X
    Names <- unique.names(X)
    for (k in X %|% seq_along)
    for (Name in Names)
    if (Name %in% names(X[[k]]) %|% `!`)
    Return[[k]] <- c(Return[[k]], list(NULL) %=:% Name)
    Return }
