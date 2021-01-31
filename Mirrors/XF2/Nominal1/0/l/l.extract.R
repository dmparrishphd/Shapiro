l.extract <- function(X, indices=list()) lapply(indices, `[[` %<=% X)
