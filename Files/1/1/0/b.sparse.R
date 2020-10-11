b.sparse <- function (n, length.out=max(n)) {
        value <- length.out %|% b.rep
        value[n] <- T
        value }
