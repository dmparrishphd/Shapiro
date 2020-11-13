`%1|n%` <- function(X, FUNs)
        lapply %<=% FUNs %:|% (`%|%` %<=% X)
