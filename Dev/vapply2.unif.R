vapply2.unif <- function(n, FUN, zero.include=T, one.include=T)
        vapply2(FUN=FUN, X=seq_unif(n,
            zero.include=zero.include, one.include=one.include))
