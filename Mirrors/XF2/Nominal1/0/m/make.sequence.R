make.sequence <- function(FUN, seed=0L, length.out=2L, ref=integer()) {
    for (i in seq(seed  % %  `#`  % %  succ, to=length.out)) seed[i] <- FUN(i, seed, ref)
    seed }
