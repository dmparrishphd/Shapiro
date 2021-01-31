spread.l <- function(l, sep=NULL, spread=1L, trailing=T) {
    if (spread != 1L)
            stop("spread != 1 not yet implemented!")
    # TO DO: REFACTOR SO THAT jump.b is unnecesary. can use
    # something like c(NULLs(1), l)
    lgt <- l %|% `#`
    if (is.null(sep))
            m <- NULLs(lgt) else
            m <- rep(sep  % %  list, lgt)
    l.interlace.l(
        jump.b(
            trailing,
            list(rev, identity),
            list(l, m))) }
