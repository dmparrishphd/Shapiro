l.extend.l <- function (l, n=0L, what=NULLs(1))
{   n - `#`(l) -> xlen # length of the extended part
    if (xlen <= 0L) return (l)
    l.join.l(l, rep_len(what, xlen)) }
