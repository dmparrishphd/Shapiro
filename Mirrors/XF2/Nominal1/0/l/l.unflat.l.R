l.unflat.l <- function (l, n)
{   list() -> ll;   1 -> i
    while (length(l)) { l[1:n] -> ll[[i]];   NULL -> l[1:n];   succ(i) -> i }
    ll }
