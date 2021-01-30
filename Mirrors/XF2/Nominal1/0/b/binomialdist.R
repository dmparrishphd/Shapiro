binomialdist <- function(n, k=0:n, p=0.5)
        choose(n, k) * p ^ k * (1 - p) ^ (n - k)
