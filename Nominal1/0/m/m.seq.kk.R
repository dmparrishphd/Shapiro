m.seq.kk <- function (n=16) do.call(
    cbind, applyf(n %|% seq_kk, list(cos, sin)))
