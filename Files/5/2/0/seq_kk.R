seq_kk <- function (n=16, from0=T)
        seq(from=0, to=2*pi, length.out=1+n) %|% (
            (rest %,% except.last) [[from0 %|% index.b]])
