xy.regular.polygon.xy <- function (n) {
    a <- seq(from=-pi, to=pi, length.out=1+n)
    cbind(cos(a), sin(a)) }
