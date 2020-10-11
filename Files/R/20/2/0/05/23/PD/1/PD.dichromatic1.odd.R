.dichromatic1.odd <- function(h, h2, n, alpha=1) {
    nn <- 1L + n %/% 2
    rev(monochrome1(h=h2, n=nn, alpha=alpha)[-1]) %,%
    hc1.greys(c=1:nn/nn)[1] %,%
    monochrome1(h=h, n=nn, alpha=alpha)[-1] }
