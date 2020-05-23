.dichromatic1.even <- function(h, h2, n, alpha=NULL)
        rev(monochrome1(h=h2, n=n %/% 2, alpha=alpha)) %,%
		monochrome1(h=h, n=n %/% 2, alpha=alpha)
