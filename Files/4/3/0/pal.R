pal <- function(dark=1, n=16, alpha=1, reverse=F)
	    hc1(
            h=rotate..n(1:n/n, dark),
            c=(if (reverse) rev else identity)(1:n/n),
            alpha=alpha)
