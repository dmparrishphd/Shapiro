tukey.fence <- function(x, fence=1.5, na.rm=T, type=7, ...) {
    Q13 <- quantile(x, probs=c(1,3)/4, na.rm=na.rm, names=F, type=type, ...)
    WHISKERS <- Q13 + fence * c(-1, 1) * diff(Q13)
    x < WHISKERS[1] | WHISKERS[2] < x }
