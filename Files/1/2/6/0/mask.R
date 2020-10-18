mask <- function(b, x, maskvalue=NA) {
    x[b %|% which] <- maskvalue
    x }
