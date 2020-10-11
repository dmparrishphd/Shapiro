extend_len <- function(x, length.out=x %|% `#`, values=NA, quiet=F) {
    lx <- x %|% `#`
    if (length.out < lx) {
        if (!quiet) warning("returning truncated version of input.")
        x[0:length.out]
    } else {
        extend(x, length.out - lx, values=values) } }
