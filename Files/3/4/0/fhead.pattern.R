fhead.pattern <- function(filename, pattern) {
    f <- file(filename, open="rt")
    LINES <- funtil(f, pattern)
    close(f)
    LINES }
