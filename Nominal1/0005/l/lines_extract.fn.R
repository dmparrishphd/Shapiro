lines_extract.fn <- function(filename, i) {
    f <- file(filename, open="rt")
    Return <- lines_extract.cn(f, i)
    f %|% close
    Return }

    Doc$lines_extract.fn <- '
        lines_extract.fn is similar to lines_extract.cn, except
        that the name of a file is given (arg 1) rather than a
        connection. Line 1 of the file corresponds with index 1.'
