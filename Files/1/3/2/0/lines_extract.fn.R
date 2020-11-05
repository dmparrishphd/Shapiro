lines_extract.fn <- function(filename, i) {
    f <- file(filename, open="rt")
    Return <- lines_extract.cn(f, i)
    f %|% close
    Return }
