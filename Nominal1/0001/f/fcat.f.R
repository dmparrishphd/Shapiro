fcat.f <- function(filenames, filename)
    fcat.cn(
        lapply(filenames, file, open="rt"),
        file=file(filename, open="wt") )
