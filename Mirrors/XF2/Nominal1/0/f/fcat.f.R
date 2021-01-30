fcat.f <- function(filenames, filename) {
    Infiles <- lapply(filenames, file, open="rt")
    Outfile <- file(filename, open="wt")
    fcat.cn(Infiles, Outfile)
    for (Infile in Infiles) close(Infile)
    close(Outfile)
}
