lines_extract.cn <- function(text.connection, i, quiet=T) {
    i <- i %|% rmna %|% uniques
    Return <- list(c("DUMMY", "DUMMY"))
    if (i %|% un(is.empty)) {
        imax <- i %|% max
        fenum <- fn.enumerate.cn(text.connection)
        k <- 0L
        if (!quiet) flush.cat("lines_extract.cn: attempting to extract", i %|% `#`, "lines.\n", file=stderr())
        repeat {
            k <- k + 1L
            if (!quiet) flush.cat("lines_extract.cn:", k, " of", i %|% `#`, "lines\n", file=stderr())
            if (imax < k) break
            Info <- fenum()
            if (Info[1] %|% is.na) break
            if (Info[1] %in% i) Return <- list(Return, Info) } }
    with_dimnames.from.within(
        matrix(unlist(Return), 2)[,-1] %|% t,
        direction=1)[, 2, drop=F] }
        
