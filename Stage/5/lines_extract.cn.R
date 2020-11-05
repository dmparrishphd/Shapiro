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

    Doc$lines_extract.cn <- '
        lines_extract.cn returns an nx1 matrix where each elment
        is the line read from the text connection (arg 1) that
        corresponds to the integer index i (arg 2).

        END-OF-LINE markers are **** STRIPPED. ****

        The rownames of the return are the character equivalent
        of the line numbers. NUMBERING begins at 1, ****
        REGARDLESS OF THE FILE POSITION **** of the text
        connection on entry.

        The index (arg 2) is **** ASSUMED **** to be sorted.
        Expect strange behavior if that is not the case.
        
        TIP: If lines are needed out of order, call multiple
        times or reorder the return.'
