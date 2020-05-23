fn.enumerate.cn <- function(con, warn=T, encoding="unknown", skipNul=F) { #TAGS file connection lines number
        readLines. <- readLines %^% list(con=con, n=1, warn=warn, encoding=encoding, skipNul=skipNul)
        k <- 0L
        Mode <- T # T ==> normal mode; F ==> past end-of-file mode
        function() {
            k <<- k + 1L
            if (Mode) {
                Line <- readLines.()
                #REMINDER: length(LINE) IS NOT LINE LENGTH, BUT THE NUMBER OF CHARACTER VECTORS READ, WHICH SHOULD BE EITHER ZERO OR ONE.
                if (!length(Line)) Mode <<- ! Mode }
            if (Mode) c(k, Line) else as.character(NA_integer_) } }

    Doc$fn.enumerate.cn <- '
        fn.enumerate.cn returns a function that enumerates lines
        from a sequential text file connection.

        The returned function **** MUTATES **** the underlying
        connection.
        
        Enumeration is from 1. The return value may be adjusted
        for other numbering datums.

        TYPICAL APPLICATION

        foo <- fn.enumerate.cn(filename)

        foo() # RETURNS ENUMERATED FIRST LINE, IF THERE IS ONE.

        foo() # RETURNS ENUMERATED SECOND LINE, IF THERE IS ONE.

        # AFTER THE END-OF-FILE IS REACHED:

        foo() # RETURNS as.character(NA): THERE ARE NO MORE LINES.
        '

