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
