fcat.cn <- function(.open.cons, file)
        for (con in .open.cons)
            repeat {
                Line <- readLines(con, n=1)
                if (!length(Line)) break
                cat(Line, file=file, sep="\n") }
