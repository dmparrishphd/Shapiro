funtil <- function(connection, pattern="^$") {
    Lines <- list()
    repeat {
        Line <- fhead.n(connection, n=1)
        if (!length(Line) || grepl(pattern, Line)) break
        Lines <- list(Line, Lines) }
    rev(unlist(Lines)) }
