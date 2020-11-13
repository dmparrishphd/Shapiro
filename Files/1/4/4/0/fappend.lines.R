fappend.lines <- function(file, .character) {
    if (!is.character(.character)) stop("fappend.lines: arg 2 must be a character vector.")
    cat(.character, file=file, sep="\n", append=T) }
