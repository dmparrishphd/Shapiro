is.color.vector <- function (x) {
    if (! x %|% is.character) return (F)
    NOPOUND <- left(x) != "#"
    ANYNOPOUND <- any(NOPOUND)
    if (ANYNOPOUND) return (F)
    NCHAR <- vapply(x, nchar, 1L)
    OK <- c(7L, 9L)
    all(NCHAR %in% OK) }
