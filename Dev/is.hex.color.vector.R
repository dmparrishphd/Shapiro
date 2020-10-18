is.hex.color.vector <- function (x) {
stop("UNTESTED!")
        x %|% is.character &&
        !any(x %|% is.na) &&
        all(x %|% nchar %in% c(7, 9) &&
        all(x %|% left == "#") &&
        x %|% characters %|% unlist %|% is.hex %|% all }
