dim. <- function (x)
    c(dim, `#`)[[x %|% dim %|% is.null %|% index.b]] %:|% x
