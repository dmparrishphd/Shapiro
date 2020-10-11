find_colors <- function (pattern) grep(
    pattern, colors(), ignore.case=T, value=T)
