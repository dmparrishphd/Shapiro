eval.lines <- function (lines, points)
        lines[-1,] %|% t %*% points + lines[1,]
