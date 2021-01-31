sign_eval.lines <- function (lines, points)
        compare(lines[-1,] %|% t %*% points, -lines[1,])
