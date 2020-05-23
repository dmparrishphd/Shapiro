col2color.vector <- function (col)
        (col %|% col2rgb %|% t / 255) %|% rgb
