alpha <- function (colors)
        vapply(FUN.VALUE="", X=substr(colors, 8, 9),
            FUN=function(h) if (h %|% nchar != 2) "FF" else h)
