idrop <- function(i)
        if (i %|% `#` == 1 && i == 1) i else i[which(i != 1)]
