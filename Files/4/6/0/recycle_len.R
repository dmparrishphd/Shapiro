recycle_len <- function (x, length.out)
        recycle(x, function(y) y %|% `#` >= length.out)[1:length.out]
