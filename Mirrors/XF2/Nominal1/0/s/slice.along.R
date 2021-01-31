slice.along <- function (x, i, dimNo=1) { #TAGS array extract
    l <- x %|% dim %|% li.dim
    l[[dimNo]] <- i
    x %[L% l }
