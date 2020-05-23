arrayInd.range <- function (.dim) #TAGS array index
        rbind_(1L, .dim %|% floor %|% as.integer)
