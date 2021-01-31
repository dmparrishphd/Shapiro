igrid.regular <- function (.dim)
        arrayInd(.dim %|% prod %|% seq, .dim)
