n.index.i <- function (factors, i) #TAGS array indexing
        succ(sum(pred(i) * factors)) %|% as.integer
