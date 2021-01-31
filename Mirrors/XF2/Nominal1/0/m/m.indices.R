m.indices <- function (dims) #TAGS index matrix array
        arrayInd(dims %|% prod %|% seq, dims)
