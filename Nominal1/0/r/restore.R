restore <- function (singleton, alternate=NA) #TAGS NULL
        if (singleton %|% is.null) alternate else singleton
