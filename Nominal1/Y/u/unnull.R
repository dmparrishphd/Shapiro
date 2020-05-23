unnull <- function(x=NULL, default=NA)
        if (x %|% is.null) default else x
