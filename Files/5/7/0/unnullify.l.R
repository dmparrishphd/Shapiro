unnullify.l  <- function (list_, default=NA)
        lapply(list_, function(x) if (x %|% is.null) default else x)
