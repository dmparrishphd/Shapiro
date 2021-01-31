.transpose.by.name.template <- function(X)
        list_frame(X %|% `#`, .names=unique.names(X))
