concatenate <- function(h, ..., collapse="") #TAGS string character
        paste0(c(h, list(...) %|% unlist), collapse=collapse)
