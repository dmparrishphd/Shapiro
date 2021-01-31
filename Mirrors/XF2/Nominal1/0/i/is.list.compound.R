is.list.compound <- function (x) is.list(x) && any(unlist(lapply(x, is.list)))
