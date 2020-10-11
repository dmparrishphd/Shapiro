as.vector0 <- function (...)
    lapply(lapply(list(...), typeof), vector) %|% unlist
