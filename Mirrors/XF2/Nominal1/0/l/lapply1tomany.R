lapply1tomany <- function(x, FUNs)
        lapply1to1(rep_along(list(x), FUNs), FUNs)
