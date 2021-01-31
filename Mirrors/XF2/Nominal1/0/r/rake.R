rake <- function (t)
{   tt <- t
    for (i in seq_along(t)) {
        if (!is.list(t[[i]])) tt[[i]] <- t[[i]] else # is.list was is.List
        if (t[[i]]  % %  is.list.simple) tt[[i]] <- h.l(t[[i]]) else
        tt[[i]] <- rake(t[[i]]) }
    tt }
