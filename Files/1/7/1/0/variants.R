variants <- function (FUN, ARGS)
        lapply(ARGS, function(arglist) FUN %^% arglist)
