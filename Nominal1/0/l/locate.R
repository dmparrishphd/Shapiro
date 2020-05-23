locate <- function (v, w) #TAGS search
        vapply(v, function(x, y) look(x, y)[1], integer(1), w)
