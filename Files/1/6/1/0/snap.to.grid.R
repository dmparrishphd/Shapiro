snap.to.grid <- function(x0, gridsize, x)
        x0 + gridsize * (isnap.to.grid(x0, gridsize, x) - 1/2)
