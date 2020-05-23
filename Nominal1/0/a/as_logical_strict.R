as_logical_strict <- function (x, na=F)
{   y <- as.logical(x)
    y[y  % %  is.na] <- na
    y }
