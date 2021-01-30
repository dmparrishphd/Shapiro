are.named <- function (v) {
    noms <- v  % %  names
    if (noms  % %  is.null) vector(length=v  % %  `#`) else
            noms % %  nchar  % %  as.logical }
