is.pp <- function (X)
        X %|% is.matrix & X %|% mode == "numeric"
