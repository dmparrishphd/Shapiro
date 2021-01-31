multiple.extract <- function (
    X, i, XEXTRACT=`[[`, IEXTRACT=`[[`, XXEXTRACT=`[[`)
        lapply(
            X %|% seq_along,
                function(k)
                    XXEXTRACT(
                        XEXTRACT(X, k),
                        IEXTRACT(i, k) ) )
