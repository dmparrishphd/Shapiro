index123.b <- function (b)
    vapply(index.b(b),
            function(x) if (x  % %  is.na) 3L else x,
            FUN.VALUE=integer(1))
