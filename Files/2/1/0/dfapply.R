dfapply <- function (df1, FUN, FUN.VALUE=NULL, ..., USE.NAMES=T)
    vapply(
        df1 %|% nrow %|% seq,
        function(i) do.call(FUN, df1[i,]),
        FUN.VALUE=if (FUN.VALUE %|% is.null)
                FUN.VALUE <- do.call(FUN, df1[1,]) else
                FUN.VALUE)
