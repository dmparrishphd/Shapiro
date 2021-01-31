lapply1to1 = function (X, FUNs, ...)
        lapply(
            X %|% seq_along,
            function(n) `%[[mod%`(FUNs, n)(X %[[% n, ...))
