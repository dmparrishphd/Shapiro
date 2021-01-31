l.multifunction <- function (FUNs)
    function(X) lapply(
        X,
        function(x) lapply(
            FUNs,
            `%|%` %<=% x))
