multifunction <- function (FUNs)
    function(X) vapply(
        X,
        function(x) vapply(
            FUNs,
            `%|%` %<=% x,
            1 %|% double),
        FUNs %|% `#` %|% double)
