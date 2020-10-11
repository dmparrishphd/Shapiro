fn.rescale <- function (from, to)
        #TODO: REWRITE IN TERMS OF WILDBERGERS LINES:
        #       cbind(from, to) ...
        if (to %|% `#` < 2) {
            return (function(x) to)
        } else {
            function(x) to[1] + (x - from[1]) * diff(to) / diff(from) }
