.xrub <- function(FUN)
        function(pattern, x, ...)
                FUN(pattern, "", x, ...)
