m.FUN.m..v <- function(FUN)
        function(m, v) FUN(m %|% t, v) %|% t
