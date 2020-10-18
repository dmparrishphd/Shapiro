        function(m, v) FUN(m, v)

m.FUN.m..v <- function(FUN)
        function(m, v) FUN(m %|% t, v) %|% t
