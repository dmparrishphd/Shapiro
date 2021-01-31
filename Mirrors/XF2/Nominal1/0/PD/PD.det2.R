.det2 <- function(m)
        m[-(2:3)] %|% prod - m[2:3] %|% prod
