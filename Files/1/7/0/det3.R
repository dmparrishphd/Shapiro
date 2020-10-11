det3 <- function (m)
        sum(m[1,]   *   c(1L, -1L, 1L)   *   vapply(
            1:3, submatrix3 %<=% m %<=% 1 %O% det2,
                    m %|% typeof %|% vector1))
