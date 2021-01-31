.ranges.m <- function (m)
    lapply(
        m %|% colNos,
        `[` %<=% m %<=% rowNos(m) %O% range.finite)
