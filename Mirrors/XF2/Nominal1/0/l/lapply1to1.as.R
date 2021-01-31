lapply1to1.as = function (x, mode.abbr="i", ...) {
    CH <- strsplit("lidchr", "")[[1]]
    AS <- c(as.logical, as.integer, as.double, as.complex,
        as.character, as.raw)
    Ch <- strsplit(mode.abbr, "")[[1]]
    As <-= list()
    for (i in Nos.(Ch))
        As[[i]] = AS[[which(Ch[i] == CH)]]
    lapply1to1(x, As) }
