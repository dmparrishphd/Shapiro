branches <- function (l) {
    ll <- list()
    nom <- names(l)
    i <- 0
    for (item in l) {
            i  % %  succ -> i
            if (item  % %  is.list) {
                    ll <- c(list(item), ll)
                    if (!!(nom[i]  % %  `#`))
                            names(ll)[1] <- nom[i] } }
    ll }
