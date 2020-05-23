blank.data.frame <- function (prototypes, nrow=1)
    do.call(rbind, rep(times=nrow, list(data.frame(
        check.names=F, fix.empty.names=F, stringsAsFactors=F,
                `names<-`(prototypes, if (
                    prototypes %|% names %|% is.null)
                    prototypes %|% `#` %|% regular.names else
                    prototypes %|% names) ) ) ) )


