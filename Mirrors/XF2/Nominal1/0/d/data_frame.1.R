data_frame <- function(..., row.names=NULL, check.rows=FALSE,
        check.names=FALSE, fix.empty.names=FALSE,
        stringsAsFactors=FALSE)
    argswap(`names<-`)(NULL, data.frame(
        `names<-`(
            list(...),
            if (is.null(row.names))
                    seq_along(list(...)) else row.names),
        row.names=row.names,
        check.rows=check.rows,
        check.names=check.names,
        fix.empty.names=fix.empty.names,
        stringsAsFactors=stringsAsFactors))
