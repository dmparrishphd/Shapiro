data_frame <- function(
    ...,
    row.names=NULL,
    check.rows=FALSE,
    check.names=FALSE,
    fix.empty.names=FALSE,
    stringsAsFactors=FALSE)
        data.frame(
            list(...) %|% with_regular.names,
            row.names=row.names,
            check.rows=check.rows,
            check.names=check.names,
            fix.empty.names=fix.empty.names,
            stringsAsFactors=stringsAsFactors) %|% rename.all
