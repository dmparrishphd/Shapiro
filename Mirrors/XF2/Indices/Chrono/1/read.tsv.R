# read.tsv
#
# DESCRIPTION
#        READ A TSV FILE OF THE KIND STORED HERE (.).
# 
# USAGE EXAMPLE
#
#        DIR.HOME <- paste0("//ad.sfwmd.gov/dfsroot/data/worm/",
#            "Files/COMP/SOFTWARE/ShapiroXF2/Indices/Chrono/1/")
#        source(
#            paste0(DIR.HOME, "read.tsv.R"),
#            local=bootEnv)
#        TABLE <- bootEnv$read.tsv(paste0(DIR.HOME, "0000.tsv"))
read.tsv <- function(
    file,
    ...,
    sep="\t",
    quote="",
    col.names=c("DATE", "PATH"),
    na.strings=NULL,
    colClasses=c("Date", "character"),
    stringsAsFactors=FALSE)
        read.table(
            file,
            sep=sep,
            quote=quote,
            col.names=col.names,
            na.strings=na.strings,
            colClasses=colClasses,
            stringsAsFactors=stringsAsFactors)
