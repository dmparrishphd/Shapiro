# list.tsv
#
# DESCRIPTION
#
#       RETURN A TABLE (data.frame) OF TSV FILES STORED HERE (.).
#
#       THE RETURN IS SORTED ACCORDING TO THE CORRESPONDING NUMERIC INDEX.
#
# USAGE EXAMPLE
#
#        DIR.HOME <- paste0("//ad.sfwmd.gov/dfsroot/data/worm/",
#            "Files/COMP/SOFTWARE/ShapiroXF2/Indices/Chrono/1/")
#        source(
#            paste0(DIR.HOME, "list.tsv.R"),
#            local=bootEnv)
#        TABLE <- bootEnv$list.tsv()
list.tsv <- function() {
    PATH <- paste0("//ad.sfwmd.gov/dfsroot/data/worm/",
        "Files/COMP/SOFTWARE/ShapiroXF2/Indices/Chrono/1/")
    LIST <- list.files(PATH, pattern="[.]tsv$")
    DF1 <- data.frame(
        INDEX=c(-1L, 1L)[1 + (substr(LIST, 1, 1) == "0")] * as.integer(substr(LIST, 2, nchar(LIST) - 4)),
        FILE=LIST,
        PATH=paste0(PATH, LIST),
        stringsAsFactors=F)
    DF1[order(DF1$INDEX),] }
