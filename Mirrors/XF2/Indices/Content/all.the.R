#USAGE:
#    foo <- source(paste0(
#        "//ad.sfwmd.gov/dfsroot/data/worm/Files/COMP/",
#        "SOFTWARE/ShapiroXF2/Indices/Content/all.the.R"))[[1]]
(function() {

    `%//%` <- paste0

    PATH <- "//ad.sfwmd.gov/dfsroot/data/worm/Files/" %//%
            "COMP/SOFTWARE/ShapiroXF2/Nominal1/"

    LIST <- list.files(
        path=PATH, recursive=T, pattern="[.]R$")

    read <- function(filename)
            scan(PATH %//% filename, what="", sep="\n")

    CONTENT <- lapply(LIST, read)
    names(CONTENT) <- LIST

    CONTENT
})()