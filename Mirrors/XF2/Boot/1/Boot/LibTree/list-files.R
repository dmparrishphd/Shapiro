# LOADER FOR THE NAMES OF THE ".dat" FILES IN THIS DIRECTORY
#
# USAGE:
#
#       datfiles <- source("path/list-files.R")[[1]](path)
#
function(path) {
    SAVE.WD <- getwd()
    setwd(path)
        M <- (function(FILENAMES=list.files(pattern="*dat$")){
            cbind(
                vapply(
                    strsplit(FILENAMES, "[.]"),
                    function(x) x[1], ""),
                FILENAMES) } )()
    setwd(SAVE.WD)
    paste0(paste0(path), M[,2][order(as.integer(M[,1] ) ) ] ) }
