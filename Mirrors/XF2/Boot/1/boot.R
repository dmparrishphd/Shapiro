function(config.file) {
    TABLE <- read.csv(config.file, stringsAsFactors=F)
    ARGS <- `names<-`(as.list(TABLE[,2]), TABLE[,1])
    do.call(
        what=source(paste0(ARGS$dir.where.installed, "Boot/boot-function.R"))[[1]],
        args=ARGS, 
        envir=parent.env(parent.frame()))
}
