list_frame <- function(nrow, ncol=NULL, .names=NULL) #TAGS data.frame
        if (is.null %:|% ncol) {
            .list.frame.names(.names, nrow)
        } else {
            .list.frame.dim(c(nrow, ncol))
        }
